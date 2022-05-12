package body ASTROMETRY is

-- Synopsis: GRIND - currently the only thing exported by POINT - calculates the positional astrometry from a command string,
-- optionally logging the results as text and JSON output on a socket channel. The internal representation of the data products is
-- stored in the SONA out record. The TUBE parameter - EAST or WEST - is just for calculating the dome azimuth, and currently is
-- called from telescopy using the actual current tube position. The commands are documented in the HL section. The protection
-- ensures file and socket operations will not be garbled with calls from the asynchronous server tasks, and that the private
-- variables holding the last and track commands stay consistent in the asynchronous communication environment.

protected body POINT is

procedure GRIND(QUERY : in string;
                SONA  : out ASTROMETRIC;
                TUBE  : in CARDINAL := EAST;
                LOG   : in Boolean  := TRUE;
                CHAN  : in GNAT.Sockets.Stream_Access := null) is

package Long_Float_IO is new Ada.Text_IO.Float_IO(Long_Float); use Long_Float_IO;
package DEGREE_IO     is new Ada.Text_IO.Float_IO(DEGREE);     use DEGREE_IO;
package Long_Float_Elfun is new Ada.Numerics.Generic_Elementary_Functions(Long_Float); use Long_Float_Elfun;

-- Commands and arguments
---------------------------------------

-- Parsing requires COMMAND to be a comma-separated list with no whitespace between items, with trailing spaces.
COMMAND     : String(1 .. STORE) := STORE * " ";
FROM_QUERY  : Boolean;   -- Signal to save the current QUERY command to protected private LAST_COMMAND.
JSON        : Boolean;
CID         : Integer;   -- CATALOG ID for BSC, FK5, IC, MESSIER, and NGC read from COMMAND.

-- INTIN, LFLTIN. Note for tests that raise PARSE_ERROR: these routines stop on comma or space. Hence if we're expecting more data,
-- then the stop character must be a comma. If the command has no more data, it's a space.
---------------------------------------
SPOINT  : Integer;  -- String position counter.

-- ZEROS, WEATHER files.
---------------------------------------
DUT            : Long_Float := 0.0;       -- UT1-UTC.
XP             : Double_Precision := 0.0; -- X Polar motion x-coordinate as given by IERS (RADIAN).
YP             : Double_Precision := 0.0; -- Y Polar motion y-coordinate as given by IERS (RADIAN).

ZEROS_FILE     : File_Type;               -- Handle to zeros.dat, which contains MOUNTINST, HA_OFFSET, DEC_OFFSET.
ZEROS_PATH     : string := "/var/point82/zeros.dat"; -- Place where zeros file is stored.
MOUNT_ON       : MOUNT      := BLANK;  -- The MOUNT that selects the coefficients for FLEX.
INST_ON        : INST       := NONE;   -- The INST name.
HA_OFFSET      : Long_Float := DEFAULT_HA_ZERO;  -- HA zero offset * COS(DEC) (ARCSECONDS). Defaults are set in specification file.
DEC_OFFSET     : Long_Float := DEFAULT_DEC_ZERO; -- DEC arcsecond OFFSET (ARCSECONDS).
ZEROS_ERROR    : Boolean    := FALSE;  -- Set to TRUE if zeros file can't be found.

-- McDonald Observatory uses inches of mercury, degrees F, and percent RH; but SLA_AOP uses millibars, degrees Kelvin, and
-- RH in [0,1]. Conversions are inline when reading weather data at start, and in procedure GET_ALTERNATE_DATE.
-- inline code.
WEATHER_FILE   : File_Type;            -- Handle to weather.dat.
WEATHER_PATH   : string := "/var/point82/weather.dat"; -- Place where weather data are stored.
WEATHER_ERROR  : Boolean    := FALSE;  -- Set to TRUE if weather file can't be found.
-- Range for sanity check and reasonable defaults if the weather file can't be read.
TDK            : Long_Float range 238.0 .. 322.0 := 273.15; -- Local ambient Temperature (K), default to 0C i.e. winter..
PMB            : Long_Float range 700.0 .. 900.0 := 789.0;  -- Local atmospheric Pressure (mB), default average Mt. Locke pressure.
RH             : Long_Float range 0.0   .. 1.0   := 0.2;    -- Local Relative Humidity (0.0 .. 1.0), default typical RH.

-- Calendar, SLA_CLDJ
---------------------------------------
UTC            : Time;
YEAR           : Year_Number;
MONTH          : Month_Number;
DAY            : Day_Number;
SEC            : Day_Duration; 
DJM            : Double_Precision;       -- Whole MJD.
OBJECT_EPOCH   : EPOCH_EQUINOX := J2000; -- Default EPOCH and EQUINOX of mean position is J2000.0. "B1950" is FK4 Besselian 1950.0
NAME           : String(1 .. 10) := 10* " "; -- FK5, BSC, MESSIER, NGC, IC catalog number or SOLARSYSTEM name for report.
STATUS         : Fortran_Integer;        -- SLA return code.

-- SLA_GMST
---------------------------------------
DTT    : Double_Precision := 37.0 + 32.184; -- Offset added to UTC to produce TT from SLA_DTT or IERS Bulletin A.
UT1    : Double_Precision;           -- UTC + DUT. SLA_AOP also needs UT1.
MJD_UT1: Double_Precision;           -- MJD and fractional UT1.
MJD_UTC: Double_Precision;           -- UTC MJD.
ST     : Double_Precision;           -- Sidereal Time.
LST    : Double_Precision;           -- Local Sidereal Time.

-- SLA_MAP
---------------------------------------
RM     : Double_Precision;           -- RA  Mean (RADIAN).
DM     : Double_Precision;           -- Dec Mean (RADIAN).
PR     : Double_Precision := 0.0;    -- Proper Motion RA dRA/dt, no factor of cos(DEC) (RADIAN/YEAR).
PD     : Double_Precision := 0.0;    -- Proper Motion DEC dDEC/dt, (RADIAN/YEAR).
PX     : Double_Precision := 0.0;    -- ParallaX in arcsec.
RV     : Double_Precision := 0.0;    -- Radial Velocity in km/s, positive if receding.
RA     : Double_Precision;           -- Returned geocentric apparent RA (RADIAN).
DA     : Double_Precision;           -- Returned geocentric apparent DEC (RADIAN).
RAT    : Double_Precision;           -- Returned geocentric apparent RA (RADIAN) for tracking.
DAT    : Double_Precision;           -- Returned geocentric apparent DEC (RADIAN) for tracking.
PR_IN  : Long_Float := 0.0;          -- Input RA proper motion in whatever units the catalog provides.
PR_OUT : Long_Float := 0.0;          -- Convert PR_IN to MILLIARCSECOND/YEAR*cos(DEC) for final report.
PD_IN  : Long_Float := 0.0;          -- Input DEC proper motion in whatever units the catalog provides.
PD_OUT : Long_Float := 0.0;          -- Convert PD_IN in MILLIARCSECOND/YEAR for final report.
MJD_TT : Double_Precision;           -- Terrestrial Time MJD.
DELTA_T: constant := 1.0;            -- Track rate interval of 1s seems accurate to 0.0001"/s at any practical ZD, even for moon.
-- RDPLAN
---------------------------------------
RD_NUM : Fortran_Integer range 1 .. 10; -- RDPLAN object number listed in type RD_NAME.
DIAM   : Double_Precision;              -- Returned equatorial diameter in radians.
TT     : Double_Precision;              -- Terrestrial Time, obtained as UTC + DTT, where DTT is obtained from SLA_DTT or the IERS.

-- REPORT
---------------------------------------
OBJECT  : ASTRONOMICAL := VOID;         -- Can be STAR (anything with a  fixed RA and DEC,) SOLARSYSTEM, or VOID.

-- GET_ALTERNATE_DATE can be called after parsing a catalog star, solarsystem object, or position to change the date and time used
-- by SET_TIME_VARS. It isn't used by LF(list FK5 stars), LB (list Yale Bright Stars), or non-positional commands.
-- After date, and time, it can also set pressure, temperature, and relative humidity. Beware that it's usually called as a
-- boolean parameter to procedure SET_TIME_VARS, and it changes YEAR,MONTH,DAY,SEC,PMB,TDK,RH so this is a kind of side-effect.
---------------------------------------
function GET_ALTERNATE_DATE return Boolean is -- macro parse COMMAND starting at SPOINT.
DONE : Boolean := FALSE;
begin
   YEAR := Year_Number(INTIN(COMMAND, SPOINT));
   if COMMAND(SPOINT + 1) = ',' then
      SPOINT := SPOINT + 2;
      MONTH := Month_Number(INTIN(COMMAND, SPOINT));
      if COMMAND(SPOINT + 1) = ',' then
         SPOINT := SPOINT + 2;
         DAY := Day_Number(INTIN(COMMAND, SPOINT));
         if COMMAND(SPOINT + 1) = ',' then
            SPOINT := SPOINT + 2;
            SEC := Day_Duration(3600.0 * (IN_SEXAGESIMAL(COMMAND, SPOINT, HOURS)));
            if COMMAND(SPOINT + 1) = ' ' then
               DONE := TRUE;  -- No more characters left to read.
            else -- read pressure, temperature, and relative humidity. These overwrite the values read from file.
               if COMMAND(SPOINT + 1) = ',' then 
                  SPOINT := SPOINT + 2;
                  PMB := LFLTIN(COMMAND, SPOINT) * 33.86389;  -- Convert to millibars
                  SONA.PMB   := PMB;
                  if COMMAND(SPOINT + 1) = ',' then
                     SPOINT := SPOINT + 2;
                     TDK := (LFLTIN(COMMAND, SPOINT) - 32.0) * 5.0 / 9.0 + 273.15;
                     SONA.TDK   := TDK;
                     if COMMAND(SPOINT + 1) = ',' then
                        SPOINT := SPOINT + 2;
                        RH := LFLTIN(COMMAND, SPOINT) / 100.0;
                        SONA.RH    := RH;
                        if COMMAND(SPOINT + 1) = ' ' then
                           DONE := TRUE;  -- No more characters left to read.
                        else
                           DONE := FALSE; -- Trailing comma or junk not allowed, and PARSE_ERROR will be raised.
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end if;
   if DONE = FALSE then
      raise PARSE_ERROR; 
   else
      return TRUE;
   end if;
end GET_ALTERNATE_DATE;

-- SET_TIME_VARS is called for all positional commands, with boolean USE_ALTERNATE_DATE set explicitly or set by GET_ALTERNATE_DATE.
---------------------------------------
procedure SET_TIME_VARS(USE_ALTERNATE_DATE : in Boolean)  is -- macro compute time variables for alternate date or current time.
begin
   if USE_ALTERNATE_DATE = FALSE then                        -- Get the current date, as it wasn't specified on the command line.
      UTC := Clock;                                          -- Get UTC. Make sure the the shell/system TZ is GMT.
      Split(UTC, YEAR, MONTH, DAY, SEC);
   end if;
   SONA.YEAR  := YEAR;
   SONA.MONTH := MONTH;
   SONA.DAY   := DAY; 
   SONA.SEC   := SEC;

   SLA_CLDJ(Fortran_Integer(YEAR), Fortran_Integer(MONTH), Fortran_Integer(DAY), DJM, STATUS); -- Whole MJD from calendar date.
   if STATUS > 0 then
      raise Constraint_Error;
   end if;

   -- DTT is set using IERS Bulletin A when the IERS data are read. SLA_DTT uses a table, which of course goes out of date.
   if USE_ALTERNATE_DATE then
      DTT := SLA_DTT(Double_Precision(DJM));                 -- N.B. SLA_DTT may be out-of-date.
   end if;
   TT  := Double_Precision(SEC) + DTT;                       -- Terrestrial Time is needed for SLA_RDPLAN and SLA_MAP.
   MJD_TT  := DJM + (TT /  86400.0);

   begin -- Get UT1-UTC, PX, PY for this date.
      DUT :=                 IERS_DATA(Integer(Long_Float(DJM) - IERS_DATA(0,IERS_MJD)), IERS_DUT);
      XP := Double_Precision(IERS_DATA(Integer(Long_Float(DJM) - IERS_DATA(0,IERS_MJD)),IERS_PMX));
      XP := XP / 3600.0 * PI / 180.0; -- Radians
      YP := Double_Precision(IERS_DATA(Integer(Long_Float(DJM) - IERS_DATA(0,IERS_MJD)),IERS_PMY));
      YP := YP / 3600.0 * PI / 180.0; -- Radians
   exception
      when others => null;                                   -- Don't let the loss of UT1 and polar motion shutdown the observatory.
   end;

   UT1 := Double_Precision(SEC) + Double_Precision(DUT);     -- SLA_GMST needs UT1 for LST.
   MJD_UT1 := DJM + (UT1 / 86400.0);
   MJD_UTC := DJM + Double_Precision(SEC / 86400.0);         -- See SLA_AOP notes on what to do during leap seconds.

   ST  := SLA_GMST(MJD_UT1);
   LST := SLA_DRANRM(ST + Double_Precision(OBS_LONGITUDE * PI / 180.0)); -- N.B. McD longitude west is negative.
   SONA.LST := LST;
end SET_TIME_VARS;

---------------------------------------
procedure WRITE_ZEROS(MOUNT_ON : MOUNT; INST_ON : INST; HA_OFFSET : Long_Float; DEC_OFFSET : Long_Float) is
-- Write the zeros file with MOUNTINST, HA_OFFSET, DEC_OFFSET
MOUNTINST : string(1 .. 9);
begin -- write the zeros file and log.
   create(File => ZEROS_FILE, Mode => Out_File, Name => ZEROS_PATH);
   MOUNTINST := MOUNT'Image(MOUNT_ON) & INST'Image(INST_ON);
   Put(MOUNTINST);
   Put(ZEROS_FILE, MOUNTINST);
   Put(HA_OFFSET, 5, 2, 0);
   Put(ZEROS_FILE, HA_OFFSET, 5, 2, 0);  -- Write to zeros file.
   Put(DEC_OFFSET, 5, 2, 0);
   Put(ZEROS_FILE, DEC_OFFSET, 5, 2, 0); -- Write to zeros file.
   close(ZEROS_FILE);
   exception
      when others => ZEROS_ERROR := TRUE;   -- Not fatal but not ideal.
      begin
         close(ZEROS_FILE);  -- Close the file so that WRITE_ZEROS can still open for write to update zeros.dat.
      exception
         when others => null;-- If it was already closed or couldn't be closed, drop the issue.
      end;
end;

-- End of declarations.
------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------

begin
   JSON := (if CHAN = null then FALSE else TRUE); -- When socket CHAN parameter specified, JSON output is turned on.

   Set(Name => "TZ", Value => "GMT"); -- Set the GMT time zone.

   COMMAND(1 .. QUERY'Length) := QUERY; -- QUERY spliced into space-padded command.

-- Always read files for weather and zeros.
---------------------------------------
   declare
   FLTVAL : Long_Float; -- Temporary for reading.
   begin -- Read range protected pressure (mB or inches Hg), temperature (K or F), and relative humidity ( 0..1 or 0..100 percent).
      open(File => WEATHER_FILE, Mode => In_File, Name => WEATHER_PATH);
      Get(WEATHER_FILE, FLTVAL);                   -- Get pressure in inches of Hg.
      PMB      := FLTVAL * 33.86389;               -- Convert to millibars
      SONA.PMB := PMB;
      Get(WEATHER_FILE, FLTVAL);                   -- Get temperature in Fahrenheit.
      TDK      := (FLTVAL - 32.0) * 5.0 / 9.0 + 273.15; -- Convert to Kelvin.
      SONA.TDK := TDK;
      Get(WEATHER_FILE, FLTVAL);                   -- Get relative humidity in percent
      RH       := FLTVAL / 100.0;                  -- Convert to [0,1];
      SONA.RH  := RH;
      close(WEATHER_FILE);
      SONA.WEATHER_ERROR := FALSE;
   exception                                       -- All problems end here: defaults will keep the show going, but not ideal.
      when others => WEATHER_ERROR      := TRUE;   -- Use default TDK, PMB, and RH.
                     SONA.WEATHER_ERROR := TRUE;
                     begin
                        close(WEATHER_FILE);       -- In case the exception happened before closing.
                     exception
                        when others => null;       -- Drop the issue.
                     end;
   end;

   declare
   MOUNTINST : string (1 .. 9) := "BLANKNONE"; -- 5 characters for mount and 4 characters for instrument.
   begin -- read the ZEROS file.
      open(File => ZEROS_FILE, Mode => In_File, Name => ZEROS_PATH);
      Get(ZEROS_FILE, MOUNTINST);                  -- 9 characters
      MOUNT_ON         := MOUNT'Value(MOUNTINST(1 .. 5));  -- Set MOUNT from the string.
      SONA.MOUNT_ON    := MOUNT_ON;                -- Store MOUNT_ON in SONA record.
      INST_ON          := INST'Value(MOUNTINST(6 .. 9));   -- Set INST from the string.
      SONA.INST_ON     := INST_ON;                 -- Store INST_ON in SONA record.
      Get(ZEROS_FILE, HA_OFFSET);                  -- HA*cos(DEC) in arcseconds.
      SONA.HA_OFFSET   := HA_OFFSET;
      Get(ZEROS_FILE, DEC_OFFSET);                 -- DEC arcseconds.
      SONA.DEC_OFFSET  := DEC_OFFSET;
      close(ZEROS_FILE);
      SONA.ZEROS_ERROR := FALSE;
   exception                                       -- All problems end here: defaults will keep the show going, but not ideal.
      when others => ZEROS_ERROR := TRUE;          -- Use default MOUNTINST, MOUNT_ON, HA_OFFSET, DEC_OFFSET.
                     SONA.MOUNT_ON := MOUNT_ON;    -- Store the default MOUNTINST in the return manifest
                     SONA.INST_ON  := INST_ON;     -- and same for INST_ON.
                     begin
                        close(ZEROS_FILE);         -- Close file so that WRITE_ZEROS can still open for write to update zeros.dat.
                     exception
                        when others => null;       -- If it was already closed or couldn't be closed, drop the issue.
                     end;
   end;
 
---------------------------------------
-- Preprocess commands that request the last command, the track command, set the track command, or rewrite PA to PT.
   begin
      case A_DISPATCH'Value(COMMAND(1 .. 2)) is
      -- Preprocess the QUERY to write the COMMAND.
      when LS =>
         FROM_QUERY := FALSE;            -- No need to cache the COMMAND into LAST_COMMAND.
         COMMAND := LAST_COMMAND;        -- Take command from the LAST_COMMAND.
      when GO =>                         -- Use the last object for the tracking object.
         FROM_QUERY := FALSE;            -- No need to cache the COMMAND into LAST_COMMAND.
         COMMAND := LAST_COMMAND;
         TRACK_COMMAND := COMMAND;       -- Save COMMAND for TR.
      when TR =>                         -- Take COMMAND from the track object set in GO.
         FROM_QUERY := FALSE;            -- Don't overwrite LAST_COMMAND  when using track command!
         COMMAND := TRACK_COMMAND;
      when PA =>                         -- Point Anonymous does not save LAST_COMMAND, so LS does not see it.
         FROM_QUERY  := FALSE;           -- This allows Track82 to compute EPHEMERIS Lagrange interpolation in the background.
         COMMAND(1 .. 2) := "PT";        -- Rewrite command to PT.
      when others =>                     -- Take COMMAND from CGI environment variable QUERY_STRING.
         FROM_QUERY := TRUE;             -- Allow positional commands to write COMMAND to LAST_COMMAND.
      end case;
   end;

---------------------------------------
-- Dispatch the command prepared in the preprocess stage above. The preprocess commands LS, GO, TR, and PA only execute null at
-- this stage, which could happen in pathological conditions, e.g. LAST_COMMAND contains LS.
   case A_DISPATCH'Value(COMMAND(1 .. 2)) is

   when BS => -- Yale Bright Star: BS,c,[YR,MO,DA,HH,MM,SS].
      OBJECT := STAR;
      if FROM_QUERY = TRUE then        
         LAST_COMMAND := COMMAND; -- Save the command into the protected private store.
      end if;
      if COMMAND(3) /= ',' then
         raise PARSE_ERROR;
      end if;
      SPOINT := 4; -- Start at character 4.
      CID := INTIN(COMMAND,SPOINT);
      if BSC_DATA(CID).ID /= -1 then -- Not an empty slot in the catalog.
         declare
         BSC_LABEL : String := Integer'Image(CID);
         begin
            NAME(1 .. 3) := "BSC";
            NAME(4 .. 3 + BSC_LABEL'Last) := BSC_LABEL;
         end;
         RM := Double_Precision(BSC_DATA(CID).RA);  -- RA
         DM := Double_Precision(BSC_DATA(CID).DEC); -- DEC
         -- BSC RA proper motion is arcseconds of RA per year with a factor of cos(DEC).
         PR_IN := BSC_DATA(CID).PR;                 -- arcsecond/yr * cos(DEC).
         -- Next conversion is arcsec/yr * cos(DEC) * PIrad/180deg * 1deg/3600arcsec / cos(DEC) = rad/yr.
         PR := Double_Precision(PR_IN * PI / 180.0 / 3600.0 / cos(RADIAN(DM)));  -- SLA wants RADIAN/year.
         PR_OUT := PR_IN * 1000.0;                                               -- Record in Hipparcos format mas/year*cos(DEC).
         -- BSC DEC proper motion is arcseconds per year.
         PD_IN := BSC_DATA(CID).PD;                 -- arcsecond/yr.
         PD := Double_Precision(PD_IN * PI / 180.0 / 3600.0);                    -- SLA wants RADIAN/year.
         PD_OUT := PD_IN * 1000.0;                                               -- Record in mas/yr.
         if COMMAND(SPOINT + 1) = ',' then -- get the alternate date.
            SPOINT := SPOINT + 2;
            SET_TIME_VARS(USE_ALTERNATE_DATE => GET_ALTERNATE_DATE);             -- GET_ALTERNATE_DATE returns TRUE or faults.
         else
            SET_TIME_VARS(USE_ALTERNATE_DATE => FALSE);                          -- Use current time.
         end if;
      else -- No entry for this HR. Start the JSON object, mark the object as VOID to prevent calculation and short circuit to coda.
         if JSON then
            string'Write(CHAN, "{");
         end if;
         if LOG then
            Put_Line("NO BSC ENTRY FOR THIS NUMBER.");
         end if;
         OBJECT := VOID;
      end if;

   when CI => -- Read 4 character INSTrument and write to zeros file.
      OBJECT := VOID;
      if COMMAND(3) /= ',' or COMMAND(8) /= ' ' then
         raise PARSE_ERROR;
      else
         -- Validates argument against type INST.
         WRITE_ZEROS(MOUNT_ON, INST'Value(COMMAND(4 .. 7)), HA_OFFSET, DEC_OFFSET);
         New_Line;
      end if;

   when CM => -- Read 5 character MOUNT and write to zeros file.
      OBJECT := VOID;
      if COMMAND(3) /= ',' or COMMAND(9) /= ' ' then
         raise PARSE_ERROR;
      else
         -- Validates argument against type INST.
         WRITE_ZEROS(MOUNT'Value(COMMAND(4 .. 8)), INST_ON, HA_OFFSET, DEC_OFFSET);
         New_Line;
      end if;

   when FK => -- Fifth Fundamental Catalog: FK,c,[YR,MO,DA,HH,MM,SS].
      OBJECT := STAR;
      if FROM_QUERY = TRUE then
         LAST_COMMAND := COMMAND; -- Save the command into the protected private store.
      end if;
      if COMMAND(3) /= ',' then
         raise PARSE_ERROR;
      end if;
      SPOINT := 4;
      CID := INTIN(COMMAND,SPOINT);
      if FK5_DATA(CID).ID /= -1 then     -- Not an empty slot in catalog.
         declare
         FK5_LABEL : String := Integer'Image(CID);
         begin
            NAME(1 .. 3) := "FK5";
            NAME(4 .. 3 + FK5_LABEL'Last) := FK5_LABEL;
         end;
         RM := Double_Precision(FK5_DATA(CID).RA);  -- RA
         DM := Double_Precision(FK5_DATA(CID).DEC); -- DEC
         -- FK5 RA proper motion is seconds of RA per century with no factor of cos(DEC).
         PR_IN := FK5_DATA(CID).PR;                 -- seconds of RA/ha.
         -- Next conversion is sec/ha * PIrad/180deg * 15arcsec/sec * ha/100yr * 1deg/3600arcsec = rad/yr.
         PR := Double_Precision(PR_IN * PI / 180.0 * 15.0 / 100.0 / 3600.0);   -- SLA wants RADIAN/year.
         PR_OUT := PR_IN * 15.0 * 1000.0 / 100.0 * cos(RADIAN(DM));            -- Record in Hipparcos format mas/year*cos(DEC).
         -- FK5 DEC proper motion is arcseconds per century.
         PD_IN := FK5_DATA(CID).PD;                 -- arcsecond/ha.
         PD := Double_Precision(PD_IN * PI / 180.0 / 100.0 / 3600.0);          -- SLA wants RADIAN/year.
         PD_OUT := PD_IN * 10.0;                                               -- Record in mas/yr.
         if COMMAND(SPOINT + 1) = ',' then -- get the alternate date.
            SPOINT := SPOINT + 2;
            SET_TIME_VARS(USE_ALTERNATE_DATE => GET_ALTERNATE_DATE);           -- GET_ALTERNATE_DATE returns TRUE or faults.
         else
            SET_TIME_VARS(USE_ALTERNATE_DATE => FALSE);                        -- Use current time.
         end if;
      else -- No entry for this FK. Start the JSON object, mark the object as VOID to prevent calculation and short circuit to coda.
         if JSON then
            String'Write(CHAN, "{");
         end if;
         if LOG then
            Put_Line("NO FK5 ENTRY FOR THIS NUMBER.");
         end if;
         OBJECT := VOID;
      end if;
   when GO => -- Rewritten to use LAST_COMMAND and copy to TRACK_COMMAND.
      null;

   when HL => -- give Help. No JSON for this command.
      OBJECT := VOID;
      Put_Line("PT|PA,HH,MM,SS,[+-]DD,MM,SS[,J2000|B1950[,PR,PD[,YR,MO,DA,HH,MM,SS[,mB,F,RH]]]]");
      New_Line;
      Put_Line("  Point at RA=HH,MM,SS DEC=[+-]DD,MM,SS.");
      Put_Line("  J2000 is Julian EQUINOX=EPOCH=2000.0 (default).");
      Put_Line("  B1950 is Besselian EQUINOX=EPOCH=1950.0.");
      Put_Line("  PR is RA proper motion in MAS/YEAR with cos(DEC).");
      Put_Line("  PD is DEC proper motion in MAS/YEAR.");
      Put_Line("  YR,MO,DA,HH,MM,SS is Year,Month,Day,Hour,Minute,Second.");
      Put_Line("  mB,F,RH is mB pressure, F temperature, and %RH.");
      Put_Line("  If not specified use current system date and time.");
      Put_Line("  Date and time sets EPHEMERIS instead of POSITION.");
      Put_Line("  PA does not write LAST_COMMAND, for anonymous pointing.");
      New_Line;
      Put_Line("---------------------------------------------------------------'");
      New_Line;
      Put_Line("SL,c|RD_NAME");
      Put_Line("SL,c|RD_NAME,YR,MO,DA,HH,MM,SS[,mB,F,RH]");
      New_Line;
      Put_Line("  c is Solar System 0 .. 9, Sun=0, Moon=3. RD_NAME is the object");
      Put_Line("  name MERCURY, VENUS, MOON, MARS, JUPITER, SATURN, URANUS, NEPTUNE,");
      Put_Line("  PLUTO, SUN, case-insensitive");
      Put_Line("---------------------------------------------------------------'");
      New_Line;
      Put_Line("BS|FK|MS|NG|ID,c");
      Put_Line("BS|FK|MS|NG|ID,c,YR,MO,DA,HH,MM,SS[,mB,F,RH]");
      New_Line;
      Put_Line("  c is an index number into the following catalogs:");
      Put_Line("  BS is Yale Bright Star (Vizier), FK is FK5 (Vizier),");
      Put_Line("  MS is Messier (KPNO), NG is New General Catalog (Steinike),");
      Put_Line("  ID is Index Catalog (Steinicke).");
      New_Line;
      Put_Line("---------------------------------------------------------------'");
      New_Line;
      Put_Line("LF|LB");
      Put_Line("LF|LB,ZD");
      Put_Line("LF|LB,ZD,MAG");
      New_Line;
      Put_Line("  LF is list Fifth Fundamental Catalog, LB is list Yale Bright Star,");
      Put_Line("  within 30 degrees Zenith Distance and < Magnitude 4. ZD is alternate");
      Put_Line("  Zenith Distance, MAG is alternate magnitude.");
      New_Line;
      Put_Line("---------------------------------------------------------------'");
      New_Line;
      Put_Line("CM");
      Put_Line("CI");
      New_Line;
      Put_Line("CM chooses mount, CI chooses instrument.");
      New_Line;
      Put_Line("---------------------------------------------------------------'");
      New_Line;
      Put_Line("ZP");
      Put_Line("ZA,PR,PD");
      Put_Line("ZR,PR,PD");
      Put_Line("ZD");
      New_Line;
      Put_Line("  ZA set absolutely.");
      Put_Line("  ZD set default zeros.");
      Put_Line("  ZP reports the current MOUNTINST, PR, PD.");
      Put_Line("  ZR set relative to existing zeros.");
      New_Line;
      Put_Line("---------------------------------------------------------------'");
      Put_Line("Other Commands");
      Put_Line("  LS is report the last positional command.");
      Put_Line("  GO is copy the last positional command to track position and report.");
      Put_Line("  TR is report the track position.");
      New_Line;
      Put_Line("  SLA by P.T. Wallace. By John W. Kuehne, McDonald Observatory.");

   when ID => -- Index Catalog: ID,c,[YR,MO,DA,HH,MM,SS]. Index Catalog.
      OBJECT := STAR;
      if FROM_QUERY = TRUE then
         LAST_COMMAND := COMMAND; -- Save the command into the protected private store.
      end if;
      if COMMAND(3) /= ',' then
         raise PARSE_ERROR;
      end if;
      SPOINT := 4; -- Start at character 4.
      CID := INTIN(COMMAND,SPOINT);
      declare
      IC_LABEL : String := Integer'Image(CID);
      begin
         NAME(1 .. 2) := "IC";
         NAME(3 .. 2 + IC_LABEL'Last) := IC_LABEL;
      end;
      RM := Double_Precision(IC_DATA(CID).RA);  -- RA
      DM := Double_Precision(IC_DATA(CID).DEC); -- DEC
      PR_IN := 0.0; PR := 0.0; PD_IN := 0.0; PD := 0.0;            -- No proper motion data in IC.
      if COMMAND(SPOINT + 1) = ',' then              -- get the alternate date.
         SPOINT := SPOINT + 2;
         SET_TIME_VARS(USE_ALTERNATE_DATE => GET_ALTERNATE_DATE);  -- GET_ALTERNATE_DATE returns TRUE or exception faults.
      else
         SET_TIME_VARS(USE_ALTERNATE_DATE => FALSE);               -- Use current time.
      end if;

   when LB => -- List Yale Bright Stars for zeroing the telescope.
      OBJECT := VOID; -- No further calculations needed.
      declare
      ZENITH_LIMIT : DEGREE := 30.0;
      ZD           : DEGREE;
      AZ           : DEGREE;
      VMAG_LIMIT   : Long_Float := 4.0;
      NEED_COMMA   : Boolean := FALSE;  -- for JSON.
      begin
        SET_TIME_VARS(USE_ALTERNATE_DATE => FALSE); -- Local time.
        if COMMAND(3) = ',' then
          SPOINT := 4; -- Start at character 4.
          ZENITH_LIMIT := DEGREE(LFLTIN(COMMAND,SPOINT));
          if COMMAND(SPOINT + 1) = ',' then
             SPOINT := SPOINT + 2;
             VMAG_LIMIT := Long_Float(LFLTIN(COMMAND,SPOINT));
          end if;
          if COMMAND(SPOINT + 1) /= ' ' then -- there's trailing comma or junk.
             raise PARSE_ERROR;
          end if;
        end if;
        if JSON then
           String'Write(CHAN, "{ ""LB"": [" & NL); -- Start the JSON array of objects.
        end if;
        if LOG then
           Put_Line("  HR     ZD       AZ      VMAG     RA              DEC ");
        end if;
        for C in BSC_DATA'Range loop
           if BSC_DATA(C).ID /= -1 then -- Not an empty slot in the catalog
              -- Compute apparent place.
              RM := Double_Precision(BSC_DATA(C).RA);
              DM := Double_Precision(BSC_DATA(C).DEC);
              PR_IN := BSC_DATA(C).PR;                                                 -- arcsec/year*cos(DEC).
              PR := Double_Precision(PR_IN * PI / 180.0 / 3600.0 / cos(RADIAN(DM)));   -- SLA wants RADIAN/year.
              PD_IN := BSC_DATA(C).PD;                                                 -- arcsec/year.
              PD := Double_Precision(PD_IN * PI / 180.0 / 3600.0);                     -- SLA wants RADIAN/year.
              SLA_MAP(RM, DM, PR, PD, PX, RV, 2000.0, MJD_TT, RA, DA);                 -- APPARENT.
              ZD := ZENITH_DISTANCE(RADIAN(SLA_DRANGE(LST - RA)), RADIAN(DA));         -- APPARENT zenith distance.
              if (ZD <= ZENITH_LIMIT)  and (BSC_DATA(C).MAG <= VMAG_LIMIT) then
                 AZ := AZIMUTH(RADIAN(SLA_DRANGE(LST - RA)), RADIAN(DA));
                 if JSON then
                    if NEED_COMMA then
                       String'Write(CHAN, "," & NL);
                    end if;
                    String'Write(CHAN, "{""HR"": " & Integer'Image(C) & ", " &
                    """ZD"": " & DEGREE'Image(ZD) & ", " & 
                    """AZ"": " & DEGREE'Image(AZ) & ", " & 
                    """VMAG"": " & Long_Float'Image(BSC_DATA(C).MAG) & ", " & 
                    """RA"": "  & OUT_SEXAGESIMAL(BSC_DATA(C).RA, HOURS, UNSIGNED, JSON) & ", " & 
                    """DEC"": " & OUT_SEXAGESIMAL(BSC_DATA(C).DEC, DEGREES, SIGNED, JSON) & "}");
                    NEED_COMMA := TRUE;
                 end if;
                 if LOG then
                    Put(C, 5); Put(ZD, 6, 1, 0); Put(AZ, 7, 1, 0); Put(BSC_DATA(C).MAG, 5, 2, 0);
                    Put("    " & OUT_SEXAGESIMAL(BSC_DATA(C).RA, HOURS, UNSIGNED, JSON=>FALSE));
                    Put("    " & OUT_SEXAGESIMAL(BSC_DATA(C).DEC, DEGREES, SIGNED, JSON=>FALSE));
                    New_Line;
                 end if;
              end if;
           end if;
        end loop;
        if JSON then
           String'Write(CHAN, "]," & NL);
        end if;
      end;

   when LF => -- List Fifth Fundamental Catalog stars for zeroing the telescope.
      OBJECT := VOID; -- No further calculations needed.
      declare
      ZENITH_LIMIT : DEGREE := 30.0;
      ZD           : DEGREE;
      AZ           : DEGREE;
      VMAG_LIMIT   : Long_Float := 4.0;
      NEED_COMMA   : Boolean := FALSE;  -- for JSON.
      begin
        SET_TIME_VARS(USE_ALTERNATE_DATE => FALSE); -- Local time.
        if COMMAND(3) = ',' then
          SPOINT := 4; -- Start at character 4.
          ZENITH_LIMIT := DEGREE(LFLTIN(COMMAND, SPOINT));
          if COMMAND(SPOINT + 1) = ',' then
             SPOINT := SPOINT + 2;
             VMAG_LIMIT := Long_Float(LFLTIN(COMMAND,SPOINT));
          end if;
          if COMMAND(SPOINT + 1) /= ' ' then -- there's a trailing comma or junk.
             raise PARSE_ERROR;
          end if;
        end if;
        if JSON then
           String'Write(CHAN, "{ ""LF"": [" & NL); -- Start the JSON array of objects.
        end if;
        if LOG then
           Put_Line("  FK5    ZD       AZ      VMAG     RA              DEC ");
        end if;
        for C in FK5_DATA'Range loop
           if FK5_DATA(C).ID /= -1 then -- Not an empty slot in the catalog
              -- Compute apparent place.
              RM := Double_Precision(FK5_DATA(C).RA);
              DM := Double_Precision(FK5_DATA(C).DEC);
              PR_IN := FK5_DATA(C).PR;                                           -- Get RA proper motion for FK5, RAsec/century.
              PR := Double_Precision(PR_IN * PI / 180.0 * 15.0 / 100.0 / 3600.0);-- SLA wants RADIAN/year
              PD_IN := FK5_DATA(C).PD;                                           -- Get DEC proper motion for FK5, arcsec/century.
              PD := Double_Precision(PD_IN * PI / 180.0 / 100.0 / 3600.0);       -- SLA wants RADIAN/year.
              SLA_MAP(RM, DM, PR, PD, PX, RV, 2000.0, MJD_TT, RA, DA);           -- APPARENT.
              ZD := ZENITH_DISTANCE(RADIAN(SLA_DRANGE(LST - RA)), RADIAN(DA));   -- APPARENT zenith distance.
              if (ZD <= ZENITH_LIMIT)  and (FK5_DATA(C).MAG <= VMAG_LIMIT) then
                 AZ := AZIMUTH(RADIAN(SLA_DRANGE(LST - RA)), RADIAN(DA));
                 if JSON then
                    if NEED_COMMA then
                       String'Write(CHAN, "," & NL);
                    end if;
                    String'Write(CHAN, "{""HR"": " & Integer'Image(C) & ", " &
                    """ZD"": " & DEGREE'Image(ZD) & ", " &
                    """AZ"": " & DEGREE'Image(AZ) & ", " &
                    """VMAG"": " & Long_Float'Image(FK5_DATA(C).MAG) & ", " &
                    """RA"": "  & OUT_SEXAGESIMAL(FK5_DATA(C).RA, HOURS, UNSIGNED, JSON) & ", " &
                    """DEC"": " & OUT_SEXAGESIMAL(FK5_DATA(C).DEC, DEGREES, SIGNED, JSON) & "}");
                    NEED_COMMA := TRUE;
                 end if;
                 if LOG then
                    Put(C, 5); Put(ZD, 6, 1, 0); Put(AZ, 7, 1, 0); Put(FK5_DATA(C).MAG, 5, 2, 0);
                    Put("    " & OUT_SEXAGESIMAL(FK5_DATA(C).RA, HOURS, UNSIGNED, JSON=>FALSE));
                    Put("    " & OUT_SEXAGESIMAL(FK5_DATA(C).DEC, DEGREES, SIGNED, JSON=>FALSE));
                    New_Line;
                 end if;
              end if;
           end if;
        end loop;
        if JSON then
           String'Write(CHAN, "]," & NL);
        end if;
      end;

   when LS => -- Rewritten to read from LAST_COMMAND.
      null;

   when MS => -- Messier: MS,c,[YR,MO,DA,HH,MM,SS].
      OBJECT := STAR;
      if FROM_QUERY = TRUE then
         LAST_COMMAND := COMMAND; -- Save the command into the protected private store.
      end if;
      if COMMAND(3) /= ',' then
         raise PARSE_ERROR;
      end if;
      SPOINT := 4; -- Start at character 4.
      CID := INTIN(COMMAND,SPOINT);
      declare
      M_LABEL : String := Integer'Image(CID);
      begin
         NAME(1) := 'M';
         NAME(2 .. 1 + M_LABEL'Last) := M_LABEL;
      end;
      RM := Double_Precision(MESSIER_DATA(CID).RA);  -- RA
      DM := Double_Precision(MESSIER_DATA(CID).DEC); -- DEC
      PR_IN := 0.0; PR := 0.0; PD_IN := 0.0; PD := 0.0;           -- No proper motion data in Messier.
      if COMMAND(SPOINT + 1) = ',' then             -- Get the alternate date.
         SPOINT := SPOINT + 2;
         SET_TIME_VARS(USE_ALTERNATE_DATE => GET_ALTERNATE_DATE); -- GET_ALTERNATE_DATE returns TRUE or exception faults.
      else
         SET_TIME_VARS(USE_ALTERNATE_DATE => FALSE);              -- Use current time.
      end if;

   when NG => -- New General Catalog: NG,c,[YR,MO,DA,HH,MM,SS].
      OBJECT := STAR;
      if FROM_QUERY = TRUE then
         LAST_COMMAND := COMMAND; -- Save the command into the protected private store.
      end if;
      if COMMAND(3) /= ',' then
         raise PARSE_ERROR;
      end if;
      SPOINT := 4; -- Start at character 4.
      CID := INTIN(COMMAND,SPOINT);
      declare
      NGC_LABEL : String := Integer'Image(CID);
      begin
         NAME(1 .. 3) := "NGC";
         NAME(4 .. 3 + NGC_LABEL'Last) := NGC_LABEL;
      end;
      RM := Double_Precision(NGC_DATA(CID).RA);  -- RA
      DM := Double_Precision(NGC_DATA(CID).DEC); -- DEC
      PR_IN := 0.0; PR := 0.0; PD_IN := 0.0; PD := 0.0;           -- No proper motion data in NGC.
      if COMMAND(SPOINT + 1) = ',' then             -- get the alternate date.
         SPOINT := SPOINT + 2;
         SET_TIME_VARS(USE_ALTERNATE_DATE => GET_ALTERNATE_DATE); -- GET_ALTERNATE_DATE returns TRUE or exception faults.
      else
         SET_TIME_VARS(USE_ALTERNATE_DATE => FALSE);              -- Use current time.
      end if;

   when PA => -- Point Anonymous rewritten to PT, but does not save to LAST_COMMAND.
      null;

   when PT => -- Position: PT,HH,MM,SS,[+-]DD,MM,SS[,J2000|B1950[,PR,PD[,YR,MO,DA,HH,MM,SS]]]
      -- N.B. when optional date is provided, NAME is "EPHEMERIS" instead of "POSITION". This is used by EPHEMERIS82, TRACK82,
      -- and GUIDE82, for example for setting the ephemeral track rate. When command is PA, it gets rewritten to this PT but
      -- with FROM_QUERY := FALSE to prevent clobbering LAST_COMMAND.
      OBJECT := STAR;
      if FROM_QUERY = TRUE then
         LAST_COMMAND := COMMAND; -- Save the command into the protected private store.
      end if;
      if COMMAND(3) /= ',' then
         raise PARSE_ERROR;
      end if;
      SPOINT := 4;
      RM := Double_Precision(IN_SEXAGESIMAL(COMMAND, SPOINT, HOURS));  -- Read RA or die.
      RM := SLA_DRANRM(RM / 12.0 * PI); -- Express Radians from hours, ranged 0-2PI.
      if COMMAND(SPOINT + 1) /= ',' then
         raise PARSE_ERROR;
      end if;
      SPOINT := SPOINT + 2;
      DM := Double_Precision(IN_SEXAGESIMAL(COMMAND, SPOINT, DEGREES, SIGNED) / 180.0 * PI); -- Read DEC or die.
      if COMMAND(SPOINT + 1) = ',' then       -- pick up OBJECT_EPOCH.
         SPOINT := SPOINT + 2;
         OBJECT_EPOCH := EPOCH_EQUINOX'Value(COMMAND(SPOINT .. SPOINT + 4));
         if COMMAND(SPOINT + 5) = ',' then    -- pick up proper motion PR, PD.
            SPOINT := SPOINT + 6;                           -- Skip past next comma.
            PR_IN  := LFLTIN(COMMAND, SPOINT);              -- Read RA proper motion.
            PR     := Double_Precision(PR_IN / 1000.0 / 3600.0 * PI / 180.0 / cos(RADIAN(DM))); -- mas/year*cos(DEC) to RADIANS/yr.
            PR_OUT := PR_IN;                                -- Input and output formats match.
            SPOINT := SPOINT + 2;
            PD_IN  := LFLTIN(COMMAND, SPOINT);              -- Read DEC proper motion.
            PD     := Double_Precision(PD_IN / 1000.0 / 3600.0 * PI / 180.0);
            PD_OUT := PD_IN; -- Input and output formats match.
            if COMMAND(SPOINT + 1) = ',' then -- pick up alternate date.
               SPOINT := SPOINT + 2;
               SET_TIME_VARS(USE_ALTERNATE_DATE => GET_ALTERNATE_DATE);  -- GET_ALTERNATE_DATE returns TRUE or exception faults.
               NAME   := "EPHEMERIS ";
            else
               SET_TIME_VARS(USE_ALTERNATE_DATE => FALSE); -- Use current time when command line ends at PR,PD.
               NAME   := "POSITION  ";
            end if;
         else
            if COMMAND(SPOINT + 5) /= ' ' then
               raise PARSE_ERROR;                       -- There's trailing garbage after EPOCH_EQUINOX.
            end if;
            SET_TIME_VARS(USE_ALTERNATE_DATE => FALSE); -- Use current time when command line ends at J2000|B1950.
         end if;
      else
         SET_TIME_VARS(USE_ALTERNATE_DATE => FALSE); -- Use current time when command line ends at DD,MM,SS.
      end if;

   when SL => -- Solarsystem: SL,RD_NUMBER|RD_NAME,[YR,MO,DA,HH,MM,SS].
      OBJECT := SOLARSYSTEM;
      if FROM_QUERY = TRUE then
         LAST_COMMAND := COMMAND; -- Save the command into the protected private store.
      end if;
      if COMMAND(3) /= ',' then
         raise PARSE_ERROR;
      end if;
      SPOINT := 4; -- Start at character 4.
      -- Similar to INTIN but handles the exceptional case where the string is an RD_NAME, returning the SLA_RDPLAN number.
      declare
      LAST_I  : Integer := 0;
      START_I : Integer := SPOINT;
      begin
         for C in SPOINT .. COMMAND'Last loop
            exit when COMMAND(C) = ',' or COMMAND(C) = ' ';
            LAST_I := C;
         end loop;
         SPOINT := LAST_I;
         begin
            -- Let command line specify Sun with 0 or 10.
            RD_NUM := (if Integer'Value(COMMAND(START_I..LAST_I)) = 0 then 10 else Fortran_Integer'Value(COMMAND(START_I..LAST_I)));
         exception -- this could also Constraint_Error, which is tough luck.
            when Constraint_Error => RD_NUM := 1 + RD_NAME'Pos(RD_NAME'Value(COMMAND(START_I .. LAST_I)));
         end;
      end;
      if COMMAND(SPOINT + 1) = ',' then                           -- get the alternate date.
         SPOINT := SPOINT + 2;
         SET_TIME_VARS(USE_ALTERNATE_DATE => GET_ALTERNATE_DATE); -- GET_ALTERNATE_DATE returns TRUE or exception faults.
      else
         SET_TIME_VARS(USE_ALTERNATE_DATE => FALSE);              -- Use current time.
      end if;

   when TR => -- Rewritten to use LAST_COMMAND
      null;

   when VD => -- Calling VoiD will always get mount and weather information in SONA.
      OBJECT := VOID;

   when ZA => -- Update zeros absolute.
      OBJECT := VOID;
      SPOINT := 4;
      declare
      HA_NEW  : Long_Float;
      DEC_NEW : Long_Float;
      begin
         HA_NEW := LFLTIN(COMMAND, SPOINT);
         if COMMAND(SPOINT + 1) /= ',' then
            raise PARSE_ERROR;
         else
            SPOINT  := SPOINT + 2;
            DEC_NEW := LFLTIN(COMMAND, SPOINT);
            if COMMAND(SPOINT + 1) /= ' ' then -- don't allow any trailing junk.
               raise PARSE_ERROR;
            else
               WRITE_ZEROS(MOUNT_ON, INST_ON, HA_NEW, DEC_NEW); -- N.B. MOUNT_ON and INST_ON read from zeros.dat at start.
               New_Line;
            end if;
         end if;
      end;

   when ZD => -- Update zeros default.
   OBJECT := VOID;
   if COMMAND(3) /= ' ' then
      raise PARSE_ERROR;
   else
      WRITE_ZEROS(MOUNT_ON, INST_ON, DEFAULT_HA_ZERO, DEFAULT_DEC_ZERO); -- N.B. MOUNT_ON and INST_ON read from zeros.dat at start.
      New_Line;
   end if;

   when ZP =>
      if COMMAND(3) = ' ' then -- Print the MOUNTINST and zeros.
      OBJECT := VOID;
         if JSON then -- start the JSON object to report the current MOUNTINST and zeros, and being VOID short circuit to coda.
            String'Write(CHAN, "{""MOUNTINST"": " & """" & MOUNT'Image(MOUNT_ON) & INST'Image(INST_ON)  & """," & NL &
            """HA_OFFSET"": "  & Long_Float'Image(HA_OFFSET) & "," & NL &
            """DEC_OFFSET"": " & Long_Float'Image(DEC_OFFSET) & "," & NL);
         end if;
         if LOG then
            Put(MOUNT'Image(MOUNT_ON) & INST'Image(INST_ON)); Put(HA_OFFSET, 6, 2, 0);  Put(DEC_OFFSET, 6, 1, 0); New_Line;
         end if;
      else -- the command started ZP, but has junk after it instead of a space.
         raise PARSE_ERROR;
      end if;

   when ZR => -- Update zeros relative.
      OBJECT := VOID;
      SPOINT := 4;
      declare
      HA_NEW  : Long_Float;
      DEC_NEW : Long_Float;
      begin
         HA_NEW := LFLTIN(COMMAND, SPOINT);
         if COMMAND(SPOINT + 1) /= ',' then
            raise PARSE_ERROR;
         else
            SPOINT  := SPOINT + 2;
            DEC_NEW := LFLTIN(COMMAND, SPOINT);
            if COMMAND(SPOINT + 1) /= ' ' then -- don't allow any trailing junk.
               raise PARSE_ERROR;
            else
               -- N.B. MOUNT_ON, INST_ON, HA_OFFSET,and DEC_OFFSET read from zeros.dat at start.
               WRITE_ZEROS(MOUNT_ON, INST_ON, HA_NEW + HA_OFFSET, DEC_NEW + DEC_OFFSET);
               New_Line;
            end if;
         end if;
      end;

   end case;

   -- Everything is now setup to perform the positional SLA operations on STAR and SOLARSYSTEM. VOID jumps to coda.
   ---------------------------------------
   SONA.OBJECT := OBJECT;

   case OBJECT is

   when VOID =>
      goto END_PROGRAM; -- Commands for VOID objects do not compute a position.

   when STAR =>
      case OBJECT_EPOCH is
      when B1950 => -- Convert Besselian EQUINOX=EPOCH=1950 to J2000 before MAP.
         declare
         --  SLA_FK45, FK4 B1950 to FK5 J2000.
         ---------------------------------------
         R2000  : Double_Precision;           -- B1950.0 RA
         D2000  : Double_Precision;           -- B1950.0 DEC
         DR2000 : Double_Precision;           -- B1950.0 RA proper motion  (RADIAN/YEAR) dRA/dt
         DD2000 : Double_Precision;           -- B1950.0 DEC proper motion (RADIAN/YEAR)
         P2000  : Double_Precision;           -- B1950.0 parallax arcsec
         V2000  : Double_Precision;           -- B1950.0 radial velocity km/s, + moving away.
         begin
            SLA_FK425(RM, DM, PR, PD, PX, RV, R2000, D2000, DR2000, DD2000, P2000, V2000); -- Convert B1950 to J2000.
            SLA_MAP(R2000, D2000, DR2000, DD2000, P2000, V2000, 2000.0, MJD_TT, RA, DA);   -- Geocentric apparent.
            -- Advance apparent place DELTA_T to estimate track rate.
            SLA_MAP(R2000, D2000, DR2000, DD2000, P2000, V2000, 2000.0, MJD_TT + DELTA_T / 86400.0, RAT, DAT);
         end;
      when J2000 =>
         SLA_MAP(RM, DM, PR, PD, PX, RV, 2000.0, MJD_TT, RA, DA);                       -- Mean to Apparent.
         -- Advance apparent place DELTA_T to estimate track rate.
         SLA_MAP(RM, DM, PR, PD, PX, RV, 2000.0, MJD_TT + DELTA_T / 86400.0, RAT, DAT);
      end case;

      -- Store results for return.
      SONA.NAME         := NAME;
      SONA.RM           := RM;
      SONA.DM           := DM;
      SONA.OBJECT_EPOCH := OBJECT_EPOCH;
      SONA.PR_OUT       := PR_OUT;
      SONA.PD_OUT       := PD_OUT;
      SONA.RA           := RA;
      SONA.DA           := DA;
      SONA.MJD_UTC      := MJD_UTC;

      if JSON then
         String'Write(CHAN, "{" &
         """INST_ON"": """              & INST'Image(INST_ON) & """," & NL &
         """YEAR"": "                   & Integer'Image(Integer(YEAR))  & "," & NL &
         """MONTH"": "                  & Integer'Image(Integer(MONTH)) & "," & NL &
         """DAY"": "                    & Integer'Image(Integer(DAY))   & "," & NL &
         """UTC_HOUR"":"                & Long_Float'Image(Long_Float(SEC) / 3600.0) & ","    & NL &
                                     -- N.B. SEX format below needs RADIANS, but SEC is in seconds.
         """UTC_HOUR_SEXAGESIMAL"":"    & OUT_SEXAGESIMAL(PI / 12.0 * Long_Float(SEC) / 3600.0, HOURS, UNSIGNED, JSON) & "," & NL &
         """LST_HOUR"": "               & Long_Float'Image(Long_Float(LST) * 12.0 / PI) & "," & NL &
         """LST_HOUR_SEXAGESIMAL"":"    & OUT_SEXAGESIMAL(Long_Float(LST), HOURS, UNSIGNED, JSON) & "," & NL & -- LST in radians.
         """PRESSURE_MB"": "            & Long_Float'Image(PMB) & "," & NL &
         """TEMPERATURE_C"": "          & Long_Float'Image(TDK - 273.15) & "," & NL &
         """HUMIDITY_PERCENT"": "       & Long_Float'Image(RH * 100.0) & ","   & NL &
         """NAME"": """                 & NAME & """," & NL &
         """MEAN_RA_HOUR"": "           & Long_Float'Image(RADIAN(RM) * 12.0  / PI) & "," & NL &
         """MEAN_RA_HOUR_SEXAGESIMAL"":"& OUT_SEXAGESIMAL(Long_Float(RADIAN(RM)), HOURS, UNSIGNED, JSON) & "," & NL &
         """MEAN_DEC_DEGREE"": "        & Long_Float'Image(RADIAN(DM) * 180.0 / PI) & "," & NL &
         """MEAN_DEC_DEGREE_SEXAGESIMAL"":"    & OUT_SEXAGESIMAL(Long_Float(RADIAN(DM)), DEGREES, SIGNED, JSON) & "," & NL &
         """OBJECT_EPOCH"": """         & EPOCH_EQUINOX'Image(OBJECT_EPOCH)  & """," & NL &
         """PROPER_RA_MILLIARCSEC"": "  & Long_Float'Image(PR_OUT) & "," & NL &
         """PROPER_DEC_MILLIARCSEC"": " & Long_Float'Image(PD_OUT) & "," & NL &
         """APPARENT_RA_HOUR"": "       & Long_Float'Image(RADIAN(RA) * 12.0 / PI) & ","       & NL &
         """APPARENT_RA_HOUR_SEXAGESIMAL"":"   & OUT_SEXAGESIMAL(Long_Float(RADIAN(RA)), HOURS, UNSIGNED, JSON) & "," & NL &
         """APPARENT_DEC_DEGREE"": "    & Long_Float'Image(RADIAN(DA) * 180.0 / PI) & ","      & NL &
         """APPARENT_DEC_DEGREE_SEXAGESIMAL"":" & OUT_SEXAGESIMAL(Long_Float(RADIAN(DA)), DEGREES, SIGNED, JSON) & "," & NL &
         """APPARENT_EPOCH"": "         & Long_Float'Image(Long_Float(SLA_EPJ(MJD_UTC))) & "," & NL);
      end if;
      if LOG then
         Put_Line("YEAR  M  D  " & "UTC" & "           LST           P(mB)     T(C)   RH(%) |");
         Put(Integer(YEAR), 4);
         Put(Integer(MONTH), 3);
         Put(Integer(DAY), 3);
         Put(" "  & OUT_SEXAGESIMAL(PI / 12.0 * Long_Float(SEC) / 3600.0, HOURS, UNSIGNED, JSON=>FALSE));
         Put("  " & OUT_SEXAGESIMAL(Long_Float(LST), HOURS, UNSIGNED, JSON=>FALSE));
         Put(Long_Float(PMB), 6, 1, 0);
         Put(Long_Float(TDK - 273.15), 7, 1, 0);
         Put(Integer(RH * 100.0), 5);
         Put("    |" & NL);
         Put("                                                               |" & NL);
         Put(NAME & "  RA            DEC           EPOCH     P_RA   P_DEC |" & NL);
         Put("MEAN PLACE " &  OUT_SEXAGESIMAL(RADIAN(RM), HOURS, UNSIGNED, JSON=>FALSE) & " ");
         Put(OUT_SEXAGESIMAL(RADIAN(DM), DEGREES, SIGNED, JSON=>FALSE) & "    ");
         Put(EPOCH_EQUINOX'Image(OBJECT_EPOCH));
         Put(Integer(PR_OUT), 9);
         Put(Integer(PD_OUT), 8);
         Put(" |" & NL);
         Put("  APPARENT " & OUT_SEXAGESIMAL(RADIAN(RA), HOURS, UNSIGNED, JSON=>FALSE) & " ");
         Put(OUT_SEXAGESIMAL(RADIAN(DA), DEGREES, SIGNED, JSON=>FALSE));
         Put("    J");
         Put(Long_Float(SLA_EPJ(MJD_UTC)), 4, 3, 0);
         Put("              |" & NL);
      end if;

   when SOLARSYSTEM =>
      SLA_RDPLAN(MJD_TT, RD_NUM, Double_Precision(OBS_LONGITUDE * PI / 180.0),
                 Double_Precision(OBS_LATITUDE * PI / 180.0), RA, DA, DIAM); 
      -- Advance apparent place DELTA_T to estimate track rate.
      SLA_RDPLAN(MJD_TT + DELTA_T / 86400.0, RD_NUM, Double_Precision(OBS_LONGITUDE * PI / 180.0),
                    Double_Precision(OBS_LATITUDE * PI / 180.0), RAT, DAT, DIAM);

      NAME(1 .. RD_NAME'Image(RD_NAME'Val(Integer(RD_NUM)-1))'Last) := RD_NAME'Image(RD_NAME'Val(Integer(RD_NUM)-1));

   -- SOLARSYSTEM objects don't have a Mean Position, only Apparent, and hence no OBJECT_EPOCH. They also have a diameter.
      SONA.NAME         := NAME;
      SONA.RA           := RA;
      SONA.DA           := DA;
      SONA.MJD_UTC      := MJD_UTC;
      SONA.DIAM         := DIAM;

      if JSON then
         String'Write(CHAN, "{" &
         """INST_ON"": """              & INST'Image(INST_ON) & """," & NL &
         """YEAR"": "                   & Integer'Image(Integer(YEAR))  & "," & NL &
         """MONTH"": "                  & Integer'Image(Integer(MONTH)) & "," & NL &
         """DAY"": "                    & Integer'Image(Integer(DAY))   & "," & NL &
         """UTC_HOUR"":"                & Long_Float'Image(Long_Float(SEC) / 3600.0) & ","    & NL &
                                     -- N.B. SEX format below needs RADIANS, but SEC is in seconds.
         """UTC_HOUR_SEXAGESIMAL"":"    & OUT_SEXAGESIMAL(PI / 12.0 * Long_Float(SEC) / 3600.0, HOURS, UNSIGNED, JSON) & "," & NL &
         """LST_HOUR"": "               & Long_Float'Image(Long_Float(LST) * 12.0 / PI) & "," & NL &
         """LST_HOUR_SEXAGESIMAL"":"    & OUT_SEXAGESIMAL(Long_Float(LST), HOURS, UNSIGNED, JSON) & "," & NL &
         """PRESSURE_MB"": "            & Long_Float'Image(PMB) & "," & NL &
         """TEMPERATURE_C"": "          & Long_Float'Image(TDK - 273.15) & "," & NL &
         """HUMIDITY_PERCENT"": "       & Long_Float'Image(RH * 100.0) & ","   & NL &
         """NAME"": """                 & NAME & """," & NL &
         """APPARENT_RA_HOUR"": "       & Long_Float'Image(RADIAN(RA) * 12.0 / PI) & ","  & NL &
         """APPARENT_RA_HOUR_SEXAGESIMAL"":"    & OUT_SEXAGESIMAL(Long_Float(RADIAN(RA)), HOURS, UNSIGNED, JSON) & "," & NL &
         """APPARENT_DEC_DEGREE"": "    & Long_Float'Image(RADIAN(DA) * 180.0 / PI) & "," & NL &
         """APPARENT_DEC_DEGREE_SEXAGESIMAL"":" & OUT_SEXAGESIMAL(Long_Float(RADIAN(DA)), DEGREES, SIGNED, JSON) & "," & NL &
         """APPARENT_EPOCH"": "         & Long_Float'Image(Long_Float(SLA_EPJ(MJD_UTC))) & ","    & NL &
         """SUBTENDS_DEGREE"": "        & Long_Float'Image(RADIAN(DIAM) * 180.0 / PI) & ","      & NL);
      end if;
      if LOG then
         Put_Line("YEAR  M  D  " & "UTC" & "           LST           P(mB)     T(C)   RH(%) |");
         Put(Integer(YEAR), 4);
         Put(Integer(MONTH), 3);
         Put(Integer(DAY), 3);
         Put(" "  & OUT_SEXAGESIMAL(PI / 12.0 * Long_Float(SEC) / 3600.0, HOURS, UNSIGNED, JSON=>FALSE));
         Put("  " & OUT_SEXAGESIMAL(Long_Float(LST), HOURS, UNSIGNED, JSON=>FALSE));
         Put(Long_Float(PMB), 6, 1, 0);
         Put(Long_Float(TDK - 273.15), 7, 1, 0);
         Put(Integer(RH * 100.0), 5);
         Put("    |" & NL);
         Put(NAME & "  RA            DEC           EPOCH     SUBTENDS     |" & NL);
         Put("  APPARENT " & OUT_SEXAGESIMAL(RADIAN(RA), HOURS, UNSIGNED, JSON=>FALSE) & " ");
         Put(OUT_SEXAGESIMAL(RADIAN(DA), DEGREES, SIGNED, JSON=>FALSE));
         Put("    J");
         Put(Long_Float(SLA_EPJ(MJD_UTC)), 4, 3, 0);
         Put(" " & OUT_SEXAGESIMAL(RADIAN(DIAM), DEGREES, UNSIGNED, JSON=>FALSE)(6 .. 12));
         Put("      |" & NL);
      end if;
   end case;

   declare
   -- SLA_AOP, Apparent Observed Place, produces OBSERVED position of STAR or SOLARSYSTEM.
   ---------------------------------------
   WL     : Double_Precision := 0.55;   -- Effective wavelength (micron).
   TLR    : Double_Precision := 0.0065; -- Tropospheric lapse rate (K/metre).
   AOB    : Double_Precision;           -- Azimuth OBserved (RADIAN: N=0,E=90).
   ZOB    : Double_Precision;           -- Zenith distance OBserved (RADIAN).
   HOB    : Double_Precision;           -- Hour angle OBserved (RADIAN).
   DOB    : Double_Precision;           -- Declination OBserved (RADIAN).
   ROB    : Double_Precision;           -- Right ascension OBserved (RADIAN).
   AOBT   : Double_Precision;           -- Azimuth for tracking.
   ZOBT   : Double_Precision;           -- Zenith distance for tracking.
   HOBT   : Double_Precision;           -- Hour angle OBserved for Tracking (RADIAN).
   DOBT   : Double_Precision;           -- Declination OBserved for Tracking (RADIAN).
   ROBT   : Double_Precision;           -- Right ascension for tracking.

   begin -- FINAL REPORT.

   -- Get apparent to observed place to get the AOB, ZOB, HOB, DOB, and ROB we need.
   SLA_AOP(RA, DA,
           MJD_UTC,
           Double_Precision(DUT),
           Double_Precision(OBS_LONGITUDE * PI / 180.0),
           Double_Precision(OBS_LATITUDE * PI / 180.0),
           Double_Precision(OBS_ELEVATION),
           XP, YP, Double_Precision(TDK), Double_Precision(PMB), Double_Precision(RH), WL, TLR, AOB, ZOB, HOB, DOB, ROB);

   if SLA_DRANRM(ZOB) > PI / 2.0 then -- Abandon further calculation. Report telescopy HA, ZD, and AZ and terminate.
      -- The main error in the following approximation is neglecting polar motion, which is too small to affect ZD or AZ, and
      -- technically it's the APPARENT, not the OBSERVED position, but keep it the same key as the successful tags.

      SONA.VISIBLE := FALSE;

      if JSON then
         String'Write(CHAN, """VISIBLE"": ""FALSE""," & NL &
         """OBSERVED_HA_HOUR"": "    & Long_Float'Image(RADIAN(SLA_DRANGE(LST - RA)) * 12.0 / PI) & "," & NL &
         """OBSERVED_HA_HOUR_SEXAGESIMAL"":" & OUT_SEXAGESIMAL (RADIAN(SLA_DRANGE(LST - RA)), HOURS, SIGNED, JSON) & "," & NL &
         """OBSERVED_ZD_DEGREE"": "  & DEGREE'Image(ZENITH_DISTANCE(RADIAN(SLA_DRANGE(LST - RA)), RADIAN(DA))) & "," & NL &
         """OBSERVED_AZ_DEGREE"": "  & DEGREE'Image(AZIMUTH(RADIAN(SLA_DRANGE(LST - RA)), RADIAN(DA))) & "," & NL);
      end if;
      if LOG then
         Put_Line("                                                               |");
         Put_Line("  OBJECT IS BELOW HORIZON.                                     |");
         Put_Line("                                                               |");
         Put_Line("                                                               |");
         Put_Line("            HA            ZD     AZ                            |");
         Put(" TELESCOPY ");
         Put(OUT_SEXAGESIMAL(RADIAN(SLA_DRANGE(LST - RA)), HOURS, SIGNED, JSON=>FALSE));
         Put(ZENITH_DISTANCE(RADIAN(SLA_DRANGE(LST - RA)), RADIAN(DA)), 5, 2, 0); 
         Put(AZIMUTH(RADIAN(SLA_DRANGE(LST - RA)), RADIAN(DA)), 4, 2, 0);
         Put_Line("                         |");
      end if;
   else -- Report observed and flexed positions and track rates and successfully terminate with telescopy.

      -- Tracking calculation advances Apparent Observed Place DELTA_T. Only need HOBT and DOBT.
      SLA_AOP(RAT, DAT,
              MJD_UTC + DELTA_T / 86400.0,
              Double_Precision(DUT),
              Double_Precision(OBS_LONGITUDE * PI / 180.0),
              Double_Precision(OBS_LATITUDE  * PI / 180.0),
              Double_Precision(OBS_ELEVATION),
              XP, YP, Double_Precision(TDK), Double_Precision(PMB), Double_Precision(RH), WL, TLR, AOBT, ZOBT, HOBT, DOBT, ROBT);

      SONA.VISIBLE := TRUE;
      SONA.ROB     := ROB;
      SONA.DOB     := DOB;
      SONA.THOB    := Long_Float(RADIAN(SLA_DRANGE(HOBT - HOB)) * 180.0 / PI * 3600.0 / DELTA_T);
      SONA.TDOB    := Long_Float(RADIAN(SLA_DRANGE(DOBT - DOB)) * 180.0 / PI * 3600.0 / DELTA_T);

      if JSON then
         String'Write(CHAN, """VISIBLE"": ""TRUE""," & NL &
         """OBSERVED_RA_HOUR"": "                  & Long_Float'Image(RADIAN(ROB) * 12.0 / PI)  & "," & NL &
         """OBSERVED_RA_HOUR_SEXAGESIMAL"":"       & OUT_SEXAGESIMAL(Long_Float(RADIAN(ROB)), HOURS, UNSIGNED, JSON) & "," & NL &
         """OBSERVED_DEC_DEGREE"": "               & Long_Float'Image(RADIAN(DOB) * 180.0 / PI) & "," & NL &
         """OBSERVED_DEC_DEGREE_SEXAGESIMAL"":"    & OUT_SEXAGESIMAL(Long_Float(RADIAN(DOB)), DEGREES, SIGNED, JSON) & "," & NL &
         """OBSERVED_TRACK_HA_ARCSEC_PER_SEC"": "  & Long_Float'Image(RADIAN(SLA_DRANGE(HOBT - HOB))
                                                                  * 180.0 / PI * 3600.0 / DELTA_T)  & "," & NL &
         """OBSERVED_TRACK_DEC_ARCSEC_PER_SEC"": " & Long_Float'Image(RADIAN(SLA_DRANGE(DOBT - DOB))
                                                                  * 180.0 / PI * 3600.0 / DELTA_T)  & "," & NL);
      end if;
      if LOG then
         Put_Line("                                        dHA/dt    dDEC/dt      |");
         Put("  OBSERVED ");
         Put(OUT_SEXAGESIMAL(RADIAN(ROB), HOURS, UNSIGNED, JSON=>FALSE) & " ");
         Put(OUT_SEXAGESIMAL(RADIAN(DOB), DEGREES, SIGNED, JSON=>FALSE));
         Put(RADIAN(SLA_DRANGE(HOBT - HOB)) * 180.0 / PI * 3600.0 / DELTA_T, 6, 4, 0);
         Put(RADIAN(SLA_DRANGE(DOBT - DOB)) * 180.0 / PI * 3600.0 / DELTA_T, 4, 4, 0);
         Put_Line("       |");
      end if;

      declare
      -- FLEX
      ---------------------------------------
      FHOB       : RADIAN;     -- Flexed Hour angle OBserved, not corrected for zeros offset.
      FDOB       : RADIAN;     -- Flexed Declination OBserved, not corrected for zeros offset.
      FLEX_RA    : RADIAN;     -- Flexed RA, corrected for zeros offset.
      FLEX_DEC   : RADIAN;     -- Flexed DEC, corrected for zeros offset.
      FLEX_HA    : RADIAN;     -- Flexed HA, corrected for zeros offset.
      FHOBT      : RADIAN;     -- Flexed Hour angle OBserved for Tracking.
      FDOBT      : RADIAN;     -- Flexed Declination OBserved for Tracking.
      begin
         FLEX(RADIAN(HOB), RADIAN(DOB), FHOB, FDOB, MOUNT_ON);
         FLEX(RADIAN(HOBT), RADIAN(DOBT), FHOBT, FDOBT, MOUNT_ON);
         -- Taking into account cos(declination) by division means zeroing data multiplies HA by cos(declination).
         FLEX_RA  := RADIAN(ROB) - (FHOB - RADIAN(HOB)) + HA_OFFSET / 3600.0 * PI / 180.0 / cos(RADIAN(DOB));
         FLEX_RA  := RADIAN(SLA_DRANRM(Double_Precision(FLEX_RA)));    -- Normalize angle into range 0 .. 2PI.
         FLEX_DEC := RADIAN(FDOB) + DEC_OFFSET / 3600.0 * PI / 180.0;  -- DEC flexure.
         FLEX_DEC := RADIAN(SLA_DRANGE(Double_Precision(FLEX_DEC)));   -- Normalize angle into range -PI .. PI.
         FLEX_HA  := RADIAN(FHOB) - HA_OFFSET / 3600.0 * PI / 180.0 / cos(RADIAN(DOB));
         FLEX_HA  := RADIAN(SLA_DRANGE(Double_Precision(FLEX_HA)));    -- Normalize HA into range -PI .. PI.


         SONA.FLEX      := TRUE;
         SONA.FLEX_RA   := FLEX_RA;
         SONA.FLEX_DEC  := FLEX_DEC;
         SONA.TFLEX_HA  := Long_Float(RADIAN(SLA_DRANGE(Double_Precision(FHOBT - FHOB)))
                                                    * 180.0 / PI * 3600.0 / DELTA_T);
         SONA.TFLEX_DEC := Long_Float(RADIAN(SLA_DRANGE(Double_Precision(FDOBT - FDOB)))
                                                    * 180.0 / PI * 3600.0 / DELTA_T);
         SONA.FLEX_HA   := FLEX_HA;

         if JSON then
            String'Write(CHAN, """FLEX"": ""TRUE""," & NL &
            """MOUNT_ON"": """ & MOUNT'Image(MOUNT_ON) & """," & NL &
            """FLEX_RA_HOUR"": "                 & Long_Float'Image(RADIAN(FLEX_RA) * 12.0 / PI) & "," & NL &
            """FLEX_RA_HOUR_SEXAGESIMAL"":"      & OUT_SEXAGESIMAL(Long_Float(RADIAN(FLEX_RA)), HOURS, UNSIGNED, JSON)  & "," & NL &
            """FLEX_DEC_DEGREE"": "              & Long_Float'Image(RADIAN(FLEX_DEC) * 180.0 / PI) & "," & NL &
            """FLEX_DEC_DEGREE_SEXAGESIMAL"":"   & OUT_SEXAGESIMAL(Long_Float(RADIAN(FLEX_DEC)), DEGREES, SIGNED, JSON) & "," & NL &
            """FLEX_TRACK_HA_ARCSEC_PER_SEC"": " & Long_Float'Image(RADIAN(SLA_DRANGE(Double_Precision(FHOBT - FHOB)))
                                                                     * 180.0 / PI * 3600.0 / DELTA_T) & "," & NL &
            """FLEX_TRACK_DEC_ARCSEC_PER_SEC"": "& Long_Float'Image(RADIAN(SLA_DRANGE(Double_Precision(FDOBT - FDOB)))
                                                                     * 180.0 / PI * 3600.0 / DELTA_T) & "," & NL &
            """FLEX_HA_HOUR"": "                 & Long_Float'Image(RADIAN(FLEX_HA) * 12.0 / PI) & "," & NL &
            """FLEX_HA_HOUR_SEXAGESIMAL"":"      & OUT_SEXAGESIMAL(Long_Float(RADIAN(FLEX_HA)), HOURS, SIGNED, JSON)    & "," & NL);
         end if;
         if LOG then
            Put("    +");
            Put(MOUNT'Image(MOUNT_ON));
            Put(" " & OUT_SEXAGESIMAL(RADIAN(FLEX_RA), HOURS, UNSIGNED, JSON=>FALSE));
            Put(" "); Put(OUT_SEXAGESIMAL(RADIAN(FLEX_DEC), DEGREES,SIGNED, JSON=>FALSE));
            Put(RADIAN(SLA_DRANGE(Double_Precision(FHOBT - FHOB))) * 180.0 / PI * 3600.0 / DELTA_T, 6, 4, 0);
            Put(RADIAN(SLA_DRANGE(Double_Precision(FDOBT - FDOB))) * 180.0 / PI * 3600.0 / DELTA_T, 4, 4, 0);
            Put_Line("       |");
         end if;
      exception
         when others => -- The flexure failed.
            SONA.FLEX := FALSE;
            if JSON then
               String'Write(CHAN, """FLEX"": ""FALSE""," & NL);
            end if;
            if LOG then
               Put_Line("  POSITION COULD NOT BE FLEXED.                                  |");
            end if;
      end;

      -- Telescopy of visible objects is based on OBSERVED position.
      if LOG then
         Put_Line("                                                               |");
         Put_Line("            HA            ZD     AZ     DOME AZ   AIRMASS      |");
      end if;

      SONA.HOB  := HOB;
      SONA.ZOB  := ZOB;
      SONA.AOB  := AOB;
      SONA.DOME := DOME_AZIMUTH(HOUR_ANGLE     => RADIAN(HOB),
                                DECLINATION    => RADIAN(DOB),
                                TUBE_OFFSET_EW => (if TUBE = EAST then TUBE_OFFSET_82 else -1.0*TUBE_OFFSET_82),
                                TUBE_OFFSET_NS => 0.0,
                                DOME_RADIUS    => DOME_RADIUS_82,
                                DOME_OFFSET    => DOME_OFFSET_82);
      SONA.AIRMASS := SLA_AIRMAS(ZOB);

      if JSON then
         String'Write(CHAN, """OBSERVED_HA_HOUR"": " & Long_Float'Image(RADIAN(SLA_DRANGE(HOB)) * 12.0 / PI) & "," & NL &
         """OBSERVED_ZD_DEGREE"": " & DEGREE'Image(DEGREE(SLA_DRANRM(ZOB) * 180.0 / PI)) & "," & NL &
         """OBSERVED_AZ_DEGREE"": " & DEGREE'Image(DEGREE(SLA_DRANRM(AOB) * 180.0 / PI)) & "," & NL &
         """DOME_DEGREE"": "        & DEGREE'Image(SONA.DOME) & "," & NL &
         """AIRMASS"": "            & Double_Precision'Image(SLA_AIRMAS(ZOB)) & "," & NL);
      end if;
      if LOG then
         Put(" TELESCOPY ");
         Put(OUT_SEXAGESIMAL(RADIAN(SLA_DRANGE(HOB)),HOURS, SIGNED, JSON=>FALSE));
         Put(DEGREE(SLA_DRANRM(ZOB) * 180.0 / PI), 5, 2, 0);
         Put(DEGREE(SLA_DRANRM(AOB) * 180.0 / PI), 4, 2, 0);
         Put(SONA.DOME, 5, 2, 0);
         Put(Long_Float(SLA_AIRMAS(ZOB)), 5, 3, 0);
         Put_Line("        |");
      end if;
   end if;

   end; -- FINAL REPORT.

<<END_PROGRAM>>

<<CODA>>
-- Normal termination.
   if JSON then
      String'Write(CHAN, """OBJECT"": """ & ASTRONOMICAL'Image(OBJECT)& """," &
      """IERS_ERROR"": """  & Boolean'Image(IERS_ERROR) & ""","  &
      """ZEROS_ERROR"": """ & Boolean'Image(ZEROS_ERROR) & """," & 
      """WEATHER_ERROR"": """ & Boolean'Image(WEATHER_ERROR) & """" &
      "}" & NL & AK); -- Tell client the JSON message is done.
   end if;
   if LOG then
      Put_Line("---------------------------------------------------------------'");
      if IERS_ERROR    then Put_Line("WARNING: IERS DATA NOT FOUND.");                  end if;
      if ZEROS_ERROR   then Put_Line("WARNING: MOUNT MODEL AND ZEROS DATA NOT FOUND."); end if;
      if WEATHER_ERROR then Put_Line("WARNING: WEATHER DATA NOT FOUND");                end if;
   end if;

   -- Let caller handle any unexpected exception that makes it to this point.

end GRIND;
end POINT;
end ASTROMETRY;
