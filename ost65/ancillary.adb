package body ANCILLARY is

------------------------------------------------------------------------------------------------------------------------------------
function DOME_AZIMUTH (HOUR_ANGLE     : RADIAN;
                       DECLINATION    : RADIAN;
                       TUBE_OFFSET_EW : Long_Float;
                       TUBE_OFFSET_NS : Long_Float;
                       DOME_RADIUS    : Long_Float;
                       DOME_OFFSET    : VECTOR) return DEGREE  is  -- REQUIRED DOME AZIMUTH

-- My novel off-center solution for L is given explicitly by solving for and factoring the quadratic in MAXIMA.
-- See "Fiber-optic gyro location of dome azimuth" SPIE Journal of Astronomical Telescopes and Systems, September 2016.
-- This formula is the more general case for both E-W and N-S offsets, using the same methodology.

A, H, D, M, G, L, R, P1, P2, P3, X, Y, Z : Long_Float;-- Prepare short variable names for calculation.

begin
   H :=  (HOUR_ANGLE);                        -- Hour angle in radians.
   D :=  (PI / 2.0 - (DECLINATION));          -- Codeclination in radians.
   A :=  -(90.0 - Long_Float(OBS_LATITUDE)) * PI / 180.0; -- colatitude of back-rotation in radians.
   -- All offsets are relative to the telescope center point, i.e. the intersection of the HA and DEC axes.
   M :=  TUBE_OFFSET_EW;                      -- Offset E-W. East tube is positive.
   G :=  TUBE_OFFSET_NS;                      -- Offset N-S. South finder postion is positive.
   X :=  DOME_OFFSET(1);                      -- Dome center, SOUTH is positive.
   Y :=  DOME_OFFSET(2);                      -- Dome center, EAST is positive.
   Z :=  DOME_OFFSET(3);                      -- Dome center, UP is positive.
   R :=  DOME_RADIUS;                         -- Dome radius.

   L := sqrt(R**2-M**2+((2.0*cos(a)*x-2.0*sin(a)*z)*sin(H)+2.0*y*cos(H))*M+((cos(a)**2-1.0)*z**2+2.0*cos(a)*sin(a)*x*z+y**2-cos(a)**2*x**2)*sin(D)**2*sin(H)**2+((2.0*sin(a)*y*z-2.0*cos(a)*x*y)*sin(D)**2*cos(H)-2.0*y*cos(D)*G+(-2.0*cos(a)*y*z-2.0*sin(a)*x*y)*cos(D)*sin(D))*sin(H)+((2.0*cos(a)*x-2.0*sin(a)*z)*cos(D)*G+(-2.0*cos(a)*sin(a)*z**2+(4.0*cos(a)**2-2.0)*x*z+2.0*cos(a)*sin(a)*x**2)*cos(D)*sin(D))*cos(H)-G**2+(-2.0*cos(a)*z-2.0*sin(a)*x)*sin(D)*G+((1.0-2.0*cos(a)**2)*z**2-4.0*cos(a)*sin(a)*x*z+(2.0*cos(a)**2-1.0)*x**2)*sin(D)**2+(cos(a)**2-1.0)*z**2+2.0*cos(a)*sin(a)*x*z-y**2-cos(a)**2*x**2)-y*sin(D)*sin(H)+(cos(a)*x-sin(a)*z)*sin(D)*cos(H)+(cos(a)*z+sin(a)*x)*cos(D);

   P1 := cos(a) * (sin(H) * M + cos(H) * (sin(D) * L + cos(D) * G)) + sin(a) * (cos(D) * L - sin(D) * G);
   P2 := cos(H) * M - sin(H) * (sin(D) * L + cos(D) * G);
   -- Not used here, but could be used for dome accouterments.
   P3 := cos(a) * (cos(D) * L - sin(D) * G) - sin(a) * (sin(H) * M + cos(H) * (sin(D) * L + cos(D) * G));

   -- Dome azimuth in degrees
   return DEGREE(180.0 - 180.0 / PI * arctan(P2 - y, P1 - x));
end DOME_AZIMUTH;


function OUT_SEXAGESIMAL(X : RADIAN;
                         FORMAT: UNIT;
                         CUT   : FORM := UNSIGNED;
                         JSON  : Boolean := FALSE) return string is
-- the sexagesimal string image of X (RADIANS) in units of HOURS or DEGREES, and cut at 0 or PI. HOURS has hundredths second
-- accuracy, i.e. HH MM SS.ss, and DEGRESS is tenths, i.e. [+-]DD MM SS.s or DDD MM SS.s. When JSON is requested, the output
-- consists of four comma separated values: the sign, either as 1 or -1, and then the positive 3 sexagesimal digits. This helps
-- prevent the infamous negative-zero bug, where the leading negative sign is not properly applied.

HOUR      : string (integer range 1..2);
DEGR      : string (integer range 1..3);
MINUTE    : string (integer range 1..2);
SECOND    : string (integer range 1..2);
HSECOND   : string (integer range 1..2);
DSECOND   : string (integer range 1..1);
SIGN      : string (integer range 1..1);
SEPARATOR : String := (if JSON then ", " else ":"); -- Use : for legacy format.
T         : integer; -- representation of X, converted to hours or degrees, cut, rounded, and positive.

HR_HR, HD_HD, MN_HS, MM_MM, SC_HS, SC_SC, HS_HS : integer;

begin
   if X < 0.0 then
      SIGN := "-"; 
   else
      SIGN := "+";
   end if;

   case FORMAT is
   when HOURS => -- HH MM SS.SS (UNSIGNED) or [+-]HH MM SS.SS (SIGNED).
      if CUT = SIGNED then -- Ranged, hundredths second, integer, positive. N.B. Ada integer rounds.
         T := abs(integer(SLA_DRANGE(Double_Precision(X)) * 24.0 * 60.0 * 60.0 * 100.0 / 2.0 / PI)); -- Cut at PI.
      else
         T := abs(integer(SLA_DRANRM(Double_Precision(X)) * 24.0 * 60.0 * 60.0 * 100.0 / 2.0 / PI)); -- Cut at 0.
      end if;

      T := (if T = 24 * 60 * 60 * 100 then 0 else T); -- Make sure rounding of 23 59 59.999 wraps to 0.
      HR_HR := T / (60 * 60 * 100);       -- Whole hours in units of hours.
      MN_HS := T - HR_HR * 60 * 60 * 100; -- Remaining minutes in units of hundredths seconds.
      MM_MM := MN_HS / (60 * 100);        -- Whole minutes in units of minutes.
      SC_HS := MN_HS - MM_MM * 60 * 100;  -- Remaining seconds in units of hundredths seconds.
      SC_SC := SC_HS / 100;               -- Whole seconds in units of seconds.
      HS_HS := SC_HS - SC_SC * 100;       -- Remaining hundredths seconds in units of hundredths seconds.

      -- Add leading zeros if necessary and extract the string.
      if HR_HR < 10 then
         HOUR    := (if JSON then " " else "0") & integer'Image(HR_HR)(2 .. 2);
      else
         HOUR    := integer'Image(HR_HR)(2 .. 3);
      end if;
      if MM_MM < 10 then 
         MINUTE  := (if JSON then " " else "0") & integer'Image(MM_MM)(2 .. 2);
      else
         MINUTE  := integer'Image(MM_MM)(2 .. 3);
      end if;
      if SC_SC < 10 then
         SECOND  := (if JSON then " " else "0") & integer'Image(SC_SC)(2 .. 2);
      else
         SECOND := integer'Image(SC_SC)(2 .. 3);
      end if;
      if HS_HS < 10 then
         HSECOND :=  "0" & integer'Image(HS_HS)(2 .. 2);
      else
         HSECOND := integer'Image(HS_HS)(2 .. 3);
      end if;
      

      if CUT = UNSIGNED then
         if JSON then -- give sign separately from sexagesimal to allow decimal parsing without the infamous -0 bug.
            return("[1" & SEPARATOR & HOUR & SEPARATOR & MINUTE & SEPARATOR & SECOND & "." & HSECOND & "]");
         else
            return(" "        &       HOUR & SEPARATOR & MINUTE & SEPARATOR & SECOND & "." & HSECOND);
         end if;
      else
         if JSON then -- give sign separately from sexagesimal to allow decimal parsing without the infamous -0 bug.
            return("[" & (if SIGN = "+" then " 1" else "-1")
                        & SEPARATOR & HOUR & SEPARATOR & MINUTE & SEPARATOR & SECOND & "." & HSECOND & "]");
         else
            return(SIGN       &       HOUR & SEPARATOR & MINUTE & SEPARATOR & SECOND & "." & HSECOND);
         end if;
      end if;

   when DEGREES => -- DDD MM SS.S (UNSIGNED) or [+-]DDD MM SS.S (SIGNED).

      if CUT = SIGNED then -- Ranged, tenths second, integer, positive.
         T := abs(integer(SLA_DRANGE(Double_Precision(X)) * 360.0 * 60.0 * 60.0 * 10.0 / 2.0 / PI)); -- Cut at PI.
      else
         T := abs(integer(SLA_DRANRM(Double_Precision(X)) * 360.0 * 60.0 * 60.0 * 10.0 / 2.0 / PI)); -- Cut at 0.
      end if;

      T := (if T = 360 * 60 * 60 * 10 then 0 else T); -- Make sure rounding of 359 59 59.999 to 360 is wrapped.
      HD_HD := T / (60 * 60 * 10);       -- Whole degrees in units of degrees.
      MN_HS := T - HD_HD * 60 * 60 * 10; -- Remaining minutes in units of tenths seconds.
      MM_MM := MN_HS / (60 * 10);        -- Whole minutes in units of minutes.
      SC_HS := MN_HS - MM_MM * 60 * 10;  -- Remaining seconds in units of tenths seconds.
      SC_SC := SC_HS / 10;               -- Whole seconds in units of seconds.
      HS_HS := SC_HS - SC_SC * 10;       -- Remaining tenths seconds in units of tenths seconds.

      -- Add leading zeros if necessary and extract the string.
      if HD_HD > 99 then -- 3 digits.
         DEGR := integer'Image(HD_HD)(2 .. 4);
      elsif  HD_HD > 9 then -- 2 digits
         DEGR := (if JSON then " " else "0") & integer'Image(HD_HD)(2 .. 3);
      else
         DEGR   :=  (if JSON then "  " else "00") & integer'Image(HD_HD)(2 .. 2);
      end if;
      if MM_MM < 10 then
         MINUTE  := (if JSON then " " else "0") & integer'Image(MM_MM)(2 .. 2);
      else
         MINUTE  := integer'Image(MM_MM)(2 .. 3);
      end if;
      if SC_SC < 10 then
         SECOND  := (if JSON then " " else "0") & integer'Image(SC_SC)(2 .. 2);
      else
         SECOND := integer'Image(SC_SC)(2 .. 3);
      end if;
      if HS_HS = 0 then
         DSECOND := "0";
      else
         DSECOND := integer'Image(HS_HS)(2 .. 2);
      end if;

      if CUT = UNSIGNED then
         if JSON then -- give sign separately from sexagesimal to allow decimal parsing without the infamous -0 bug.
            return("[1" & SEPARATOR & DEGR & SEPARATOR & MINUTE & SEPARATOR & SECOND & "." & DSECOND & "]");
         else -- always ddd mm ss.s
            return(" "      &         DEGR & SEPARATOR & MINUTE & SEPARATOR & SECOND & "." & DSECOND);
         end if;
      else
         if JSON then -- give sign separately from sexagesimal to allow decimal parsing without the infamous -0 bug.
            return("[" & (if SIGN = "+" then " 1" else "-1")
                        & SEPARATOR & DEGR & SEPARATOR & MINUTE & SEPARATOR & SECOND & "." & DSECOND & "]");
         else
            if DEGR(1) = '0' then -- smart format to drop leading zero to produce [+-]dd mm ss.s
               return(" " & SIGN & DEGR(2..3) & SEPARATOR & MINUTE & SEPARATOR & SECOND & "." & DSECOND);
            else                     -- smart format has full [+-]ddd mm ss.s
               return(SIGN    &       DEGR & SEPARATOR & MINUTE & SEPARATOR & SECOND & "." & DSECOND);
            end if;
         end if;
      end if;
   end case;
end OUT_SEXAGESIMAL;

------------------------------------------------------------------------------------------------------------------------------------
procedure FLEX(HA  : RADIAN;
               DEC : RADIAN;
               HA_FLEX  : out RADIAN;
               DEC_FLEX : out RADIAN;
               MODEL    : MOUNT) is -- HISTORICAL FLEXURE CALCULATION FOR 82".

-- Coefficients: 
--       x0    HA offset
--       x1    Dec offset
--       x2    x tilt (1)
--       x3    y tilt (1)
--       x4    x tilt (2)
--       x5    y tilt (2)
--       x6    transverse
--       x7    non-orthogonality
--       x8    linear HA in HA
--       x9    linear Dec in Dec
--       xa    flex 1
--       xb    flex 2
--       xc    flex 3
--       xd    HA squared in HA
--       xe    cos Dec in Dec
--       xg    sin Dec in Dec

COEFF: array (integer range -2 .. 15) of Long_Float; -- Ignore first two coefficients, which were the solved offsets in arcsec.
HA_COR, DEC_COR, COS_HA, SIN_HA, LAT, COLAT : Long_Float;
FIVE_D : Long_Float := 5.0 * PI / 180.0; -- Historical.
begin
   LAT   := Long_Float(OBS_LATITUDE * PI / 180.0);
   COLAT := PI / 2.0 - LAT;

   -- Historical models.
   case MODEL is
     when VIS_E => COEFF := (-107.1, -72.0, -0.0000030, 0.0017521, 0.0000082, 0.0000345, -0.0000237, -0.0001062, -0.0001781, 0.0006710, -0.0001796, 0.0000963, 0.0000045, -0.0000395, 0.0004098, 0.0004724, 0.0000390, -0.0002209);
     when VIS_W => COEFF := (-4.3, 15.1, 0.0019244, -0.0032995, 0.0001884, 0.0000836, 0.0000020, 0.0000618, -0.0018417, -0.0007053, -0.0000020, -0.0002065, -0.0000091, 0.0002062, 0.0000442, -0.0004894, -0.0000362, 0.0001784);
     when CAS_E => COEFF := (-76.5, -59.6, -0.0000389, 0.0018630, 0.0000841, 0.0000491, -0.0000301, -0.0001554, -0.0000733, 0.0006370, -0.0003661, -0.0003499, 0.0000011, 0.0001832, 0.0002328, 0.0004891, -0.0001722, 0.0003124);
     when CAS_W => COEFF := (-21.2, 8.5, 0.0027736, -0.0005111, 0.0001224, 0.0000440, -0.0000420, 0.0000181, 0.0005396, -0.0006150, -0.0003652, 0.0003180, -0.0000033, 0.0007971, 0.0001608, -0.0005426, 0.0002018, -0.0004368);
     when COU_E => COEFF := (-50.1, -137.5, 0.0002845, 0.0003438, -0.0000524, -0.0000140, -0.0000820, 0.0001571, 0.0000017, 0.0006388, 0.0000000, 0.0000000, 0.0000000, -0.0003124, 0.0000227, 0.0004573, -0.0001204, -0.0001379);
     when BLANK => COEFF := (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
   end case;

   HA_COR  := HA + COEFF(0);
   DEC_COR := DEC + COEFF(1);
   COS_HA  := cos(HA_COR);
   SIN_HA  := sin(HA_COR);

   HA_FLEX := HA -
           (-(COEFF(3) * SIN_HA + COEFF(2) * COS_HA + COEFF(7)) * tan(DEC_COR)
            + COEFF(6)/cos(DEC_COR)
            + COEFF(10) * SIN_HA/cos(DEC_COR - COLAT)
            + COEFF(11) * SIN_HA * cos(LAT)
            + COEFF(8) * HA_COR
            + COEFF(13) * (HA_COR + FIVE_D) * (HA_COR + FIVE_D)
            + COEFF(0));
   HA_FLEX :=  RADIAN(SLA_DRANGE(Double_Precision(HA_FLEX)));


   DEC_FLEX := DEC -
            (-COEFF(5) * COS_HA + COEFF(4) * SIN_HA
             + COEFF(12) * COS_HA * sin(DEC_COR - COLAT)
             + COEFF(9) * DEC_COR
             + COEFF(14) * cos(DEC_COR)
             + COEFF(15) * sin(DEC_COR)
             + COEFF(1));
   DEC_FLEX :=  RADIAN(SLA_DRANGE(Double_Precision(DEC_FLEX)));
end FLEX;

------------------------------------------------------------------------------------------------------------------------------------
function ZENITH_DISTANCE (HOUR_ANGLE  : RADIAN;
                          DECLINATION : RADIAN) return DEGREE is -- ZD IN DEGREES.

A, H, D, P3 : Long_Float;

begin
   H := -HOUR_ANGLE;              -- Negative of Hour angle in radians.
   D :=  PI / 2.0 - DECLINATION;  -- Codeclination in radians.
   A :=  -(PI / 2.0 - (Long_Float(OBS_LATITUDE) * PI / 180.0));  -- colatitude of back-rotation in radians.

   P3 := -sin(A) * (cos(H) * sin(D)) + cos(A) * cos(D);
   return(DEGREE(arccos(P3) * 180.0 / PI));

end ZENITH_DISTANCE;

------------------------------------------------------------------------------------------------------------------------------------
function AZIMUTH (HOUR_ANGLE  : RADIAN;
                  DECLINATION : RADIAN) return DEGREE is -- AZIMUTH IN DEGREES.

A, H, D, P1, P2 : Long_Float;

begin
   H := -HOUR_ANGLE;             -- Negative of Hour angle in radians.
   D :=  PI / 2.0 - DECLINATION; -- Codeclination in radians.
   A :=  -(PI / 2.0 - (Long_Float(OBS_LATITUDE) * PI / 180.0));  -- colatitude of back-rotation in radians.

   P1 := cos(a) * (cos(H) * sin(D)) + sin(A) * cos(D);
   P2 := sin(H) * sin(D);

   return(DEGREE(180.0 -180.0 / PI * arctan(P2, P1))); 

end AZIMUTH;

------------------------------------------------------------------------------------------------------------------------------------
function INTIN(COMMAND  : in String;
               POSITION : in out Integer) return Integer is
-- Read an integer from comma-separated or space-separated items in string COMMAND starting at character POSITION, leaving
-- POSITION at the last character before the comma.
LAST_I  : Integer := 0;
START_I : Integer := POSITION;
begin
   for C in POSITION .. COMMAND'Last loop
      exit when COMMAND(C) = ',' or COMMAND(C) = ' ';
      LAST_I := C;
   end loop;
   POSITION := LAST_I;
   Return Integer'Value(COMMAND(START_I..LAST_I));
end;

------------------------------------------------------------------------------------------------------------------------------------
function LFLTIN(COMMAND  : in String;
                POSITION : in out Integer) return Long_Float is
-- Read a long float from comma-separated or space-separated items in string COMMAND starting at character POSITION, leaving
-- POSITION at the last character before the comma. Value attribute accepts numbers without a decimal point.
LAST_I  : Integer := 0;
START_I : Integer := POSITION;
begin
   for C in POSITION .. COMMAND'Last loop
      exit when COMMAND(C) = ',' or COMMAND(C) = ' ';
      LAST_I := C;
   end loop;
   POSITION := LAST_I;
   Return Long_Float'Value(COMMAND(START_I .. LAST_I));
end;

------------------------------------------------------------------------------------------------------------------------------------
function IN_SEXAGESIMAL(COMMAND   : in String;
                        START_ARG : in out Integer;
                        FORMAT    : UNIT;
                        CUT       : FORM := UNSIGNED) return Long_Float is
-- Allow minutes and seconds to be 60 as a concession to string input where 59.9999... got rounded.

SEX_VAL  : Long_Float;
SEX_SIGN : Long_Float;
SEX_DONE : Boolean := FALSE;
-- Temporaries.
SEX_FLT  : Long_Float;
begin
   if COMMAND (START_ARG) = '-' then
      SEX_SIGN := -1.0;                  -- Store the sign to apply at end.
   else
      SEX_SIGN := 1.0;
   end if;
   SEX_VAL := abs(LFLTIN(COMMAND, START_ARG)); -- Get HH as decimal.
   if COMMAND(START_ARG + 1) = ',' then
      START_ARG := START_ARG + 2;
      SEX_FLT := LFLTIN(COMMAND, START_ARG);
      if SEX_FLT < 0.0 or SEX_FLT > 60.0 then raise Constraint_Error; end if; -- Minutes cannot be negative or >60.
      SEX_VAL := SEX_VAL + SEX_FLT / 60.0; -- Accumulate the minutes.
      if COMMAND(START_ARG + 1) = ',' then
         START_ARG := START_ARG + 2;
         SEX_FLT := LFLTIN(COMMAND, START_ARG) / 3600.0; -- Seconds can be decimal
         if SEX_FLT < 0.0 or SEX_FLT > 60.0 / 3600.0 then raise Constraint_Error; end if; -- but they can't be negative or >61.0
         SEX_VAL := SEX_VAL + SEX_FLT;
         SEX_DONE := TRUE;
      end if;
   end if;

   if SEX_DONE = FALSE then
      raise PARSE_ERROR;
   end if;

   SEX_VAL := SEX_SIGN * SEX_VAL;

   if FORMAT = HOURS then
      if CUT = UNSIGNED then
         if SEX_VAL < 0.0 or SEX_VAL > 24.0 then raise Constraint_Error; end if;
      else
         if SEX_VAL < -12.0 or SEX_VAL > 12.0 then raise Constraint_Error; end if;
      end if;
   elsif FORMAT = DEGREES then
      if CUT = UNSIGNED then
         if SEX_VAL < 0.0 or SEX_VAL > 360.0 then raise Constraint_Error; end if;
      else
         if SEX_VAL < -90.0 or SEX_VAL > 90.0 then raise Constraint_Error; end if;
      end if;
   end if;

   Return SEX_VAL;

end IN_SEXAGESIMAL;

------------------------------------------------------------------------------------------------------------------------------------
procedure IERS is
-- Read excerpt from Bulletin A to get TAI, polar motion, and dUT1.
use Ada.Text_IO;
IERS_FILE  : File_Type;     -- Handle to iers.dat.
IERS_COUNT : Integer := 0;  -- Counter for populating the matrix rows.
TAI_UTC : Long_Float;       -- TAI - UTC from Bulletin A.
package Long_Float_IO is new Ada.Text_IO.Float_IO(Long_Float); use Long_Float_IO;
begin -- Read daily PM and UT1-UTC data, prepared from IERS Bulletin A.
   open(File => IERS_FILE, Mode => In_File, Name => IERS_PATH);
   Get(IERS_FILE, TAI_UTC); -- First number is TAI - UTC.
   DTT := Double_Precision(TAI_UTC) + 32.184;
   while not End_of_File(IERS_FILE) loop
      for C in IERS_FIELD loop
         Get(IERS_FILE, IERS_DATA(IERS_COUNT,C));
      end loop;
      IERS_COUNT := IERS_COUNT + 1;
   end loop;
   close(IERS_FILE);
exception
   when others => IERS_ERROR := TRUE; -- Diagnostics at end of POINT82 tell this. Use default DTT.
end IERS;

end ANCILLARY;
