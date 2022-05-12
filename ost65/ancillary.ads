with Interfaces.Fortran; use Interfaces.Fortran;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

package ANCILLARY is

package Elfun is new Ada.Numerics.Generic_Elementary_Functions(Long_Float); use Elfun;

CR          : constant character := character'val(13); -- Carriage Return: used by BEI encoder devices.
NL          : constant character := character'Val(10); -- New Line: terminates lines in formatted text,legacy and JSON output.
AK          : constant character := character'Val(6);  -- Terminates JSON socket transmissions.
BL          : constant character := character'Val(7);  -- Signal exception on JSON socket transmissions, followed by AK.
PI          : constant := 3.14159265358979; -- Result of 2.0*arcsin(1.0).
PARSE_ERROR : exception;                    -- Catch bad input format.

STORE : constant := 255; -- Maximum line length for reading or writing command on a socket;

-- Subtype conveniently leaves RADIAN compatible with Long_Float, but it can't accidentally mix with DEGREE and HOUR.
subtype RADIAN is Long_Float     range (-2.0 * PI) .. (2.0 * PI);
type    DEGREE is new Long_Float range      -360.0 .. 360.0;
type    HOUR   is new Long_Float range       -24.0 .. 24.0;

-- The mount model is used in the flexure code, and is set in zeros.dat with command CM (choose mount). These are the historical
-- models for VISual cassegrain, heavy CASsegrain,  and COUde. There never was a prime focus model, and Coude is retired.
type MOUNT is (VIS_E, VIS_W, CAS_E, CAS_W, COU_E, BLANK);
-- The instrument is alphanumeric with a Cassegrain or Prime affix and is set with CI (choose instrument).
type INST  is (ARGC, ARGP, CQNC, SESC, ES2C, NONE);

-- For reading and writing sexagesimal numbers, choose the style.
type UNIT          is (HOURS, DEGREES);   -- Tell SEXAGESIMAL which units to use. 
type FORM          is (SIGNED, UNSIGNED); -- Tell SEXAGESIMAL where to cut the circle.

-- Catalogs are preprocessed from their original data files with _CATALOGS/Xtoads.pl < X.dat > X.ads as an array of OBJECTs. To
-- permit direct indexing into a catalog, the array index matches the OBJECT.ID by inserting empty records where there is no catalog
-- entry, e.g. HR92 and FK5-8.
type OBJECT        is record -- describing an object in BSC, IC, FK5, MESSIER, and NGC.
                         ID    : integer := -1; -- Catalog ID. Value -1 means there is no catalog entry.
                         RA    : RADIAN;        -- Right ascension, equinox and epoch depend on catalog.
                         DEC   : RADIAN;        -- Declination, equinox and epoch depend on catalog.
                         PR    : long_float;    -- Proper motion in right ascension, units depend on catalog.
                         PD    : long_float;    -- Proper motion in declination, units depend on catalog.
                         MAG   : long_float;    -- Magnitude given in catalog.
                      end record;
type CATALOG       is array (integer range <>) of OBJECT; -- Created and initialized in _CATALOGS/Xtoads.pl < X.dat > X.ads
   
-- Values for the 2.1m Otto Struve Telescope, an offset German Equatorial mount.
type VECTOR is array (1..3) of Long_Float;
TUBE_OFFSET_82   : constant   := 2.13;            -- Positive EAST tube offset. Change sign crossing pole.
DOME_RADIUS_82   : constant   := 10.7;            -- Dome radius at the slit.
DOME_OFFSET_82   : VECTOR     := (0.0, 0.0, 0.5); -- Positive SOUTH, EAST, UP. Hemisphere 0.5m up from center.

type CARDINAL is (NORTH, SOUTH, EAST, WEST);      -- TUBE position.

OBS_LATITUDE     : DEGREE   := 30.0 + 40.0 / 60.0 + 17.4 / 3600.0;           -- Telescope latitude.
OBS_LONGITUDE    : DEGREE   := -1.0 * (104.0 + 1.0 / 60.0 + 21.4 / 3600.0);  -- Telescope longitude west - SLA is signed this way.
OBS_ELEVATION    : constant := 2076.0;                                       -- Meters above geoid WGS84.

-- IERS
procedure IERS;                                           -- Reads Bulletin A excerpt into IERS_DATA, sets IERS_ERROR, DTT.
type IERS_FIELD is (IERS_YEAR, IERS_MONTH, IERS_DAY, IERS_MJD, IERS_PMX, IERS_PMY, IERS_DUT);
IERS_PATH      : string := "/var/point82/iers.dat";       -- Place where processed iers data from Bulletin A are stored.
IERS_DATA      : Array (0 .. 3650, IERS_FIELD) of Long_Float; -- Ten years and one day of data.
IERS_ERROR     : Boolean    := FALSE;                     -- Set to TRUE if data from Bulletin A can't be found.
DTT            : Double_Precision := 37.0 + 32.184;       -- Offset added to UTC to produce TT from SLA_DTT or IERS Bulletin A.

function DOME_AZIMUTH (HOUR_ANGLE     : RADIAN;
                       DECLINATION    : RADIAN;
                       TUBE_OFFSET_EW : Long_Float;
                       TUBE_OFFSET_NS : Long_Float;
                       DOME_RADIUS    : Long_Float;
                       DOME_OFFSET    : VECTOR) return DEGREE; -- REQUIRED DOME AZIMUTH 

function OUT_SEXAGESIMAL(X: RADIAN; FORMAT : UNIT; CUT : FORM := UNSIGNED; JSON : Boolean := FALSE) return string;

procedure FLEX(HA : RADIAN; DEC : RADIAN; HA_FLEX : out RADIAN; DEC_FLEX : out RADIAN; MODEL : MOUNT);

function ZENITH_DISTANCE (HOUR_ANGLE : RADIAN; DECLINATION : RADIAN) return DEGREE;

function AZIMUTH (HOUR_ANGLE : RADIAN; DECLINATION : RADIAN) return DEGREE;

function INTIN(COMMAND : in String; POSITION : in out Integer) return Integer;

function LFLTIN(COMMAND : in String; POSITION : in out Integer) return Long_Float;

function IN_SEXAGESIMAL(COMMAND : in String; START_ARG : in out Integer; FORMAT: UNIT; CUT : FORM := UNSIGNED) return Long_Float;

-- Slalib specifications. gnatmake with -largs libsla.a

--type FORTRAN_MATRIX is array (1..3, 1..3) of double_precision with convention => fortran;

function SLA_AIRMAS( ZD : in double_precision) return double_precision with import => true, convention => fortran,
         external_name => "sla_airmas_";

function SLA_DRANRM( ANGLE : Double_Precision) return Double_Precision with import => true, convention => fortran,
         external_name => "sla_dranrm_";

function SLA_DRANGE( ANGLE : Double_Precision) return Double_Precision with import => true, convention => fortran,
         external_name => "sla_drange_";

procedure SLA_CLDJ( IY : Fortran_Integer; IM: Fortran_Integer; ID: Fortran_Integer; DJM: out Double_Precision;
          J: out Fortran_Integer) with import => true, convention => fortran, external_name => "sla_cldj_";

function SLA_GMST(UT1 : in Double_Precision) return Double_Precision with import => true, convention => fortran,
         external_name => "sla_gmst_";

function SLA_DTT(UTC : Double_Precision) return Double_Precision with import => true, convention => fortran,
         external_name => "sla_dtt_";

function SLA_EPJ(DATE : Double_Precision) return Double_Precision with import => true, convention => fortran,
         external_name => "sla_epj_";

procedure SLA_FK425(R1950 : Double_Precision; D1950 : Double_Precision; DR1950 : Double_Precision; DD1950 : Double_Precision;
          P1950 : Double_Precision; V1950 : Double_Precision; R2000 : out Double_Precision; D2000 : out Double_Precision;
          DR2000 : out Double_Precision; DD2000 : out Double_Precision; P2000 : out Double_Precision; V2000 : out Double_Precision)
          with import => true, convention => fortran, external_name => "sla_fk425_";

procedure SLA_MAP(RM : Double_Precision; DM : Double_Precision; PR : Double_Precision; PD : Double_Precision; PX : Double_Precision;
          RV : Double_Precision; EQ : Double_Precision; DATE : Double_Precision; RA : out Double_Precision;
          DA : out Double_Precision) with import => true, convention => fortran, external_name => "sla_map_";

procedure SLA_AOP(RAP : Double_Precision; DAP : Double_Precision; DATE : Double_Precision; DUT : Double_Precision;
          ELONGM : Double_Precision; PHIM : Double_Precision; HM : Double_Precision; XP : Double_Precision; YP : Double_Precision;
          TDK : Double_Precision; PMB : Double_Precision; RH : Double_Precision; WL : Double_Precision; TLR : Double_Precision;
          AOB : out Double_Precision; ZOB : out Double_Precision; HOB : out Double_Precision; DOB : out Double_Precision;
          ROB : out Double_Precision) with import => true, convention => fortran, external_name => "sla_aop_";

procedure SLA_RDPLAN(DATE : Double_Precision; NP : Fortran_Integer; ELONG : Double_Precision; PHI : Double_Precision;
          RA : out Double_Precision; DEC : out Double_Precision; DIAM : out Double_Precision)
          with import => true, convention => fortran, external_name => "sla_rdplan_";

end ANCILLARY;
