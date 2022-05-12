with Ada.Numerics.Generic_Elementary_Functions;    -- Need math for RADIAN, DEGREES, HOURS.
with ancillary; use ancillary;                     -- General support.

package SAFETY is

type CODE is (GREEN, YELLOW, ORANGE, RED);  -- Geobrick codes control speed and stop.
type WHY  is (NORMAL, NORTH_PIER, SOUTH_PIER, CURTAIN, ZENITH, STOW, FLAT);

procedure GUARD(HA         : in RADIAN,
                DEC        : in RADIAN,
                TUBE       : in CARDINAL,
                MOUNT_ON   : in INST,
                VERDICT    : out CODE,
                REASON     : out WHY,
                EAST_LIMIT : out HOUR,
                WEST_LIMIT : out HOUR);

end SAFETY;
