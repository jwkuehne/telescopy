with GNAT.Sockets; use GNAT.Sockets;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with ancillary;

package pmac is

type STATUS       is (PMAC_GOOD, PMAC_ERROR, SOCKET_OPEN, SOCKET_CLOSED, SOCKET_ERROR);
type P_DISPATCH   is (OP, CL, AS);
type PMAC_NUMBERS is array (natural range <>) of Long_Float;
type INTEGRITY    is record
                        HEALTH   : STATUS;
                        DOG_TIME : Time;
                     end record;

protected PCOM is
   procedure GRIND(COMMAND         : in string;
                   EXTRACTED       : out PMAC_NUMBERS;
                   EXTRACTED_COUNT : out integer;
                   CHAN            : in GNAT.Sockets.Stream_Access := null)
                   with Pre => COMMAND'Length <= ancillary.STORE + 2;

   function  MONITOR return INTEGRITY;
   -------
   private
      PMAC_CLIENT : Socket_Type;
      PMAC_CHAN   : Stream_Access := null;
      HEALTH      : STATUS;
      DOG_TIME    : Time := Clock;

end PCOM;
end pmac;
