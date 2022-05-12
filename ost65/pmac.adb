with GNAT.Sockets; use GNAT.Sockets;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with ancillary;    use ancillary;

-- PMAC communications in a single protected procedure GRIND. Like astrometry's GRIND, this one takes a command string and grinds
-- out information that can be used internally (the EXTRACTED array) or sent to an asynchronous server's CHANnel as JSON. Function
-- MONITOR is meant to assist an internal watchdog task in case of SOCKET_ERROR.

package body PMAC is

protected body PCOM is

   function MONITOR return INTEGRITY is -- never call from a subprogram that calls GRIND: we're checking against a stall in GRIND.
      LAST_KNOWN : INTEGRITY;
   begin
      LAST_KNOWN.HEALTH   := HEALTH;
      LAST_KNOWN.DOG_TIME := DOG_TIME;
      return(LAST_KNOWN);
   end MONITOR;

   procedure GRIND(COMMAND         : in string;
                   EXTRACTED       : out PMAC_NUMBERS;
                   EXTRACTED_COUNT : out integer;
                   CHAN            : in GNAT.Sockets.Stream_Access := null) is
   -- N.B. EXTRACTED_COUNT is -1 for any error, 0 for commands that don't return a value, and >0 otherwise.
   -----------------------
   JSON   : boolean;
   REPLY  : string (1 .. 1399); -- PMAC returns maximum of 1399 characters consisting of numbers each followed by CR and final AK.
   COUNT  : natural;            -- The number of characters returned by ASK_PMAC.
   SPOINT : integer := 1;       -- Prepare SPOINT for first character in loop to parse the string into array of Long_Float.
   -----------------------
   procedure CLOSE_PMAC is
   begin
      Close_Socket(PMAC_CLIENT);
      HEALTH := SOCKET_CLOSED;
      EXTRACTED_COUNT := 0;
   exception
      when others =>
         HEALTH := SOCKET_ERROR;
         EXTRACTED_COUNT := -1;
   end CLOSE_PMAC;
   -----------------------
   procedure OPEN_PMAC is
   begin
      begin
         CLOSE_PMAC;
      exception
         when others => null;
      end;
      Create_Socket  (Socket => PMAC_CLIENT);
      Connect_Socket (Socket => PMAC_CLIENT,
                      Server => (Family => Family_Inet,
                      Addr   => Inet_Addr ("192.168.30.20"),
                      Port   => 1025));
      PMAC_CHAN := Stream(PMAC_CLIENT);
      HEALTH    := SOCKET_OPEN;
      EXTRACTED_COUNT := 0;
   exception
      when others =>
         HEALTH := SOCKET_ERROR;
         EXTRACTED_COUNT := -1;
   end OPEN_PMAC;
   -----------------------
   procedure ASK_PMAC(QUERY : in string;
                      REPLY : out string;
                      COUNT : out natural) is
      -- Command format given for PMAC network communications, with I3=2, I6=1. Normal reply is one or more numbers each
      -- followed by CR and a final AK.
      Z8       : constant character := character'val(0);
      COMMAND  : string := character'Val(64) & character'Val(191) & Z8 & Z8 & Z8 & Z8 & Z8 & character'Val(QUERY'Length) & QUERY;
      CHAR     : character;
   begin
      if HEALTH /= SOCKET_ERROR then
         COUNT    := REPLY'First - 1; -- Initialize COUNT for loop incrementing.
         HEALTH   := PMAC_GOOD;       -- Innocent until proven guilty.
         string'Write(PMAC_CHAN, COMMAND);
         loop
            CHAR   := character'Input(PMAC_CHAN);
            if CHAR = BL then  -- PMAC error reply terminates with BL & 6 character error message & CR for PMAC I3=2 and I6=1.
               HEALTH := PMAC_ERROR;
            end if;
            exit when (CHAR = AK); -- Normal termination.
            CHAR  := (if CHAR = CR then ',' else CHAR);
            COUNT := COUNT + 1;
            REPLY(COUNT) := CHAR; -- Trying to go beyond REPLY'Last will be flagged as SOCKET_ERROR, as the protocol is now wrecked.
            exit when (HEALTH = PMAC_ERROR and (CHAR = ',')); -- BL error termination with message and CR converted to comma.
         end loop;
      end if;
   exception -- Whatever happened, call it a socket error.
      when others =>
         HEALTH := SOCKET_ERROR;
   end ASK_PMAC;
   -----------------------
   -----------------------
   begin
      JSON := (if CHAN = null then FALSE else TRUE); -- When communication CHAN specified, JSON output is turned on.
      case P_DISPATCH'Value(COMMAND(COMMAND'First .. COMMAND'First + 1)) is
      when OP =>
         OPEN_PMAC;
         if JSON then
            string'Write(CHAN,"{""OPEN"": """ & STATUS'Image(HEALTH) & """}" & NL);
         end if;
         Put_Line("OPEN_PMAC returns " & STATUS'Image(HEALTH));

      when CL =>
         CLOSE_PMAC;
         if JSON then
            string'Write(CHAN, "{""CLOSE"": """ & STATUS'Image(HEALTH) & """}" & NL);
         end if;
         Put_Line("CLOSE_PMAC returns " & STATUS'Image(HEALTH));

      when AS =>
         if COMMAND(COMMAND'First + 2) = ',' then
            ASK_PMAC(COMMAND(COMMAND'First + 3 .. COMMAND'Last), REPLY, COUNT);
            -- String representation in REPLY must not be hexadecimal, because my LFLTIN is only decimal.
            case HEALTH is
               when PMAC_GOOD =>
                  if COUNT > 0 then -- COUNT - 1 omits last CR converted to comma.
                     if JSON then
                        string'Write(CHAN, "{""ASK"": [" & REPLY(1 .. COUNT - 1) & "]}" & NL);
                     end if;
                     Put_Line(REPLY(1 .. COUNT - 1));
                     -- Parse REPLY string. EXTRACTED can have more or less elements than REPLY.
                     for I in EXTRACTED'Range loop
                        EXTRACTED(I)     := LFLTIN(REPLY, SPOINT);
                        EXTRACTED_COUNT  := I;
                        if SPOINT = COUNT - 1 then -- The last character must be a comma after the last datum.
                           exit;
                        else
                           SPOINT := SPOINT + 2;
                        end if;
                     end loop;
                  else -- Some commands return nothing except for AK.
                     EXTRACTED_COUNT := 0;
                     if JSON then
                        string'Write(CHAN, "{""ASK"": []}" & NL);
                     end if;
                  end if;
               when PMAC_ERROR =>
                  EXTRACTED_COUNT := -1;
                  Put_Line("ASK_PMAC command " & COMMAND(COMMAND'First + 3 .. COMMAND'Last) & " returns " & REPLY(1 .. COUNT));
                  if JSON then
                     string'Write(CHAN, "{""ASK"": """ & REPLY(1 .. COUNT - 1) & """}" & NL);
                  end if;
               when others =>
                  EXTRACTED_COUNT := -1;
                  Put_Line("ASK_PMAC command " & COMMAND(COMMAND'First + 3 .. COMMAND'Last) & " returns " & STATUS'Image(HEALTH));
                  if JSON then
                     string'Write(CHAN, "{""ASK"": """ & STATUS'Image(HEALTH) & """}" & NL);
                  end if;
            end case;
         else
            raise PARSE_ERROR;
         end if;
      end case;
      DOG_TIME := Clock;
   end GRIND;

end PCOM;
end PMAC;
