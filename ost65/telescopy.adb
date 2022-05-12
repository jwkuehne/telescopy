------------------------------------------------------------------------------------------------------------------------------------
-- Please view in a 132-column window/sheet.

-- telescopy by John W. Kuehne, 2021. Reads SSI encoders using the BEI (or similar) interface to USB with standard Linux kernel
-- driver that creates /dev/ttyUSBx devices. Produces legacy format ephemera for OST systems, including Track82, ICEX, CQUEAN, the
-- console, and hand paddles, on TCP/IP sockets for port 22370 (one-shot) and 22371 (keepalive). Integrates astrometry, and all
-- the functions needed for telescope control, except a graphical front-end. Note the asynchronous socket server is my contribution
-- to Rosetta Code "echo server".
--
-- Synopsis of encoder service:
-- Task HA_DEC opens and initializes the HA and DEC encoders, and then starts looping at 10Hz, reading the encoders and producing
-- HA, DEC, LST, UTC, and RA. On each iteration it calls Assembly, which assembles the data in a protected type to ensure the data
-- are coherent, e.g. RA = LST - HA. The protected Assembly procedure also takes cached data from task FOCUS_SPARE to create the
-- legacy output format. The cached data strategy prevents stalled IO on the FOCUS_SPARE interface from stalling the whole program,
-- as focus is not necessary for telescope safety. The communcations tasks run asynchronously, with the listening tasks started in
-- the environment task. These dispatch the asynchronous communication tasks - either one-shot or keepalive - which get the safely
-- assembled data using the protected Delivery function. The keepalive socket responds to the legacy 50-byte command, but can also
-- respond immediately upon receipt of NL. The critical HA and DEC encoders are watchdog protected against encoder parity error
-- and communicaiton errors, including stall from the BEI interface. The watchdog is checked in Delivery, called by the
-- communications tasks. Focus is also watchdog protected during Assembly, leaving dashes instead of digits in the focus value.
-- Finally, the legacy serial coordinate displays - especially on the Otto console - are serviced by a 10Hz task that sends out the
-- 66-byte format to the 2 built-in serial ports on the POC-120.

-- Connections Notes
-- The DE-15 pin assignments on the legacy connector are for the DEVA PCI board: 1:C 3:D 6:C/ 8:D/ 11:+(10 to 30)V 13:0V.
-- Note the legacy connector ties 0V to encoder pin 5 DIR/ on the blue wire. See drawing "82 Readout System" for legacy cable
-- color and pin assignment. The rewiring into the BEI interfaces included fixing egregious ground loops on HA, DEC, and FOCUS
-- that caused decades of encoder faults.

with Ada.Environment_Variables; use Ada.Environment_Variables; -- To set TZ to GMT.
with Ada.Text_IO; use Ada.Text_IO;                         -- Adequate for USB-to-Serial tty BEI interface.
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.IO_Exceptions;                                    -- For asynchronous socket communication tasks.
with Ada.Strings.Fixed; use Ada.Strings.Fixed;             -- String multiplication operator, etc.
with Ada.Numerics.Generic_Elementary_Functions;            -- Need math for Long_Float.
with Ada.Calendar; use Ada.Calendar;                       -- For acquiring UTC and local time/date, watchdog data Delivery.

with Interfaces.Fortran; use Interfaces.Fortran;           -- Using Starlink SLALIB positional astronomy by P.T. Wallace.

with GNAT.Sockets;                                         -- The socket server connections are asynchronous.
with GNAT.OS_Lib;                                          -- Force DOG_TIME abort, even if stalled in a protected procedure.

with ancillary;  use ancillary;                            -- Fortran SLALIB calls and all global definitions.
with astrometry; use astrometry;                           -- Positional astrometry, zeros, mount model selection.
with pmac;       use pmac;

procedure telescopy is

   package TTY renames Ada.Text_IO;
 
   Tasks_To_Create : constant := 20; -- simultaneous socket connections.
 
   package Long_Float_IO is new Ada.Text_IO.Float_IO(Long_Float); use Long_Float_IO;
   package Long_Float_Elfun is new Ada.Numerics.Generic_Elementary_Functions(Long_Float); use Long_Float_Elfun;

   SIGIL : character := '*'; -- Non-legacy commands begin with *, followed by class letter and two-letter class code, e.g. *ABS,1001

------------------------------------------------------------------------------------------------------------------------------------
-- Communicate with the USB serial port. Sends a string, gets a reply terminated with a CR, and parses for BEI interface.
-- Protection guarantees that the reply is for the command, so this can used freely in a tasking environment.
---------------------------------------

   type BEI_CODE is (DATUM, ACK, NACK, INTERFACE_ERROR, ENCODER_ERROR, IO_EXCEPTION); -- BEI status code in BEI_IO.

   protected type BEI_IO is
      procedure OpenTTY(DEVICE : in string);
      procedure IO(REQUEST  : in string;
                   POSITION : out integer;
                   STATUS   : out BEI_CODE);
      procedure CloseTTY;
      -------
      private
         USBO : TTY.File_Type; -- Out_File handle.
         USBI : TTY.File_Type; -- In_File handle.
         CHAR : character;     -- Store a character at a time when reading reply,
   end BEI_IO;

   protected body BEI_IO is
      procedure OpenTTY(DEVICE : in string) is
      begin
         -- Nota Bene: shared=no for opening the same file twice, as the /dev/tty devices are read/write.
         TTY.Open(File => USBO, Mode => TTY.Out_File, Name => DEVICE, Form => "shared=no");
         TTY.Open(File => USBI, Mode => TTY.In_File,  Name => DEVICE, Form => "shared=no");
      exception
         when others =>
            Put_Line("TTY OPEN ERROR " & DEVICE & ".");
      end;

      procedure IO(REQUEST : in string; POSITION : out integer; STATUS : out BEI_CODE) is
      REPLY_STRING : string(1 .. 80);
      REPLY_COUNT  : integer := 0;
      begin
         TTY.Put(USBO, REQUEST); -- Send the string to the interface,
         TTY.PUT(USBO, CR);      -- and the terminator.
         TTY.Flush(USBO);        -- Override Text_IO buffers and send immediately.
         loop
            TTY.Get_Immediate(USBI, CHAR); -- Get character, no Text_IO meddling.
            exit when CHAR = CR;           -- Exits upon receipt of CR. See DOG_TIME.
            REPLY_COUNT := REPLY_COUNT + 1;    -- Increment storage pointer
            REPLY_STRING(REPLY_COUNT) := CHAR; -- and store character in the REPLY.
         end loop;
         if REPLY_STRING(1 .. 5)     = "*0ACK"  then -- Success code for setting encoder bit length and parity.
            STATUS := ACK;
         elsif REPLY_STRING(1 .. 6)  = "*0NACK" then -- Error code from interface.
            STATUS := NACK;
         elsif REPLY_STRING(1 .. 3)  = "*0R"    then -- candidate for parsing encoder response.
            if REPLY_STRING(REPLY_COUNT - 1 .. REPLY_COUNT) = ",1"  then -- a parity error was raised.
               STATUS := ENCODER_ERROR;
            else
               if REPLY_STRING(REPLY_COUNT - 1 .. REPLY_COUNT) = ",0" then       -- parity checked.
                  STATUS := DATUM;
                  POSITION := integer'value(REPLY_STRING(5 .. REPLY_COUNT - 2)); -- Extract the encoder position string.
               else -- no parity information present, but decode value anyway.
                  STATUS := DATUM;
                  POSITION := integer'value(REPLY_STRING(5 .. REPLY_COUNT));     -- Extract the encoder position string.
               end if;
            end if;
         else -- an unspecified erroneous reply was encountered.
            STATUS := INTERFACE_ERROR;
         end if;
      exception
         when others =>
            STATUS := IO_EXCEPTION;
      end; 

      procedure CloseTTY is
      begin
         TTY.Close(USBO);
         TTY.Close(USBI);
      exception
         when others =>
            Put_Line("TTY CLOSE ERROR.");
      end;
   end BEI_IO;

   -- Create the BEI_IO to the SSI-USB interface for critical encoders HA and DEC, and a second interface for FOCUS and SPARE.
   HA_DEC_INTERFACE : BEI_IO; -- Interface 1 hosts HA and DEC.
   FOCUS_SPARE_INTERFACE  : BEI_IO; -- Interface 2 hosts FOCUS.

------------------------------------------------------------------------------------------------------------------------------------
-- Prepare protection for DATA PRODUCTs to ensure the functionally delivered data are coherent against the asynchronous tasks.
---------------------------------------
   type RAW is array(1 .. 4) of integer; -- For storing raw encoder values in type Payload.

   type Payload is record
      ENCODER : RAW; -- Raw encoder values.
      -- Legacy format, default empty error response. Each packet is 128 bytes, and each line is terminated with CRLF.
      -- The _V variables are up-to-date and valid when the corresponding string is not composed of dashes.
      TUBE    : CARDINAL;                                      -- EAST or WEST, determined when DEC encoder crosses north pole.
      UTC     : string(1 .. 16) := "0  --:--:--.--" & CR & NL; -- Seconds.
      UTC_V   : Day_Duration;
      UTC_DATE: string(1 .. 16) := "1  --:--:--.--" & CR & NL; -- YY:YY:MO.DA
      YEAR_V  : Year_Number; MONTH_V : Month_Number; DAY_V : Day_Number;
      AVAIL1  : string(1 .. 16) := "2             " & CR & NL; -- Unused.
      RA      : string(1 .. 16) := "3? --:--:--.--" & CR & NL; -- Hours. Position 2 has E or W if command starts with 'axis'.
      RA_V    : RADIAN; -- SLALIB units.
      LST     : string(1 .. 16) := "4  --:--:--.--" & CR & NL; -- Hours.
      LST_V   : RADIAN; -- SLALIB units.
      HA      : string(1 .. 16) := "5 ---:--:--.--" & CR & NL; -- Hours.
      HA_V    : RADIAN; -- SLALIB units.
      DEC     : string(1 .. 16) := "6 ---:--:--.--" & CR & NL; -- Degrees.
      DEC_V   : RADIAN; -- SLALIB units.
      FOCUS   : string(1 .. 16) := "7 ---:--:--.--" & CR & NL; -- Legacy format for fixed displays - just an integer.
      FOCUS_V : integer; -- Arbitrary units from encoder, but actually close to microns.
      SPARE_V : integer;
   end record;

   -- The second BEI interface is used for focus, but it's not essential for telescope safety. To prevent an IO stall from
   -- shutting down the whole works, the data are cached in this protected object, where Assembly integrates them.
   type OPTIONAL_DATA is record
      DOG_TIME      : Time;     -- IO timeout protects freshness.
      FOCUS         : integer;  -- Cooked focus encoder value.
      SPARE         : integer;  -- Second encoder not currently used.
      FOCUS_ENCODER : integer;  -- Raw focus encoder value.
      SPARE_ENCODER : integer;  -- Raw spare encoder value (not programmed.)
   end record;

   protected Coherent is -- Coherently assemble data from encoders and log the raw encoder values to the console.
      procedure Assembly(NOW   : in Time;
                         YEAR  : in Year_Number;
                         MONTH : in Month_Number;
                         DAY   : in Day_Number;
                         UTC   : in Day_Duration; 
                         LST   : in RADIAN;
                         HA    : in RADIAN;
                         DEC   : in RADIAN;
                         RA    : in RADIAN;
                         TUBE  : in CARDINAL;
                         HA_ENCODER  : in integer;
                         DEC_ENCODER : in integer);
      procedure Fill_Cache(DOG_TIME : Time;
                           FOCUS : integer;
                           SPARE : integer;
                           FOCUS_ENCODER : in integer;
                           SPARE_ENCODER : in integer);
      function  Delivery return Payload;
      -------
      private
         PROTOTYPICAL : Payload; -- Shows format without any data, for error return. Track82 should abandon motion when delivered.
         SAFE         : Payload; -- When delivered, critical data are coherent against asynchronous access by communications tasks.
         CACHE        : OPTIONAL_DATA; -- Second BEI interface caches data here.
         DOG_TIME     : Time := Clock; -- Watchdog IO on BEI interfaces for freshness. Initialize so abs below is always defined.
   end Coherent;

   protected body Coherent is
      procedure Assembly(NOW   : in Time;
                         YEAR  : in Year_Number;
                         MONTH : in Month_Number;
                         DAY   : in Day_Number;
                         UTC   : in Day_Duration;
                         LST   : in RADIAN;
                         HA    : in RADIAN;
                         DEC   : in RADIAN;
                         RA    : in RADIAN;
                         TUBE  : in CARDINAL;
                         HA_ENCODER  : in integer;
                         DEC_ENCODER : in integer) is
      begin
         SAFE         := PROTOTYPICAL; -- Clear detritus, i.e. we should not depend on strictly overwriting the old strings.
         DOG_TIME     := NOW;          -- Timestamp these data. Checked for freshness on Delivery in communication tasks.
         SAFE.ENCODER := (HA_ENCODER, DEC_ENCODER, CACHE.FOCUS_ENCODER, CACHE.SPARE_ENCODER);

         SAFE.TUBE    := TUBE;
         SAFE.UTC_V   := UTC;
         SAFE.YEAR_V  := YEAR;
         SAFE.MONTH_V := MONTH;
         SAFE.DAY_V   := DAY;
         SAFE.RA_V    := RA;
         SAFE.LST_V   := LST;
         SAFE.HA_V    := HA;
         SAFE.DEC_V   := DEC;
         SAFE.FOCUS_V := CACHE.FOCUS;
         SAFE.SPARE_V := CACHE.SPARE;
      
         -- N.B. convert UTC in Day_Duration seconds to radians for SEXAGESIMAL output.
         SAFE.UTC(3 .. 14)      := OUT_SEXAGESIMAL(PI / 12.0 * Long_Float(UTC) / 3600.0, HOURS, UNSIGNED, FALSE);
         SAFE.UTC_DATE(4..5)   := Integer'Image(YEAR)(2..3); SAFE.UTC_DATE(7..8) := Integer'Image(YEAR)(4..5);
         SAFE.UTC_DATE(10..11) := (if Integer(MONTH) < 10 then "0" & Integer'Image(MONTH)(2..2) else Integer'Image(MONTH)(2..3));
         SAFE.UTC_DATE(13..14) := (if Integer(DAY)   < 10 then "0" & Integer'Image(DAY)(2..2)   else Integer'Image(DAY)(2..3));
         SAFE.LST(3 .. 14)     := OUT_SEXAGESIMAL(LST, HOURS, UNSIGNED, JSON=>FALSE);
         SAFE.HA(3  .. 14)     := OUT_SEXAGESIMAL(HA,  HOURS, SIGNED,   JSON=>FALSE);
         SAFE.DEC(2 .. 13)     := OUT_SEXAGESIMAL(DEC, DEGREES, SIGNED, JSON=>FALSE);
         SAFE.DEC(14)          := '0'; -- ancillary outputs only tenths for dec, hundredths for time, but legacy display is fixed.
         SAFE.RA(3  .. 14)     := OUT_SEXAGESIMAL(RA,  HOURS, UNSIGNED, JSON=>FALSE);
         SAFE.RA(2)            := (if TUBE = EAST then 'E' else 'W'); -- Extension to the legacy format seems to be safe for ICEX.

         -- The legacy FOCUS format is just decimal with colons, probably made to support ancient mechanical displays, numitrons,
         -- etc. Easiest to plug digits into the 7 colon-separated sexagesimal places, and negative sign into place 3.
         if abs(CLOCK - CACHE.DOG_TIME) <= 1.0 then -- FOCUS encoder is fresh enough.
            declare
            FOCUS_STRING : string  := integer'Image(CACHE.FOCUS);
            INDEX        : integer := FOCUS_STRING'Last;
            begin
               SAFE.FOCUS(4 .. 14) := "00:00:00.00";
               SAFE.FOCUS(3) := (if FOCUS_STRING(1) = '-' then '-' else ' '); -- Apparently + was not used.
               if INDEX > 1 then SAFE.FOCUS(13) := FOCUS_STRING(INDEX); INDEX := INDEX - 1; end if;
               if INDEX > 1 then SAFE.FOCUS(11) := FOCUS_STRING(INDEX); INDEX := INDEX - 1; end if;
               if INDEX > 1 then SAFE.FOCUS(10) := FOCUS_STRING(INDEX); INDEX := INDEX - 1; end if;
               if INDEX > 1 then SAFE.FOCUS(8)  := FOCUS_STRING(INDEX); INDEX := INDEX - 1; end if;
               if INDEX > 1 then SAFE.FOCUS(7)  := FOCUS_STRING(INDEX); INDEX := INDEX - 1; end if;
               if INDEX > 1 then SAFE.FOCUS(5)  := FOCUS_STRING(INDEX); INDEX := INDEX - 1; end if;
               if INDEX > 1 then SAFE.FOCUS(4)  := FOCUS_STRING(INDEX); INDEX := INDEX - 1; end if;
            end;
         else
            null; -- Loss of FOCUS is not enough reason to halt the server and telescope. Just fill with PROTOTYPICAL dashes.
         end if;
      exception
         when others => Put_Line("A critical sexagesimal conversion error occurred.");
                        Put("UTC   "); Put(PI / 12.0 * Long_Float(UTC) / 3600.0, 10,10,0); New_Line;
                        Put("LST   "); Put(LST, 10,10,0);  New_Line;
                        Put("HA    "); Put(HA,  10,10,0);  New_Line;
                        Put("DEC   "); Put(DEC, 10,10,0);  New_Line;
                        Put("RA    "); Put(RA,  10,10,0);  New_Line;
                        Put("FOCUS "); Put(CACHE.FOCUS);   New_Line;
                        -- This should never happen, so if it does, nothing can be trusted, hence the total wipeout.
                        Flush; -- Write any pending buffered bytes to STDOUT.
                        GNAT.OS_Lib.OS_Exit(-1); -- Abort_Task may fail if stalled in protected IO procedure, hence hard kill.
      end Assembly;
 
      procedure Fill_Cache(DOG_TIME : in Time; FOCUS : integer; SPARE : integer; FOCUS_ENCODER : in integer;
                           SPARE_ENCODER : in integer) is
      begin
        CACHE.FOCUS         := FOCUS;
        CACHE.SPARE         := SPARE;
        CACHE.DOG_TIME      := DOG_TIME; 
        CACHE.FOCUS_ENCODER := FOCUS_ENCODER;
        CACHE.SPARE_ENCODER := SPARE_ENCODER;
      end Fill_Cache;
 
      function Delivery return Payload is
      begin
         if abs(CLOCK - DOG_TIME) > 1.0 then -- N.B. Ada.Calendar operator overloads to return seconds.
            Put_Line("ERROR: Interface for HA/DEC timed out.");
            Flush; -- Write any pending buffered bytes to STDOUT.
            GNAT.OS_Lib.OS_Exit(-1); -- Abort_Task may fail if stalled in protected IO procedure.
         else
            return(SAFE);
         end if;
      end Delivery;
   end coherent;

------------------------------------------------------------------------------------------------------------------------------------
-- Use stack to pop the next free task index. When a task finishes its
-- asynchronous (no rendezvous) phase, it pushes the index back on the stack.
---------------------------------------
   type Integer_List is array (1 .. Tasks_To_Create) of integer;
   subtype Counter   is integer range 0 .. Tasks_To_Create;
   subtype Index     is integer range 1 .. Tasks_To_Create;
   protected type Info is
      procedure Push_Stack (Return_Task_Index : in Index);
      procedure Initialize_Stack;
      entry Pop_Stack (Get_Task_Index : out Index);
      -------
      private
         Task_Stack   : Integer_List; -- Stack of free-to-use tasks.
         Stack_Pointer: Counter := 0;
   end Info;
 
   protected body Info is
      procedure Push_Stack (Return_Task_Index : in Index) is
      begin -- Performed by tasks that were popped, so won't overflow.
         Stack_Pointer := Stack_Pointer + 1;
         Task_Stack(Stack_Pointer) := Return_Task_Index;
      end;
 
      entry Pop_Stack (Get_Task_Index : out Index) when Stack_Pointer /= 0 is
      begin -- guarded against underflow.
         Get_Task_Index := Task_Stack(Stack_Pointer);
         Stack_Pointer := Stack_Pointer -  1;
      end;         
 
      procedure Initialize_Stack is
      begin
         for I in Task_Stack'range loop
            Push_Stack (I);
         end loop;
      end;
   end Info;
 
   -- Create a single stack for keeping track of available tasks in the Worker array of tasks below.
   Task_Info : Info;

------------------------------------------------------------------------------------------------------------------------------------
-- These are the tasks that are popped from the stack for use, and returned
-- to the pool for reuse when finished using the protected stack operations.
---------------------------------------
   task type SocketTask is
      -- Rendezvous the setup, which sets the parameters for entry Ephemera.
      entry Setup (Connection : GNAT.Sockets.Socket_Type;
                   Client     : GNAT.Sockets.Sock_Addr_Type;
                   Channel    : GNAT.Sockets.Stream_Access;
                   Task_Index : Index;
                   Disconnect : boolean);
                   
      -- Ephemera accepts the asynchronous phase, i.e. no rendezvous. When the
      -- communication is over, push the task number back on the stack.
      entry Ephemera;
   end SocketTask;
 
   task body SocketTask is
      my_Connection : GNAT.Sockets.Socket_Type;
      my_Client     : GNAT.Sockets.Sock_Addr_Type;
      my_Channel    : GNAT.Sockets.Stream_Access;
      my_Index      : Index; -- Task used for this socket.
      my_Disconnect : boolean; -- if TRUE then socket sends data upon connection, closes immedidately.
  
      CHAR          : character;                               -- Read one character at a time.
      LINE          : string(1 .. ancillary.STORE);            -- Where the command is accumulated.
      IN_COUNT      : integer range 0 .. ancillary.STORE := 0; -- Pointer for LINE.
      SAFE          : Payload;                                 -- The coherently assembled time and postional data.
      BUFFER        : string(1 .. 128);                        -- The legacy formatted time and positional data.
   begin
      loop -- Infinitely reusable
         accept Setup (Connection : GNAT.Sockets.Socket_Type; 
                       Client  : GNAT.Sockets.Sock_Addr_Type; 
                       Channel : GNAT.Sockets.Stream_Access;
                       Task_Index : Index;
                       Disconnect : boolean) do
            -- Store parameters and mark task busy.
            my_Connection := Connection;
            my_Client     := Client;
            my_Channel    := Channel;
            my_Index      := Task_Index;
            my_Disconnect := Disconnect;
         end;
 
         accept Ephemera; -- Async (non-rendezvous) communications return computed telescope ephemera in legacy format.
                          -- Extended sigil commands, starting with *, have a broader scope, including motion commands, etecetera.
         begin
            loop
               LINE := ancillary.STORE * " "; -- Clear command to spaces; parser terminates on space.
               if my_Disconnect = FALSE then -- Socket stays open, service sigil and legacy commands.
                  IN_COUNT := 0;
                  loop
                     CHAR := Character'Input(my_Channel);
                     exit when (CHAR = NL);    -- Terminate on NL. NL never part of LINE.
                     IN_COUNT := IN_COUNT + 1; -- Accept new character, increment pointer into LINE.
                     LINE(IN_COUNT) := (if CHAR=CR then ' ' else CHAR); -- Blank out CR before LF, e.g. Telnet EOL behavior.
                     exit when (IN_COUNT = 50 and LINE(1) /= SIGIL) or IN_COUNT = LINE'Last; -- Legacy command 50 null bytes.
                  end loop;
                  if LINE(1) = SIGIL and IN_COUNT >= 4 then -- it's a sigil command, i.e. *{A,P}XY
                     Put_Line(LINE(1 .. IN_COUNT)); -- Always log incoming commands.
                     ---------------------
                     if LINE(2) = 'A' then -- it's an astrometry command.
                        declare
                        POINT_DATA : ASTROMETRIC; -- Store the internal results of GRINDing through POINT.
                        begin
                           SAFE := Coherent.Delivery;
                           POINT.GRIND(LINE(3 .. IN_COUNT), TUBE=>SAFE.TUBE, LOG=>TRUE, CHAN=>my_Channel, SONA=>POINT_DATA);
                        exception
                           when Constraint_Error =>
                              Put("Constraint error processing astrometry command!" & NL & LINE(1 .. IN_COUNT) & NL);
                              string'Write(my_Channel, "{""A_DISPATCH"": ""Constraint Error""}" & NL);
                           when Parse_Error=>
                              Put("Parse error processing astrometry command."      & NL & LINE(1 .. IN_COUNT) & NL);
                              string'Write(my_Channel, "{""A_DISPATCH"": ""Parse Error""}" & NL);
                           -- Any unexplained exception will close the socket and release the task below.
                        end;
                     ------------------------
                     elsif LINE(2) = 'P' then -- it's a PMAC command.
                        declare
                        NUMBERS       : PMAC_NUMBERS (1 .. ancillary.STORE); -- Maximum return values imaginable.
                        NUMBERS_COUNT : integer;
                        begin
                           PCOM.GRIND(COMMAND         => LINE(3 .. IN_COUNT),
                                      EXTRACTED       => NUMBERS,
                                      EXTRACTED_COUNT => NUMBERS_COUNT,
                                      CHAN            => my_Channel);
                        exception
                           when Constraint_Error =>
                              Put("Constraint error processing PMAC command." & NL & LINE(1 .. IN_COUNT) & NL);
                              string'Write(my_Channel, "{""P_DISPATCH"": ""Constraint Error""}" & NL);
                           when Parse_Error=>
                              Put("Parse error processing astrometry command."      & NL & LINE(1 .. IN_COUNT) & NL);
                              string'Write(my_Channel, "{""P_DISPATCH"": ""Parse Error""}" & NL);
                           -- Any unexplained exception will close the socket and release the task below.
                        end;
                     ------------------------
                     else
                        Put("Invalid sigil command." & NL & LINE(1 .. IN_COUNT) & NL);
                        string'Write(my_Channel, "{""TELESCOPY"": ""Invalid sigil command""}" & NL);
                     end if;
                  else -- It's not a sigil command, so assume legacy and return the legacy 128 byte packet.
                     SAFE := Coherent.Delivery; -- Get the data coherently assembled by Assembly.
                     BUFFER := SAFE.UTC & SAFE.UTC_DATE & SAFE.AVAIL1 & SAFE.RA & SAFE.LST & SAFE.HA & SAFE.DEC & SAFE.FOCUS;
                     string'Write(my_Channel, BUFFER);
                  end if;
               else -- Socket closes after delivering legacy packet, without reading anything.
                  SAFE := Coherent.Delivery; -- Get the data coherently assembled by Assembly.
                  BUFFER := SAFE.UTC & SAFE.UTC_DATE & SAFE.AVAIL1 & SAFE.RA & SAFE.LST & SAFE.HA & SAFE.DEC & SAFE.FOCUS;
                  string'Write(my_Channel, BUFFER);
                  Put_Line("END QUICK TASK" & integer'image(my_index));
                  exit; -- to RELEASE the socket.
               end if;
            end loop; -- Exit loop when client closes socket, forcing exception.
         exception
            when Ada.IO_Exceptions.End_Error =>
               Put_Line("END "   & (if my_Disconnect = FALSE then "KEEPALIVE" else "QUICK") & " TASK " & integer'image(my_Index));
            when others => 
               Put_Line("ERROR " & (if my_Disconnect = FALSE then "KEEPALIVE" else "QUICK") & " TASK " & integer'image(my_Index));
         end;
<<RELEASE>>
         GNAT.Sockets.Close_Socket (my_Connection);
         Task_Info.Push_Stack (my_Index); -- Protected return to stack of released task index.
      end loop;
   end SocketTask;

   -- Create a single pool of tasks, which can be dispatched on differnt ports via SocketServer below.
   Worker : array (1 .. Tasks_To_Create) of SocketTask;
 
------------------------------------------------------------------------------------------------------------------------------------
-- This task listens for connection requests and dispatches worker communication tasks. Disconnect is passed to the dispatched
-- worker task in its setup phase to control one-shot or keepalive.
---------------------------------------
   task type SocketServer(Disconnect : boolean)  is
      entry Setup(IP : string; Port : GNAT.Sockets.Port_Type);
      entry Listen;
   end SocketServer;
 
   task body SocketServer is
   Receiver      : GNAT.Sockets.Socket_Type;
   Connection    : GNAT.Sockets.Socket_Type;
   Client        : GNAT.Sockets.Sock_Addr_Type;
   Channel       : GNAT.Sockets.Stream_Access;
   Use_Task      : Index;
 
   -- Setup the socket receiver, initialize the task stack, and then loop, blocking on Accept_Socket, using Pop_Stack for the next
   -- free task from the stack, waiting if necessary. Reuse_Address may help with quick turnaround on new connections, and
   -- Keep_Alive may heartbeat the connection.
   begin
      accept Setup(IP : string; Port : GNAT.Sockets.Port_Type) do
         GNAT.Sockets.Create_Socket (Socket => Receiver);
         GNAT.Sockets.Set_Socket_Option (Socket => Receiver, Option => (Name => GNAT.Sockets.Reuse_Address, Enabled => True));
         if Disconnect = FALSE then
            GNAT.Sockets.Set_Socket_Option (Socket => Receiver, Option => (Name => GNAT.Sockets.Keep_Alive, Enabled => True));
         end if;
         GNAT.Sockets.Bind_Socket(Socket  => Receiver, Address => (Family => GNAT.Sockets.Family_Inet,
                                  Addr => GNAT.Sockets.Inet_Addr (IP), Port => Port));
         GNAT.Sockets.Listen_Socket (Socket => Receiver);
      end;

      accept Listen;
Find: loop -- Block for connection and take next free task.
         GNAT.Sockets.Accept_Socket
           (Server  => Receiver,
            Socket  => Connection,
            Address => Client);
         Channel := GNAT.Sockets.Stream (Connection);
         Task_Info.Pop_Stack(Use_Task); -- Protected guard waits if full house.
         -- Setup the socket in this task in rendezvous.
         Worker(Use_Task).Setup(Connection, Client, Channel, Use_Task, Disconnect);
         -- Run the asynchronous task for the socket communications.
         Worker(Use_Task).Ephemera; -- Start echo loop.
         Put_Line (NL & "Connect " & GNAT.Sockets.Image(Client) & " TASK " & integer'image(Use_Task) &
                  (if Disconnect = TRUE then " DISCONNECT" else " KEEPALIVE"));
      end loop Find;
   end SocketServer;
 
   -- Create two servers, one for persistent connections, the other for one-shot.
   Ephemera_Server_Keepalive  : SocketServer(Disconnect => FALSE);
   Ephemera_Server_Disconnect : SocketServer(Disconnect => TRUE);

   -- Create the external servers
   xEphemera_Server_Keepalive  : SocketServer(Disconnect => FALSE);
   xEphemera_Server_Disconnect : SocketServer(Disconnect => TRUE);
 
------------------------------------------------------------------------------------------------------------------------------------
-- Needed in next tasks that process the multi-turn encoder values.
---------------------------------------
   function MULTITURN_RANGE(ENCODER: in integer; OFFSET : in integer; BITS : in integer; SCALE : in long_float) return long_float is
   -- range a multi-turn encoder for crossing from 0 to 2**BITS-1. Only requires that the capacity of the encoder is not exceeded.
   -- OFFSET is the encoder value read at the zero point, e.g. DEC 0. 2**BITS is the total count, e.g. 2^12 turns and 2^13 counts
   -- per turn is a 25-bit encoder. SCALE is the final factor applied for the application, e.g. conversion to radians, microns, etc.
   begin
   if OFFSET >= 2**BITS / 2 then
      -- zero-point is in the upper half of the encoder range, so it's possible that the value could cross the upper
      -- encoder range from 2**BITS - 1 to 0.
      return( (if ENCODER < OFFSET - (2**BITS / 2) then
                 long_float(2**BITS + ENCODER - OFFSET) * SCALE
              else
                           long_float(ENCODER - OFFSET) * SCALE
             ));
   else
      -- zero-point is in the lower half of the encoder range, so it's possible that the value could cross the lower
      -- encoder range from 0 to 2**BITS - 1. Nota bene this return is >=, above is just <.
      return( (if ENCODER >= OFFSET + (2**BITS / 2) then
                 long_float(-(2**BITS) + ENCODER - OFFSET) * SCALE
              else
                              long_float(ENCODER - OFFSET) * SCALE
             ));
   end if;
   end MULTITURN_RANGE;

------------------------------------------------------------------------------------------------------------------------------------
-- Anonymous task for sending serial data to legacy devices, especially the Otto console.
---------------------------------------
   task SERIAL_OUTPUT is
      entry Commence; -- Open /dev/ttyS0 and S1 output and start sending. Must be set to 19200 baud with stty.
   end SERIAL_OUTPUT;

   task body SERIAL_OUTPUT is
   SERIAL_0 : TTY.File_Type;   -- Serial device 0.
   SERIAL_1 : TTY.File_Type;   -- Serial device 1.
   SAFE     : Payload;         -- Coherent data.
   LEGACY   : string(1 .. 66); -- Legacy format for serial dispays.
   begin
      accept Commence;
      TTY.Open(File => SERIAL_0, Mode => TTY.Out_File, Name => "/dev/ttyS0");
      TTY.Open(File => SERIAL_1, Mode => TTY.Out_File, Name => "/dev/ttyS1");
      loop
         SAFE := Coherent.Delivery;
         for I in integer range 1 .. 2 loop -- Repetition matches data rate of legacy LCD displays.
            LEGACY := "*8" &
            SAFE.UTC(3..5)      & SAFE.UTC(7..8)      & SAFE.UTC(10..11)      & SAFE.UTC(13)     &
            SAFE.LST(3..5)      & SAFE.LST(7..8)      & SAFE.LST(10..11)      & SAFE.LST(13)     &
            SAFE.HA(3..5)       & SAFE.HA(7..8)       & SAFE.HA(10..11)       & SAFE.HA(13)      &
            SAFE.DEC(3..5)      & SAFE.DEC(7..8)      & SAFE.DEC(10..11)      & SAFE.DEC(13)     &
            SAFE.RA(3..5)       & SAFE.RA(7..8)       & SAFE.RA(10..11)       & SAFE.RA(13)      &
            SAFE.FOCUS(3..5)    & SAFE.FOCUS(7..8)    & SAFE.FOCUS(10..11)    & SAFE.FOCUS(13)   &
            "        "                                                                           &
            SAFE.UTC_DATE(4..5) & SAFE.UTC_DATE(7..8) & SAFE.UTC_DATE(10..11) & SAFE.UTC_DATE(13..14);

            begin
               TTY.Put(SERIAL_0, LEGACY);
               TTY.Flush(SERIAL_0);
            exception
               when others =>
               TTY.Close(SERIAL_0);
               TTY.Open(File => SERIAL_0, Mode => TTY.Out_File, Name => "/dev/ttyS0");
            end;

            begin
               TTY.Put(SERIAL_1, LEGACY);
               TTY.Flush(SERIAL_1);
            exception
               when others =>
               TTY.Close(SERIAL_1);
               TTY.Open(File => SERIAL_1, Mode => TTY.Out_File, Name => "/dev/ttyS1");
            end;
         end loop;
         delay(0.1);
      end loop;
   exception
      when others =>  Put("BYE SERIAL");
   end SERIAL_OUTPUT;

------------------------------------------------------------------------------------------------------------------------------------
-- Anonymous task for BEI interface 1 hosting HA and DEC. This is where the data are read and processed, and passed to the
-- protected Assembly to produce a consistent, coherent reply for the communication tasks.
---------------------------------------
   task HA_DEC is
      entry Process_Loop; -- No rendezvous, acquire data forever using coherent.Assembly to cache the data for coherent.Delivery.
   end HA_DEC;

   task body HA_DEC is
   -- Calendar, SLA_CLDJ
   NOW    : Time;
   YEAR   : Year_Number;
   MONTH  : Month_Number;
   DAY    : Day_Number;
   SEC    : Day_Duration;       -- ***DATA PRODUCT ***
   DJM    : Double_Precision;   -- Whole MJD.
   SLA_STATUS : Fortran_Integer;    -- SLA return code.

   -- SLA_GMST
   DUT            : Long_Float := 0.0;       -- UT1-UTC.
   UT1     : Double_Precision;  -- UTC + DUT.
   MJD_UT1 : Double_Precision;  -- MJD and fractional UT1.
   ST      : Double_Precision;  -- Sidereal Time.
   LST     : RADIAN;            -- *** DATA PRODUCT ***

   HA    : RADIAN;     -- *** DATA PRODUCT ***
   RA    : RADIAN;     -- *** DATA PRODUCT ***
   DEC   : RADIAN;     -- *** DATA PRODUCT ***
   TUBE  : CARDINAL;   -- *** DATA PRODUCT ***

   -- Returned from TTY.IO for BEI interface.
   POSITION   : integer; -- Dummy for when IO returns just an ACK or NACK.
   BEI_STATUS : BEI_CODE; -- Interface status.

   -- Paths to BEI interfaces hosting HA/DEC and FOCUS. These are /dev/ttyUSBnn devices with dynamically created links using the
   -- BEI serial number in /etc/udev/rules.d/99-usb-serial.rules
   HA_DEC_INTERFACE_PATH : string := "/dev/HA_DEC_INTERFACE";

   -- The encoder parameters and offset are compiled for reliability and readability. Recompile using the top command.
   -- HA encoder is currently 17-bit single turn.
   HA_ENCODER_BITS       : constant integer := 17;
   HA_ENCODER            : integer  range 0 .. 2**HA_ENCODER_BITS - 1;                    -- The raw HA encoder value.
   HA_ENCODER_OFFSET     : constant integer := 131066;                                    -- Set when installing encoder at HA 0.
   HA_SCALE              : constant RADIAN  := 1.0 * 2.0 * PI / long_float(2**HA_ENCODER_BITS); -- RADIANS.

   -- DEC encoder is currently 12-bit turns & 13-bits/turn for 25 bits. The number of turns must be > 180 and ideally 360.
   DEC_ENCODER_BITS      : constant integer := 25;
   DEC_TURN_BITS         : constant integer := 13;
   DEC_ENCODER           : integer  range 0 .. 2**DEC_ENCODER_BITS - 1;                       -- The raw DEC encoder value.
-- Original encoder offset until April 19 2022, i.e. just 1 count off from 0.
-- DEC_ENCODER_OFFSET    : constant integer := 33554431;                                      -- Set when installing encoder DEC 0.
-- After scratch balance on April 19 2022 we accidentally skipped a tooth on the 360T gear, i.e. 1 deg or 8192 counts. The result
-- was that DEC=-21 on the setting circle read DEC=-20 on the encoder.
   DEC_ENCODER_OFFSET    : constant integer :=  33554431 - 8191;                              -- Set when installing encoder DEC 0.
   DEC_SCALE             : constant RADIAN  := 1.0 * PI / 180.0 / long_float(2**DEC_TURN_BITS - 1); -- multi-turn is 2^13-1/turn.

   begin
      accept process_loop;
      -- Open BEI dual SSI-to-USB encoder interface 1. Channel 1 is HA, channel 2 is DEC. It is possible to open before the
      -- interface is ready for IO, causing the initialization to hang or fail. In this case the DOG_TIME will cause an abort
      -- when the socket attempts to get the data. An external watchdog program should monitor, notify, and attempt to restart.
      HA_DEC_INTERFACE.OpenTTY(HA_DEC_INTERFACE_PATH); 


      declare -- Send BEI initialization string for HA with parity.
      BIT_IMAGE : string := integer'Image(HA_ENCODER_BITS); -- string image of the integer with leading space.
      begin
         HA_DEC_INTERFACE.IO("$0L1" & BIT_IMAGE(2 .. BIT_IMAGE'Last) & "1", POSITION, BEI_STATUS); -- Remove Image leading space.
         if (BEI_STATUS = NACK) or (BEI_STATUS = INTERFACE_ERROR) then
            Put_Line("Interface 1 encoder 1 configuration error.");
            raise Constraint_Error; -- This is fatal.
         end if;
      end;

      declare -- Send BEI initialization string for DEC with parity.
      BIT_IMAGE : string := integer'Image(DEC_ENCODER_BITS); -- string image of the integer with leading space.
      begin
         HA_DEC_INTERFACE.IO("$0L2" & BIT_IMAGE(2 .. BIT_IMAGE'Last) & "1", POSITION, BEI_STATUS); -- Remove Image leading space.
         if (BEI_STATUS = NACK) or (BEI_STATUS = INTERFACE_ERROR) then
            Put_Line("Interface 1 encoder 2 configuration error.");
            raise Constraint_Error; -- This is fatal.
         end if;
      end;

      loop
         NOW := Clock; -- Get UTC. Make sure TZ is GMT. Delay to get encoder value is insignificant in this application.

         -- Acquire HA encoder.
         HA_DEC_INTERFACE.IO("$0R1", HA_ENCODER, BEI_STATUS); -- BEI command to ask for HA on channel 1 of interface 1. 
         if BEI_STATUS = DATUM then
            HA := long_float(HA_ENCODER - HA_ENCODER_OFFSET) * HA_SCALE;
            -- Acquire DEC encoder.
            HA_DEC_INTERFACE.IO("$0R2", DEC_ENCODER, BEI_STATUS); -- BEI command to ask for DEC on channel 2 of interface 1. 
            if BEI_STATUS = DATUM then
               DEC := MULTITURN_RANGE(ENCODER => DEC_ENCODER, OFFSET => DEC_ENCODER_OFFSET,
                                      BITS => DEC_ENCODER_BITS, SCALE => DEC_SCALE);
               if DEC > (PI / 2.0) then -- Nota Bene: OST is in the northern hemisphere, and the tube just crossed the north pole.
                  TUBE := WEST;
                  HA   := RADIAN(SLA_DRANGE(Double_Precision(HA + PI)));  -- and we're now looking out across the pole.
                  DEC  := RADIAN(SLA_DRANGE(Double_Precision(PI - DEC)));
               else
                  TUBE := EAST; -- Nota Bene: EAST is the default position for setting up the encoders.
                  HA   := RADIAN(SLA_DRANGE(Double_Precision(HA)));
                  DEC  := RADIAN(SLA_DRANGE(Double_Precision(DEC)));
               end if;

               -- Compute LST and RA.
               Split(NOW, YEAR, MONTH, DAY, SEC);
               SLA_CLDJ(Fortran_Integer(YEAR), Fortran_Integer(MONTH), Fortran_Integer(DAY), DJM, SLA_STATUS); -- Whole MJD.
               if SLA_STATUS > 0 then
                  Put_Line("SLA_CLDJ returned an error status code");
                  raise Constraint_Error;
               end if;

               begin -- Get UT1-UTC, PX, PY for this date.
               DUT := IERS_DATA(Integer(Long_Float(DJM) - IERS_DATA(0,IERS_MJD)), IERS_DUT);
               exception
                  when others => DUT := 0.0; -- Don't let the loss of IERS tabulated UT1 shutdown OST!
               end;

               UT1 := Double_Precision(SEC) + Double_Precision(DUT);                 -- SLA_GMST needs UT1 for LST.
               MJD_UT1 := DJM + (UT1 / 86400.0);
               ST  := SLA_GMST(MJD_UT1);                                             -- Sideral Time, Local Sideral Time next line.
               LST := RADIAN(SLA_DRANRM(ST + Double_Precision(OBS_LONGITUDE * PI / 180.0))); -- N.B. McD longitude west is negative.
               RA := RADIAN(SLA_DRANRM(Double_Precision(LST - HA)));

               -- Store in protected data so that asynchronous readers get synchronous coherent values with DATA.Delivery.
               -- If BEI_STATUS /= DATUM, then DOG_TIME will soon time out on delivery and kill the program.
               Coherent.Assembly(NOW   => NOW,
                                 YEAR  => YEAR,
                                 MONTH => MONTH,
                                 DAY   => DAY,
                                 UTC   => SEC,
                                 LST   => LST,
                                 HA    => HA,
                                 DEC   => DEC,
                                 RA    => RA,
                                 TUBE  => TUBE,
                                 HA_ENCODER  => HA_ENCODER,
                                 DEC_ENCODER => DEC_ENCODER);
            else
               Put_Line("DEC ERROR: " & BEI_CODE'Image(BEI_STATUS));
            end if;
         else
            Put_Line("HA ERROR: " & BEI_CODE'Image(BEI_STATUS));
         end if;
         delay(0.1); 
      end loop;
   end HA_DEC;

   task FOCUS_SPARE is
      entry Process_Loop;
   end FOCUS_SPARE;

   task body FOCUS_SPARE is -- a separate interface to BEI 2 for focus, so that a failure here won't stop BEI 1 Assembly.
   --UNCOMMENT WHEN SECOND BEI INTERFACE IS USED.
   FOCUS_SPARE_INTERFACE_PATH  : string := "/dev/FOCUS_SPARE_INTERFACE";

   -- The encoder parameters are compiled for reliability and readability. Recompile using the top command.
   FOCUS_ENCODER_BITS : constant integer := 17;                        -- FOCUS encoder is currently 17-bit single turn.
   FOCUS_ENCODER      : integer  range 0 .. 2**FOCUS_ENCODER_BITS - 1; -- The raw FOCUS encoder value.

   FOCUS : integer;       -- *** DATA PRODUCT ***
   SPARE : integer := -1; -- *** DATA PRODUCT ***

   -- Returned from TTY.IO for BEI interface.
   POSITION   : integer; -- Dummy for when IO returns just an ACK or NACK.
   BEI_STATUS : BEI_CODE;

   -- Freshness used in Coherent.Assembly.
   DOG_TIME   : Time := Clock;

   begin
   accept Process_Loop;
     -- Open BEI dual SSI-to-USB encoder interface 2. Channel 1 is FOCUS, channel 2 is currently unused.
      FOCUS_SPARE_INTERFACE.OpenTTY(FOCUS_SPARE_INTERFACE_PATH); 

      declare -- Send BEI initialization string for FOCUS with parity.
      BIT_IMAGE : string := integer'Image(FOCUS_ENCODER_BITS); -- string image of the integer with leading space.
      begin
         FOCUS_SPARE_INTERFACE.IO("$0L1" & BIT_IMAGE(2 .. BIT_IMAGE'Last) & "1", POSITION, BEI_STATUS); -- Remove leading space.
         if (BEI_STATUS = NACK) or (BEI_STATUS = INTERFACE_ERROR) then
            Put_Line("Interface 2 encoder 1 configuration error.");
         end if;
      end;

      loop
         -- Acquire FOCUS encoder - desirable but not critical.
         FOCUS_SPARE_INTERFACE.IO("$0R1", FOCUS_ENCODER, BEI_STATUS); -- BEI command to ask for FOCUS on channel 1 of interface 2. 
         if BEI_STATUS = DATUM then
            FOCUS := FOCUS_ENCODER;

            Coherent.Fill_Cache(DOG_TIME => Clock,
                                FOCUS    => FOCUS,
                                SPARE    => SPARE,
                                FOCUS_ENCODER => FOCUS_ENCODER,
                                SPARE_ENCODER => 0);
          else
             Put_Line("FOCUS ERROR: " & BEI_CODE'Image(BEI_STATUS));
          end if;
         delay(0.1);
      end loop;
   end FOCUS_SPARE;

------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------
-- Environment task.
------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------

   begin
   Set(Name => "TZ", Value => "GMT"); -- Set the GMT time zone.

   IERS; -- Read IERS data using ancillary.

   -- Start listening tasks.
   Task_Info.Initialize_Stack;        -- One task stack to rule them all.

   Ephemera_Server_Disconnect.Setup(IP => "192.168.30.19", Port => 22370);
   Ephemera_Server_Disconnect.Listen; -- Legacy support for ICEX, etcetera, one-shot request for writing FITS headers.

   Ephemera_Server_Keepalive.Setup(IP => "192.168.30.19", Port => 22371);
   Ephemera_Server_Keepalive.Listen;  -- Supports Track82, which keeps the socket open as sockets are designed to be.

   xEphemera_Server_Disconnect.Setup(IP => "198.214.229.60", Port => 22370);
   xEphemera_Server_Disconnect.Listen; -- Legacy support for ICEX, etcetera, one-shot request for writing FITS headers.

   xEphemera_Server_Keepalive.Setup(IP => "198.214.229.60", Port => 22371);
   xEphemera_Server_Keepalive.Listen;  -- Supports Track82, which keeps the socket open as sockets are designed to be.

   HA_DEC.Process_Loop;               -- Read the encoders, process the information, and submit for Coherent.Assembly.
   FOCUS_SPARE.Process_Loop;          -- Read the encoders and cache results using Coherent.Fill_Cache.

   SERIAL_OUTPUT.Commence;            -- Start serial service to legacy coordinate displays on both serial ports.

   exception
      when others => -- Catch the Constraint_Error raised when the /dev device can't be opened, and any other disaster.
         Put_Line("Environment task errors prevent telescopy from starting.");
         Flush;                       -- Let STDOUT escape the impending doom.
         GNAT.OS_Lib.OS_Exit(-1);
end telescopy;
