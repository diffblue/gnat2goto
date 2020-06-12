procedure Record_Aggregates is
   type Subsystem is (Example_Subsystem,
		      Health_Monitor,
		      Battery_Controller,
		      IO_Processor,
		      Primary_Sensors,
		      Secondary_Sensors);

   type Topics is (Outside_Temperature,
                   Air_Pressure,
                   Current_Speed,
                   Current_Altitude);

   type Message_Header is record
      Sender       : Subsystem;
      Topic        : Topics;
      Content_Size : Natural;  -- The amount of data in the message (in bits)
   end record;

   type Temp_Type   is range -40 .. 200;
   type Press_Type  is range -100 .. 100;
   type Speed_Type  is range 0 .. 1000;
   type Alt_Type    is range -100 .. 100_000;

   type Temperature_Message is record
      Header      : Message_Header;
      Temperature : Temp_Type;
   end record;
   pragma Pack (Temperature_Message);
   for Temperature_Message'Size use 88;

   type Pressure_Message is record
      Header      : Message_Header;
      Pressure    : Press_Type;
   end record;
   pragma Pack (Pressure_Message);
   for Pressure_Message'Size use 80;

   type Speed_Message is record
      Header      : Message_Header;
      Speed       : Speed_Type;
   end record;
   pragma Pack (Speed_Message);
   for Speed_Message'Size use 88;

   type Altitude_Message is record
      Header      : Message_Header;
      Altitude    : Alt_Type;
   end record;
   pragma Pack (Altitude_Message);
   for Altitude_Message'Size use 96;

   type Message (Topic : Topics := Outside_Temperature) is record
      case Topic is
         when Outside_Temperature => Temp  : Temperature_Message;
         when Air_Pressure        => Press : Pressure_Message;
         when Current_Speed       => Speed : Speed_Message;
         when Current_Altitude    => Alt   : Altitude_Message;
      end case;
   end record;

   procedure Fill_Message (Topic : Topics;
                           Filled_Message : out Message) is
      Header : constant Message_Header:=
      (Example_Subsystem, Topic, 100);
   begin
      case Topic is
         when Outside_Temperature =>
            Filled_Message := (Outside_Temperature,
                               (Header      => Header,
                                Temperature => 4));
          when Air_Pressure =>
            Filled_Message := (Air_Pressure,
                               (Header   => Header,
                                Pressure => 5));
          when Current_Speed =>
            Filled_Message := (Current_Speed,
                               (Header => Header,
                                Speed => 6));
          when Current_Altitude =>
            Filled_Message := (Current_Altitude,
                               (Header   => Header,
                                Altitude => 7));
      end case;
   end Fill_Message;

   M : Message;

begin
   Fill_Message (Outside_Temperature, M);
   pragma Assert (M.Temp.Temperature = 4);

   Fill_Message (Current_Speed, M);
   pragma Assert (M.Speed.Speed = 6);
end Record_Aggregates;
