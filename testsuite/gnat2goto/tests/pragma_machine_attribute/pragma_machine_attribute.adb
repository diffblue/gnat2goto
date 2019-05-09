procedure Pragma_Machine_Attribute is

   procedure Timer_Interrupt;
   pragma Machine_Attribute (Timer_Interrupt, "signal");

   Seconds_Count : Integer := 0;
   Interval : Integer := 1;
   Timeout : Boolean := False;

   procedure Timer_Interrupt is
   begin
      Seconds_Count := Seconds_Count + 1;
      if Seconds_Count = Interval then
         Timeout := True;
      end if;
   end Timer_Interrupt;

begin
   Timer_Interrupt;

   pragma Assert (Timeout and Interval = Seconds_Count);

end Pragma_Machine_Attribute;
