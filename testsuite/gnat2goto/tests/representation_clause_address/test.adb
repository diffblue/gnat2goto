with System;

procedure Test is
   -- A 32 bit hardware register
   type Unsigned_32 is mod 2 ** 32;
   Device_Input_Value: Unsigned_32;
   for Device_Input_Value'Address use System'To_Address (16#8000_05C4#);
begin
   Device_Input_Value := 5;
   pragma Assert (Device_Input_Value + 1 = 6);
end Test;
