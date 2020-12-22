package My_Intefaces is
   type Byte is range 0 .. 255;
   function Shift_Left (Value : Byte; N : Natural) return Byte
   is
     (if N = 0 then
         Value
      elsif Value >= 128 then
        (Value - 128) * 2
      else
         Value * 2);
end My_Intefaces;
