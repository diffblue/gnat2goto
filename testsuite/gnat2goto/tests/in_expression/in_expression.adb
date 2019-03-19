procedure In_Expression is
   subtype Range1 is Integer range 10 .. 20;
   type Arr3 is array (Range1) of Integer;
   Actual3 : Arr3 := (7, 8, 9, others => 10);
   Val : Integer := 10;
begin
   if Val in Actual3'Range then
      pragma Assert (Val > 1);
   else
      pragma Assert (Val <= 1);
   end if;
end In_Expression;
