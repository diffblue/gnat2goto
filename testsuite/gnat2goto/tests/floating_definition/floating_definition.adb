procedure Floating_Definition is
   type Coefficient is digits 10 range -1.0 .. 2.0;
   type Unbounded is digits 8;

   Small_Number : Coefficient := 0.5;
   Large_Number : Unbounded := 22.0;
begin
   Large_Number := Large_Number + 0.7;
   Small_Number := Small_Number * 2.0;
   pragma Assert (Small_Number > 0.9);
   pragma Assert (Large_Number > 22.7);
end Floating_Definition;
