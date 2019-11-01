procedure Minus is
   Large_Positive : Integer := Integer'Last;
   Large_Negative : Integer;

   function Negate (Num : Integer) return Integer
   is
   begin
      -- this will generate overflow check (which will pass)
      return -Num;
   end;

begin
   Large_Negative := Negate(Large_Positive);
   -- the following is reachable, thus failing
   pragma Assert (Large_Negative = 2);
end Minus;
