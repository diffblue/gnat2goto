procedure Mixed_Parameters is
   C : Integer;

   function Subtract (Minuend : Integer; Subtrahend : Integer) return Integer is begin
      return Minuend - Subtrahend;
   end;

begin
   C := Subtract (2, 1);
   pragma Assert (C = 1);
   C := Subtract (Minuend => C, Subtrahend => 1);
   pragma Assert (C = 0);
   C := Subtract (Subtrahend => C, Minuend => 5);
   pragma Assert (C = 5);
   C := Subtract (7, Subtrahend => C);
   pragma Assert (C = 2);
   C := Subtract (C, C);
   pragma Assert (C /= 0);
end;
