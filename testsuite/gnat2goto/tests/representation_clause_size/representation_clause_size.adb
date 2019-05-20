procedure Representation_Clause_Size is
   type Unsigned_8 is mod 2 ** 8;
   for Unsigned_8'Size use 16;

   Var : Unsigned_8 := 255;
begin
   pragma Assert (Var + 1 = 0);
end Representation_Clause_Size;
