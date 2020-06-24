procedure For_Loop_Int_1 is
   type Small_Int is range 1 .. 6;

   type A is array (Small_Int) of Integer;

   procedure Inc (E : Small_Int; N : in out Integer) is
   begin
      pragma Assert (E in Small_Int);
      N := N + 1;
   end Inc;

   Arr : A;
   I : Integer := 1;

begin
   for E in Small_Int range 1 .. 6 loop
      Arr (E) := I;
      Inc (E, I);
   end loop;

   I := 1;
   for E in Small_Int range 1 .. 6 loop
      pragma Assert (Arr (E) = I);
      Inc (E, I);
   end loop;

end For_Loop_Int_1;
