procedure For_Loop_Sub_Int_1 is
   subtype Small_Int is Integer range 1 .. 6;

   type A is array (Small_Int) of Integer;

   procedure Inc (E : Small_Int; N : in out Integer) is
   begin
      pragma Assert (E in Small_Int);
      N := N + 1;
   end Inc;

   Arr : A;
   I : Integer := 1;

begin
   for E in 1 .. 5 loop
      Arr (E) := I;
      Inc (E, I);
   end loop;

   I := 1;
   for E in 1 .. 5 loop
      pragma Assert (Arr (E) = I);
      Inc (E, I);
   end loop;

end For_Loop_Sub_Int_1;
