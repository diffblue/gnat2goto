procedure For_Loop_Sub_Int is
   subtype Small_Int is Integer range 1 .. 6;
   subtype Smaller is Small_Int range 1 .. 5;

   type A is array (Small_Int) of Integer;

   procedure Inc (E : Small_Int; N : in out Integer) is
   begin
      N := N + 1;
   end Inc;

   Arr : A;
   I : Integer := 1;

begin
   for E in Small_Int loop
      Arr (E) := I;
      Inc (E, I);
   end loop;

   I := 1;
   for E in Small_Int loop
      pragma Assert (Arr (E) = I);
      Inc (E, I);
   end loop;

end For_Loop_Sub_Int;
