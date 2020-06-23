procedure For_Loop_Char is
   subtype Small_char is Character range 'a' .. 'g';

   type A is array (Small_Char) of Integer;

   procedure Inc (E : Small_Char; N : in out Integer) is
   begin
      pragma Assert (E in Small_Char);
      N := N + 1;
   end Inc;

   Arr : A;
   I : Integer := 1;

begin
   for E in Small_Char loop
      Arr (E) := I;
      Inc (E, I);
   end loop;

   I := 1;
   for E in Small_Char loop
      pragma Assert (Arr (E) = I);
      Inc (E, I);
   end loop;

end For_Loop_Char;
