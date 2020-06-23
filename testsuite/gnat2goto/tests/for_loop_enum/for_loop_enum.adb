procedure For_Loop_Enum is
   type Enum is (one, two, three, four, five, six);

   type A is array (Enum) of Integer;

   procedure Inc (E : Enum; N : in out Integer) is
   begin
      pragma Assert (E in one .. six);
      N := N + 1;
   end Inc;

   Arr : A;
   I : Integer := 1;

begin
   --  cbmc incorrectly fails this first range check.
   --  I have seen this several times in tests dependent on
   --  the structure of the code it succeeds or fails.
   Arr (one) := -1;

   for E in Enum loop
      Arr (E) := I;
      Inc (E, I);
   end loop;

   I := 1;
   for E in Enum loop
      pragma Assert (Arr (E) = I);
      Inc (E, I);
   end loop;

   I := 1;
   for E in reverse Enum loop
      Arr (E) := I;
      Inc (E, I);
   end loop;

   I := 1;
   for E in reverse Enum loop
      pragma Assert (Arr (E) = I);
      Inc (E, I);
   end loop;

end For_Loop_Enum;
