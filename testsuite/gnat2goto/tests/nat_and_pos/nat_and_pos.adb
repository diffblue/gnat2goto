procedure Nat_And_Pos is
   Bool_1 : Boolean;
   Bool_2 : Boolean;
   Bool_3 : Boolean;
   Bool_4 : Boolean;
   Var : Integer;
   function In_Type_Natural (N : Natural) return Boolean
     with Annotate => (ASVAT, In_Type_Function),
     Import;

   function In_Type_Positive (P : Positive) return Boolean
     with Annotate => (ASVAT, In_Type_Function),
     Import;
begin
   Var := -1;
   if Bool_1 then
      pragma Assert (In_Type_Natural (Var));
      pragma Assert (In_Type_Positive (Var));
      pragma Assert (False);
   end if;
   Var := 0;
   if Bool_2 then
      pragma Assert (In_Type_Natural (Var));
      pragma Assert (In_Type_Positive (Var));
      pragma Assert (False);
   end if;
   Var := 1;
   if Bool_3 then
      pragma Assert (In_Type_Natural (Var));
      pragma Assert (In_Type_Positive (Var));
      pragma Assert (False);
   end if;
   Var := Integer'Last;
   if Bool_4 then
      pragma Assert (In_Type_Natural (Var));
      pragma Assert (In_Type_Positive (Var));
      pragma Assert (False);
   end if;
end Nat_And_Pos;
