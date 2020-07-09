procedure Nat_And_Pos is
   Var : Integer;
   function In_Type_Natural (N : Natural) return Boolean
     with Annotate => (ASVAT, In_Type_Function),
     Import;

   function In_Type_Positive (P : Positive) return Boolean
     with Annotate => (ASVAT, In_Type_Function),
     Import;
begin
   Var := -1;
   pragma Assert (In_Type_Natural (Var));
   pragma Assert (In_Type_Positive (Var));
   Var := 0;
   pragma Assert (In_Type_Natural (Var));
   pragma Assert (In_Type_Positive (Var));
   Var := 1;
   pragma Assert (In_Type_Natural (Var));
   pragma Assert (In_Type_Positive (Var));
   Var := Integer'Last;
   pragma Assert (In_Type_Natural (Var));
   pragma Assert (In_Type_Positive (Var));
end Nat_And_Pos;
