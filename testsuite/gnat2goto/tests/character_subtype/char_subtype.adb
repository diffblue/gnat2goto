procedure Char_Subtype is
   subtype Capitals is Character range 'A' .. 'Z';

   Var_C : Capitals;
begin
   Var_C := 'C';
   pragma Assert (Var_C in Capitals);
end Char_Subtype;
