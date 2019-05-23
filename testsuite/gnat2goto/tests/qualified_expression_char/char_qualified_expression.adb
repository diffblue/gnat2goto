procedure Char_Qualified_Expression is
   subtype Capital is Character range 'A' .. 'Z';
   
   C1, C2, C3 : Character;
begin
   C1 := 'Y'; 
   C1 := Character'Succ (C1);
   C2 := Capital'(C1);  -- Should succeed
   C2 := Character'Succ (C2);
   C3 := Capital'(C2);  -- Should fail
   
end Char_Qualified_Expression;
   
