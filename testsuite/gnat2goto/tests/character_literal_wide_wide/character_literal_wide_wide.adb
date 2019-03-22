procedure Character_Literal_Wide_Wide is
   A : Wide_Wide_Character := Wide_Wide_Character'('a');
   B : Wide_Wide_Character := Wide_Wide_Character'('b');
begin
   pragma Assert (A > B);
   pragma Assert (B > A);
end Character_Literal_Wide_Wide;
