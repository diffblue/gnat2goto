procedure Character_Literal_Wide is
   A : Wide_Character := Wide_Character'('a');
   B : Wide_Character := Wide_Character'('b');
begin
   pragma Assert (A > B);
   pragma Assert (B > A);
end Character_Literal_Wide;
