procedure Character_Literal is
   A : Character := 'a';
   B : Character := 'b';
begin
   pragma Assert (A > B);
   pragma Assert (B > A);
end Character_Literal;
