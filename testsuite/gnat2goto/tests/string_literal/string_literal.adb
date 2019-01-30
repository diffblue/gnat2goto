procedure string_literal is
   A : constant String := "Hello";
   B : constant String := "Hello";
   C : constant String := "Goodbye";
begin
   pragma Assert(A = B);
   pragma Assert(A /= C);
end string_literal;
