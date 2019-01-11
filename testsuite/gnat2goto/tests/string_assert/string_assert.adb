procedure string_assert is
   A : constant String := "Hello";
begin
   pragma Assert (A'Length = 5);
end string_assert;
