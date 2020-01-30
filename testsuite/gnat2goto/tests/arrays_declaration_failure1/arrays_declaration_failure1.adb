--  see also test "arrays_multidimensional_declaration_failure1"
procedure Arrays_Declaration_Failure1 is
   A1 : Array (1..3) of Integer;

   A2 : Array (1..3) of Integer := (1,2,3);
begin
   A1 (1) := 1;
   pragma Assert (A1 (1) = 1);
   pragma Assert (A1 (1) = 10);
   pragma Assert (A2 (1) = 1);
   pragma Assert (A2 (1) = 2);
end Arrays_Declaration_Failure1;
