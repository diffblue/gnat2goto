--  see also test  "arrays_multidimensional_declaration_failure2"
procedure Arrays_Declaration_Failure2 is
   A1 : array (Integer range 1..3) of Integer;
begin
   A1 (1) := 10;
   pragma Assert (A1 (1) = 10);
   pragma Assert (A1 (1) = 20);
end Arrays_Declaration_Failure2;
