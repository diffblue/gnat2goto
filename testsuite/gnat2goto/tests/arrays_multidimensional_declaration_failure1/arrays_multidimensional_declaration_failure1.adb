--  see also test "arrays_declaration_failure1"
procedure Arrays_Multidimensional_Declaration_Failure1 is
   --  ERROR, "GNAT BUG DETECTED" "GNU Ada (ada2goto) Constraint_Error Symbol_Table_Info.Symbol_Maps.Constant_Reference: key not in map|"
   AA1: array (0..2, 0..3) of Integer;

   --  ERROR, "GNAT BUG DETECTED" "GNU Ada (ada2goto) Constraint_Error Symbol_Table_Info.Symbol_Maps.Constant_Reference: key not in map|"
   AA2: array (Integer range 0..1, Integer range 0..1) of Integer := ((0,1),(2,3));
begin
   null;
end Arrays_Multidimensional_Declaration_Failure1;
