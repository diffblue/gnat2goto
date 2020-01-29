--  see also test "arrays_multidimensional_declaration_failure1"
procedure Arrays_Declaration_Failure1 is
   --  ERROR, "GNAT BUG DETECTED" " GNU Ada (ada2goto) Constraint_Error Symbol_Table_Info.Symbol_Maps.Constant_Reference: key not in map|"
   A1 : Array (1..3) of Integer;

   --  ERROR, "GNAT BUG DETECTED" " GNU Ada (ada2goto) Constraint_Error Symbol_Table_Info.Symbol_Maps.Constant_Reference: key not in map|"
   --  A2 : Array (1..3) of Integer := (1,2,3);
begin
  A1 (1) := 1;
  pragma Assert (A1 (1) = 1);
  pragma Assert (A1 (1) = 10);
end Arrays_Declaration_Failure1;
