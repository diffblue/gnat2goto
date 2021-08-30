--  see also test "arrays_declaration_failure1"
--  This subprogram no longer causes a constrait error
procedure Arrays_Multidimensional_Declaration_Failure1 is
   AA1: array (0..2, 0..3) of Integer;

   --  Raises unsupported feature - multi-dimensional aggregates.
   AA2: array (Integer range 0..1, Integer range 0..1) of Integer := ((0,1),(2,3));
begin
   null;
end Arrays_Multidimensional_Declaration_Failure1;
