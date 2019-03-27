--  see also test "arrays_declaration_failure2"
procedure Arrays_Multidimensional_Declaration_Failure2 is
   --  ERROR, "GNAT BUG DETECTED" "GNU Ada (ada2goto) Assert_Failure sinfo.adb:2232"
   AA: array (Integer range 0..2, Integer range 0..3) of Integer;
begin
   null;
end Arrays_Multidimensional_Declaration_Failure2;
