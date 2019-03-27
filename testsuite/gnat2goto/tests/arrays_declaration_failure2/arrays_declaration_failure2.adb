--  see also test  "arrays_multidimensional_declaration_failure2"
procedure Arrays_Declaration_Failure2 is
   --  ERROR, "GNAT BUG DETECTED" "GNU Ada (ada2goto) Assert_Failure sinfo.adb:2232"
   A1 : array (Integer range 1..3) of Integer;
begin
   null;
end Arrays_Declaration_Failure2;
