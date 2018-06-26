with Ada.Text_IO;
procedure Test_Long_Integer is
  x: Long_Long_Integer := 1152921504606846976;
begin
  pragma Assert (x > 100000);
end Test_Long_Integer;
