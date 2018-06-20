with Test_Util;

with Ada.Command_Line;
with Ada.Text_IO;

procedure Unit_Tests is
begin
   if Test_Util.Has_Test_Failures then
      Ada.Command_Line.Set_Exit_Status (1);
      Ada.Text_IO.Put_Line ("Some tests failed");
   end if;
end Unit_Tests;
