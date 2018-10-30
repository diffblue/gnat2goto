with Test_Util;
package body Failing_Test is
   procedure Test_Suite;

   procedure Do_Test_Suite is
   begin
      Test_Util.Run_Test_Suite ("A failing test suite",
         Test_Suite'Access);
   end Do_Test_Suite;

   procedure Broken_Test;
   procedure Broken_Test is
   begin
      pragma Assert (False);
   end Broken_Test;

   procedure Test_Suite is
   begin
      Test_Util.Run_Test ("Broken", Broken_Test'Access);
   end Test_Suite;

end Failing_Test;
