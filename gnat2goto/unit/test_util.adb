with Ada.Text_IO;
package body Test_Util is

   package IO renames Ada.Text_IO;

   Suite_Test_Count : Natural;
   Suite_Failed_Test_Count : Natural;

   Total_Test_Count : Natural := 0;
   Total_Failed_Test_Count : Natural := 0;

   procedure Run_Test (Name : String; Test : Unit_Test) is
   begin
      IO.Put ("Running test: " & Name);
      Suite_Test_Count := Suite_Test_Count + 1;
      Test.all;
      IO.Put_Line (" [Success]");
      exception
         when others =>
            Suite_Failed_Test_Count := Suite_Failed_Test_Count + 1;
            IO.Put_Line (" [Fail]");
   end Run_Test;

   procedure Run_Test_Suite (Name : String; Suite : Test_Suite) is
   begin
      Suite_Test_Count := 0;
      Suite_Failed_Test_Count := 0;

      IO.Put_Line
        ("##########################################################");
      IO.Put_Line ("# Running suite: " & Name);
      IO.Put_Line
        ("##########################################################");
      Suite.all;
      if Suite_Failed_Test_Count > 0 then
         IO.Put_Line ("Failed"
                        & Positive'Image (Suite_Failed_Test_Count)
                        & " out of"
                        & Positive'Image (Suite_Test_Count)
                        & " tests");
      else
         IO.Put_Line ("All"
                        & Positive'Image (Suite_Test_Count)
                        & " tests succeeded");
      end if;
      IO.Put_Line ("");

      Total_Test_Count := Total_Test_Count + Suite_Test_Count;
      Total_Failed_Test_Count := Total_Failed_Test_Count +
        Suite_Failed_Test_Count;
   end Run_Test_Suite;

   function Has_Test_Failures return Boolean is (Total_Failed_Test_Count > 0);

end Test_Util;
