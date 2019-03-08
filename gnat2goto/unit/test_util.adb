with Ada.Text_IO;
package body Test_Util is

   package IO renames Ada.Text_IO;

   Unexpected_Success : exception;

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

   procedure Expect_Failure (Name : String; Test : Unit_Test) is
      procedure Expect_Failure_Test;
      procedure Expect_Failure_Test is
      begin
         begin
            Test.all;
         exception
            when others =>
               return;
         end;
         raise Unexpected_Success;
      end Expect_Failure_Test;
   begin
      --  Unrestricted Access is a GNAT extension
      --  that allows us to access things that
      --  are more deeply nested than the access type
      --  itself
      Run_Test ("[Expect Failure] " & Name,
                Expect_Failure_Test'Unrestricted_Access);
   end Expect_Failure;

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
