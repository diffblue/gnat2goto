package Test_Util is
   type Unit_Test is access procedure;
   type Test_Suite is access procedure;

   --  Run a test. Succeeds if Test doesn't throw an exception
   procedure Run_Test (Name : String; Test : Unit_Test);

   --  Like Run_Test, but fail on success and succeed on failure
   procedure Expect_Failure (Name : String; Test : Unit_Test);

   --  Run a suite of tests.
   --  Suite should be a procedure that calls Run_Test
   --  or Expect_Failure for each test of the suite.
   procedure Run_Test_Suite (Name : String; Suite : Test_Suite);

   --  True if any tests in any suite failed
   function Has_Test_Failures return Boolean;

end Test_Util;
