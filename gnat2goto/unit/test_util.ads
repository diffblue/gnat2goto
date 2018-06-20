package Test_Util is
   type Unit_Test is access procedure;
   type Test_Suite is access procedure;

   procedure Run_Test (Name : String; Test : Unit_Test);
   procedure Run_Test_Suite (Name : String; Suite : Test_Suite);

   function Has_Test_Failures return Boolean;
end Test_Util;
