with Test_Util;
with Result;
package body Result_Tests is

   type Test_Error is (Err_1, Err_2);
   type Test_Ok is (Ok_1, Ok_2);

   package Test_Result is new Result (Ok_T => Test_Ok, Error_T => Test_Error);

   procedure Test_Suite;

   procedure Do_Test_Suite is
   begin
      Test_Util.Run_Test_Suite ("Result type tests", Test_Suite'Access);
   end Do_Test_Suite;

   procedure Make_Error_Is_Error_Test;
   procedure Make_Ok_Is_Ok_Test;

   procedure Test_Suite is
   begin
      Test_Util.Run_Test ("Can create and extract errors",
                          Make_Error_Is_Error_Test'Access);
      Test_Util.Run_Test ("Can create and extract ok values",
                          Make_Ok_Is_Ok_Test'Access);
   end Test_Suite;

   procedure Make_Error_Is_Error_Test is
      R : constant Test_Result.Result_T := Test_Result.Make_Error (Err_1);
   begin
      pragma Assert (Test_Result.Is_Error (R));
      pragma Assert (Test_Result.Get_Error (R) = Err_1);
   end Make_Error_Is_Error_Test;

   procedure Make_Ok_Is_Ok_Test is
      R : constant Test_Result.Result_T := Test_Result.Make_Ok (Ok_2);
   begin
      pragma Assert (Test_Result.Is_Ok (R));
      pragma Assert (Test_Result.Get_Ok (R) = Ok_2);
   end Make_Ok_Is_Ok_Test;

end Result_Tests;
