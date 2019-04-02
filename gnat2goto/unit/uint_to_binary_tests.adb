with Test_Util;
with Uintp; use Uintp;
with Uint_To_Binary; use Uint_To_Binary;

package body Uint_To_Binary_Tests is
   procedure Test_Suite;

   procedure Do_Test_Suite is
   begin
      Test_Util.Run_Test_Suite ("Uint to binary", Test_Suite'Access);
   end Do_Test_Suite;

   procedure Zero_Test;
   procedure Two_Test;
   procedure Five_Test;
   procedure Two_Hundred_And_Fifty_Five_Test;
   procedure Odd_Size_Test;

   procedure Minus_One_Test;
   procedure Minus_Five_Test;

   procedure Test_Suite is
   begin
      Test_Util.Run_Test ("Converting 0", Zero_Test'Access);
      Test_Util.Run_Test ("Converting 2", Two_Test'Access);
      Test_Util.Run_Test ("Converting 5", Five_Test'Access);
      Test_Util.Run_Test ("Converting 255",
                          Two_Hundred_And_Fifty_Five_Test'Access);
      Test_Util.Run_Test ("Converting odd sizes", Odd_Size_Test'Access);
      Test_Util.Run_Test ("Converting -1", Minus_One_Test'Access);
      Test_Util.Run_Test ("Converting -5", Minus_Five_Test'Access);
   end Test_Suite;

   procedure Zero_Test is
   begin
      pragma Assert (Convert_Uint_To_Binary (Uint_0, 8) =
                    "00000000");
   end Zero_Test;

   procedure Two_Test is
   begin
      pragma Assert (Convert_Uint_To_Binary (Uint_2, 8) =
                    "00000010");
   end Two_Test;

   procedure Five_Test is
   begin
      pragma Assert (Convert_Uint_To_Binary (Uint_5, 8) =
                    "00000101");
   end Five_Test;

   procedure Two_Hundred_And_Fifty_Five_Test is
      Uint_255 : constant Uint := Uint_2 ** 8 - Uint_1;
   begin
      pragma Assert (Convert_Uint_To_Binary (Uint_255, 8) =
                    "11111111");
   end Two_Hundred_And_Fifty_Five_Test;

   procedure Odd_Size_Test is
   begin
      pragma Assert (Convert_Uint_To_Binary (Uint_8, 5) =
                       "01000");
   end Odd_Size_Test;

   procedure Minus_One_Test is
   begin
      pragma Assert (Convert_Uint_To_Binary (Uint_Minus_1, 8) =
                       "11111111");
   end Minus_One_Test;

   procedure Minus_Five_Test is
   begin
      pragma Assert (Convert_Uint_To_Binary (Uint_Minus_5, 8) =
                    "11111011");
   end Minus_Five_Test;
end Uint_To_Binary_Tests;
