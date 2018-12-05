with Test_Util;

with Binary_To_Hex; use Binary_To_Hex;
with Uint_To_Binary; use Uint_To_Binary;
with Uintp; use Uintp;
with Ada.Text_IO;

package body Binary_To_Hex_Tests is

   package IO renames Ada.Text_IO;

   procedure Test_Suite;

   procedure Do_Test_Suite is
   begin
      Test_Util.Run_Test_Suite ("Binary to hex", Test_Suite'Access);
   end Do_Test_Suite;

   procedure Convert_0_Test;
   procedure Convert_2_Test;
   procedure Convert_17_Test;
   procedure Convert_138_Test;
   procedure Convert_48879_Test;

   procedure Test_Suite is
   begin
      Uintp.Initialize;
      Test_Util.Run_Test ("Converting 0", Convert_0_Test'Access);
      Test_Util.Run_Test ("Converting 2", Convert_2_Test'Access);
      Test_Util.Run_Test ("Converting 17", Convert_17_Test'Access);
      Test_Util.Run_Test ("Converting 138", Convert_138_Test'Access);
      Test_Util.Run_Test ("Converting 48879", Convert_48879_Test'Access);
   end Test_Suite;

   procedure Convert_0_Test is
      Binary : constant String := Convert_Uint_To_Binary (Uint_0, 32);
      Hex : constant String := Convert_Binary_To_Hex (Binary);
   begin
      pragma Assert (Hex = "00000000");
   end Convert_0_Test;

   procedure Convert_2_Test is
      Binary : constant String := Convert_Uint_To_Binary
        (Uint_2, 16);
      Hex : constant String := Convert_Binary_To_Hex (Binary);
   begin
      pragma Assert (Hex = "0002");
   end Convert_2_Test;

   procedure Convert_17_Test is
      Binary : constant String := Convert_Uint_To_Binary (Uint_1 + Uint_16, 8);
      Hex : constant String := Convert_Binary_To_Hex (Binary);
   begin
      pragma Assert (Hex = "11");
   end Convert_17_Test;

   procedure Convert_138_Test is
      Binary : constant String := Convert_Uint_To_Binary
        (Uint_128 + Uint_10, 8);
      Hex : constant String := Convert_Binary_To_Hex (Binary);
   begin
      pragma Assert (Hex = "8A");
   end Convert_138_Test;

   procedure Convert_48879_Test is
      Binary : constant String := Convert_Uint_To_Binary
        (UI_From_Int (48879), 16);
      Hex : constant String := Convert_Binary_To_Hex (Binary);
   begin
      pragma Assert (Hex = "BEEF");
   end Convert_48879_Test;

end Binary_To_Hex_Tests;
