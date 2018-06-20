with Test_Util;

with Urealp; use Urealp;
with Uintp; use Uintp;

with Ureal_To_Binary; use Ureal_To_Binary;

package body Floating_Point_Tests is

   procedure Test_Integer;
   procedure Test_Between_0_And_1;
   procedure Test_General;

   procedure Test_Negative_Integer;
   procedure Test_Negative_Between_0_And_1;
   procedure Test_Negative_General;

   procedure Test_Suite;

   procedure Do_Test_Suite is
   begin
      Test_Util.Run_Test_Suite ("Floating point literals", Test_Suite'Access);
   end Do_Test_Suite;

   procedure Test_Integer is
      --  16/2 = 8
      Eight : constant Ureal := UR_From_Components (Uint_16, Uint_2);
      IEEE_Bits : constant String :=
        "0" -- Sign bit
        & "10000010" -- 130, i.e. exponent is 3 (+ 127)
        & "00000000" -- fraction is all 0, so 2^3 * 1.0b = 8.0
        & "00000000"
        & "0000000";
   begin
      pragma Assert (Convert_Ureal_To_Binary_IEEE (Eight) = IEEE_Bits);
   end Test_Integer;

   procedure Test_Between_0_And_1 is
      Zero_Point_Seven_Five : constant Ureal :=
        UR_From_Components (Uint_3, Uint_4);
      IEEE_Bits : constant String :=
        "0" -- Sign bit
        & "01111110" -- exponent 126 (-1 + 127)
        & "10000000" -- 1.1b * 2^-2 = 0.11b = 0.75
        & "00000000"
        & "0000000";
   begin
      pragma Assert (Convert_Ureal_To_Binary_IEEE (Zero_Point_Seven_Five)
                    = IEEE_Bits);
   end Test_Between_0_And_1;

   procedure Test_General is
      Three_Point_Five : constant Ureal :=
        UR_From_Components (Uint_14, Uint_4);
      IEEE_Bits : constant String :=
        "0" -- Sign bit
        & "10000000" -- exponent 128 (1 + 127)
        & "11000000" -- 1.11b * 2^1 = 11.1b = 3.5
        & "00000000"
        & "0000000";
   begin
      pragma Assert (Convert_Ureal_To_Binary_IEEE (Three_Point_Five)
                    = IEEE_Bits);
   end Test_General;

   procedure Test_Negative_Integer is
      Minus_Three : constant Ureal :=
        UR_From_Components (-Uint_12, Uint_4);
      IEEE_Bits : constant String :=
        "1" -- Sign bit
        & "10000000" -- exponent 128 (1 + 127)
        & "10000000" -- 1.1b * 2^1 = 11.0b = 3
        & "00000000"
        & "0000000";
   begin
      pragma Assert (Convert_Ureal_To_Binary_IEEE (Minus_Three)
                    = IEEE_Bits);
   end Test_Negative_Integer;

   procedure Test_Negative_Between_0_And_1 is
      Minus_Zero_Point_Five : constant Ureal :=
        UR_From_Components (-Uint_1, Uint_2);
      IEEE_Bits : constant String :=
        "1" -- Sign bit
        & "01111110" -- exponent 126 (-1 + 127)
        & "00000000" -- 1.0b * 2^-1 = 0.1b = 0.5
        & "00000000"
        & "0000000";
   begin
      pragma Assert (Convert_Ureal_To_Binary_IEEE (Minus_Zero_Point_Five)
                    = IEEE_Bits);
   end Test_Negative_Between_0_And_1;

   procedure Test_Negative_General is
      Minus_Forty_Point_Five : constant Ureal :=
        UR_From_Components (Uint_Minus_80 + Uint_Minus_5, Uint_2);
      IEEE_Bits : constant String :=
        "1" -- Sign bit
        & "10000100" -- exponent 132 (5 + 127)
        & "01010100" -- 1.010101b * 2^5 = 101010.1b = 42.5
        & "00000000"
        & "0000000";
   begin
      pragma Assert (Convert_Ureal_To_Binary_IEEE (Minus_Forty_Point_Five)
                       = IEEE_Bits);
   end Test_Negative_General;

   procedure Test_Suite is
   begin

      Uintp.Initialize;
      Urealp.Initialize;

      Test_Util.Run_Test ("Integer", Test_Integer'Access);
      Test_Util.Run_Test ("Between 0 and 1", Test_Between_0_And_1'Access);
      Test_Util.Run_Test ("General number", Test_General'Access);

      Test_Util.Run_Test ("Negative Integer", Test_Negative_Integer'Access);
      Test_Util.Run_Test ("Negative between 0 and 1",
                          Test_Negative_Between_0_And_1'Access);
      Test_Util.Run_Test ("General negative number",
                          Test_Negative_General'Access);
   end Test_Suite;

end Floating_Point_Tests;
