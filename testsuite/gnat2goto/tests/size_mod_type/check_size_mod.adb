procedure Check_Size_Mod is
   S1 : constant := 39 - 7;
   S2 : constant Integer := -100 + 164;

   type Unsigned_8 is mod 2 ** 8;
   for Unsigned_8'Size use S1;

   type Unsigned_4 is mod 2 ** 4;
   for Unsigned_4'Size use S2;

   V1 : Unsigned_8;
   V2 : Unsigned_4;
begin
   V1 := 255;
   V2 := 15;

   pragma Assert (V1 + 1 = 0);
   pragma Assert (V2 + 2 = 1);
end Check_Size_Mod;
