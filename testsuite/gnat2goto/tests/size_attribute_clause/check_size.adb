procedure Check_Size is
   Size_1 : constant := -23 + 31;
   Size_2 : constant Integer := 35 - 3;
   Size_3 : constant := 2 * Size_1;

   type T_1 is new Integer range 0 .. 47;
   for T_1'Size use Size_1;

   type T_2 is range 0 .. 2**16 + 1;
   for T_2'Size use Size_2;

   type Unsigned_8 is mod 2**8;
   for Unsigned_8'Size use Size_3;

   Var_T_1 : T_1;
   Var_T_2 : T_2;

   Var_Size_1 : Integer range 0 .. 47;
   for Var_Size_1'Size use Size_1;

   Var_Size_2 : Integer range 0 .. 2**16 + 1;
   for Var_Size_2'Size use Size_2;

   Var_U8 : Unsigned_8;
   for Var_U8'Size use Size_1;

begin
   Var_T_1 := 30;
   Var_T_2 := 40;
   pragma Assert (Integer (Var_T_1) + Integer (Var_T_2) = 70);

   Var_Size_1 := 10;
   Var_Size_2 := 20;
   pragma Assert (Var_Size_1 + Var_Size_2 = 30);

   Var_U8 := 255;
   pragma Assert (Var_U8 + 1 = 0);
end Check_Size;
