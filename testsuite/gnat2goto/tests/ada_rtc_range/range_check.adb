procedure Range_Check is

   type t1 is range 0 .. 10;
   subtype t2 is t1 range 1 .. 8;
   
   type arr1 is array (t2) of t1;
   
   type arr2 is array (Integer range <>) of integer;
   
   
   var1 : t1 := 0;
   var2 : t1 := 1;
   var3 : t1;
   var4 : t2;

   var5 : arr1;
   
   var6 : arr2(1..10);
   var7 : arr2(1..5);
   
   
   function Get_Arr (arr : arr2) return arr2 is (arr);
begin


   --  test type out of range
   var3 := 10;

   pragma Assert (var3 = 10);
 
   -- test type out of range
   var3 := Var3 + var2;

   pragma Assert (var3 = 11);
   
   --  test subtype in range
   var4 := var2;

   pragma Assert (var4 = 1);
 
   -- test subtype out of range
   var4 := var1;

   pragma Assert (var4 = 0);

   -- test array in range
   var5 := (1 => 1, 
            2 => 2,
            3 => 3,
            4 => 4,
            5 => 5,
            6 => 6,
            7 => 7,
            8 => 8);
   
   pragma Assert (var5(1) = 1);
   pragma Assert (var5(8) = 8);
   
   -- test array index out of range
   var5(1) := 8;
   var5(8) := 1;
   var5(var3) := var3;

   pragma Assert (var5(1) = 8);
   pragma Assert (var5(8) = 1);

   var6 := (others => 6);
   
   var7 := (others => 7);
   
   -- should copy ok
   var6 := Get_Arr (Var7);
   
   pragma Assert (var6(1) = 7);
   pragma Assert (var6(6) = 6);
   
  var6 := (others => 6);
   
   var7 := (others => 7);
   
   -- should fail as var6 is too big
   var7 := Get_Arr (Var6);
   
   pragma Assert (var7(1) = 6);
   
end Range_Check;
