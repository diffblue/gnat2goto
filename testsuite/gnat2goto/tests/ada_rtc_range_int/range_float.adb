package body Range_Float is
   
   procedure Check_Float (val1 : float; val2 : float) is

   type t1 is digits 7 range 0.0 .. 10.0;
   
   var1 : t1 := 0.0;
   var2 : t1 := 1.0;
   var3 : t1;


begin

   --  test type in range
   var3 := t1 (val1);

   pragma Assert (var3 = 5.0);
 
   -- test type in range
   var3 := Var3 + t1 (val2);

   pragma Assert (var3 = 6.0);
   
   -- add expected failure
   pragma Assert (False);

   end Check_Float;
   
end Range_Float;
