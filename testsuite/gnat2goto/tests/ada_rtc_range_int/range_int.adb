package body Range_Int is
   
   procedure Check_Int (val1 : Integer; val2 : Integer) is

      type t1 is range 0 .. 10;
      subtype t2 is t1 range 1 .. 8;
   
      var1 : t1 := 0;
      var2 : t1 := 1;
      var3 : t1;
      var4 : t2;

   begin

      --  test type in range
      var3 := t1 (Val1);

      pragma Assert (var3 = 5);
 
      -- test type in range
      var3 := Var3 + var2;

      pragma Assert (var3 = 6);
   
      --  test subtype in range
      var4 := t1 (val2);

      pragma Assert (var4 = 1);
 
      -- add expected failure
      pragma Assert (False);

      -- This will fail
      var4 := var2 - t1 (val2);

   end Check_Int;
   
end Range_Int;
