procedure Test is

  Val1 : Integer;
  Val2 : Integer;

begin

   Val1 := 5;
   Val2 := 11;

   pragma Assert (Integer'Max (Val1,Val2) = Val2);

   pragma Assert (Integer'Min (Val1,Val2) = Val1);  

   -- expect this one to fail
   pragma Assert (Integer'Max (Val1,Val2) = Val1);

end Test;