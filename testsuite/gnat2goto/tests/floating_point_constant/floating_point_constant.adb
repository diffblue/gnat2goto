--  This test is testing the floating point constants
procedure Floating_Point_Constant is
  F : Float := 10.0;
  --  the function here is needed because for
  --  the second assert the constant gets simplified
  --  into True that doesn't have a value at the moment.
  function Haha return Integer is
   (1);
begin
  if F < 5.0 then
    pragma Assert (False);
  end if;

  if F > 6.0 then
    pragma Assert (1 = Haha);
  end if;
end Floating_Point_Constant;
