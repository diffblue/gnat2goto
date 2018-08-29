procedure Floating_Point_Literals is
  X : Float := 1.0;
  Y : Float := 1.0;
  Z : Float := 3.0;
begin
  pragma Assert (X > Z);
  pragma Assert (X < Z);
  pragma Assert (X + Y = Z);
  pragma Assert (X + Y < Z);
end Floating_Point_Literals;
