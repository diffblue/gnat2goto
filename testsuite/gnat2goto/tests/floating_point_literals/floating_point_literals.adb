procedure Floating_Point_Literals is
  X : Float := 1.0;
  Y : Float := 1.0;
  Z : Float := 3.0;
  W : Float := 1.1;
begin
  --  Supposed to fail (1 > 1)
  pragma Assert (X > Z);
  --  Supposed to succeed (1 < 3)
  pragma Assert (X < Z);
  --  Supposed to fail (2 = 3)
  pragma Assert (X + Y = Z);
  --  Supposed to succeed (2 < 3)
  pragma Assert (X + Y < Z);
  --  Supposed to succeed (1.1 < 1.2)
  pragma Assert (W < 1.2);
end Floating_Point_Literals;
