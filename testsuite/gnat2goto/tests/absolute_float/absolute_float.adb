procedure Absolute_Float is
  function Symmetric_Difference (A : Float; B : Float) return Float is
     (abs (A - B));
  X : constant Float := 10.0;
  Y : constant Float := 15.0;
begin
   pragma Assert (Symmetric_Difference (X, Y) = Symmetric_Difference (Y, X));
   pragma Assert (Symmetric_Difference (X, Y) = 5.0);
   --  Intentional false assert
   pragma Assert (Symmetric_Difference (Y, X) /= 5.0);
end Absolute_Float;
