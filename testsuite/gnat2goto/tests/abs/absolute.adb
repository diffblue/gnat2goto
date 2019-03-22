procedure Absolute is
   function Symmetric_Difference (A : Integer; B : Integer) return Integer is
      (abs (A - B));
   X : constant Integer := 10;
   Y : constant Integer := 15;
begin
   pragma Assert (Symmetric_Difference (X, Y) = Symmetric_Difference (Y, X));
   pragma Assert (Symmetric_Difference (X, Y) = 5);
   --  Intentional false assert
   pragma Assert (Symmetric_Difference (Y, X) /= 5);
end Absolute;
