procedure Attributes_Continuous is
   I: Float := 3.0;
begin
   pragma Assert (Float'Pred(I) < I);
   pragma Assert (Float'Succ(I) > I);
end Attributes_Continuous;
