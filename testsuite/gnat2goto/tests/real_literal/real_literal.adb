procedure Real_Literal is
   X : Float := 3.402823*(10.0**38);
   Y : Float := 3.412223*(10.0**33);
begin
   pragma Assert (X>0.0);
   pragma Assert (Y>0.0);
   pragma Assert (X>Y);
end Real_Literal;
