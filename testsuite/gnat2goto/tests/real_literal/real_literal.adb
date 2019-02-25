procedure Real_Literal is
  X : Float := 16777215.0*(2.0**104);
begin
   pragma Assert (X>0.0);
end Real_Literal;
