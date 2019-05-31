with Private_Dec; use Private_Dec;

procedure Test is
   My_Int : Integer := 4;
begin
   Q(My_Int);
   pragma Assert (My_Int = 5);
end Test;
