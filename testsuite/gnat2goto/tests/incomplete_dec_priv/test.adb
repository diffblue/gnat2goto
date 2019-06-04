with Incomplete_Dec_Priv; use Incomplete_Dec_Priv;

procedure Test is
   My_Int : Integer := 4;
begin
   Q(My_Int);
   pragma Assert (My_Int = 5);
end Test;
