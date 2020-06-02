with My_Pack;
procedure Main is
   I : Integer := 3;
begin
   pragma Assert (My_Pack.G (I) = 3);
end Main;
