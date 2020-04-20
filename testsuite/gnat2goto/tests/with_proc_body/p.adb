with Inc, Inc_2;
procedure P is
   J : Integer  := 0;
begin
   Inc (J);
   pragma Assert (J = 1);
   Inc_2 (J);
   pragma Assert (J = 3);
end P;
