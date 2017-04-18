
procedure Primitive_Pointer is
   A : aliased Integer;
   B : access Integer := A'Access;
begin
   A := 5;
   B.all := B.all + 1;
   --pragma Assert (A = 6);
end;
