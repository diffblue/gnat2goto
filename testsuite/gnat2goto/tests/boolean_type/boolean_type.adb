procedure Boolean_Type is
  A : Boolean := True;
  B : Boolean := False;
begin
  pragma Assert (A);
  pragma Assert (B);
  pragma Assert (True);
  pragma Assert (False);
end Boolean_Type;
