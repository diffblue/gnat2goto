procedure Records is
  type X is record
    a : Integer;
    b : Integer;
  end record;
  c : constant Integer := 1;
  d : X;
begin
  d.a := c;
  d.b := d.a + c;
end;
