procedure Records is
  type X is record
    A : Integer;
    B : Integer;
  end record;
  C : constant Integer := 1;
  D : X;
begin
  D.A := C;
  D.B := D.A + C;
  pragma Assert (d.a = 1);
  pragma Assert (d.b = 2);
end;
