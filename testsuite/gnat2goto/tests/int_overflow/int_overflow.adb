procedure Int_Overflow is
  X : Integer := Integer'Last;
  Y : Integer := 1;
begin
  X := X + Y;
end;
