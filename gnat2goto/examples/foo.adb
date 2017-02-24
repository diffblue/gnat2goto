procedure Foo (A, B : Integer;
               R    : out Integer)
is
begin
   R := A;
   R := R + B;
end Foo;
