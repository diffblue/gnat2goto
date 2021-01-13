with Bar;

procedure Foo
is
   X : Integer := 1;
   R : Integer;
begin
   Bar.Use_Add_One (X, R);

   pragma Assert (R = 2);
end Foo;
