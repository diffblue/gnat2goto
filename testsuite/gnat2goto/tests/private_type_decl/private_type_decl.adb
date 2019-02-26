package body Private_Type_Decl is

procedure Foo
is
   Five : MyInteger := 5;

begin
   pragma Assert (Integer (Five) > 0);
end Foo;

end Private_Type_Decl;
