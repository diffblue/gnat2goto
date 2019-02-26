package Private_Type_Decl is
   
   type MyInteger is private;
   procedure Foo;
   
private
   type MyInteger is range 0..10;
end Private_Type_Decl;
