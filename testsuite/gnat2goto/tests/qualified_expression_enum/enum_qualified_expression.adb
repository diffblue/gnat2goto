procedure Enum_Qualified_Expression is
   type Colour is (Red, Brown, Green, Blue, Yellow);
   subtype Colour_Blind is Colour range Red .. Green;
   
   C1, C2, C3 : Colour;
begin
   C1 := Brown; 
   C1 := Colour'Succ (C1);
   C2 := Colour_Blind'(C1);  -- Should succeed
   C2 := Colour'Succ (C2);
   C3 := Colour_Blind'(C2);  -- Should fail
   
end Enum_Qualified_Expression;
   
