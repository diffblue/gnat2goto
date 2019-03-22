procedure Enum_Character_Literals is
  type My_Enum is ('a', 'b', 'c');
  First : My_Enum := 'a';
begin
  pragma assert (First = 'a');
  pragma assert (First /= 'b');
end Enum_Character_Literals;
