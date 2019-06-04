procedure Test is
   type Enum_Type is (One, Two, Three, Four);
   type Array_Type is array (Enum_Type) of Integer;

   Array_Value : Array_Type := Array_Type'(One => 1, Two => 2, Three => 3, Four => 4);
begin
   pragma Assert (Array_Value(Two) = 2);
end Test;
