procedure Test is
   Storage_Unit : constant := 8;

   type Storage_Offset is range
     -(2 ** (Integer'(Standard'Address_Size) - 1)) ..
     +(2 ** (Integer'(Standard'Address_Size) - 1)) - Long_Long_Integer'(1);

   type Storage_Element is mod 2 ** Storage_Unit;

   type Storage_Array is
     array (Storage_Offset range <>) of aliased Storage_Element;
   for Storage_Array'Component_Size use Storage_Unit;

   My_Storage_Array : constant Storage_Array (1..5) := (1,2,3,4,5);
begin
   pragma Assert (My_Storage_Array(2)=2);
end Test;
