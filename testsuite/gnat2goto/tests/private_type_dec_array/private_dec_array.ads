package Private_Dec_Array is
   type Buffer_Element_Array is private;
   procedure P (X : in out Integer);
private
   type Buffer_Element_Array is array (1..10) of Integer;
end Private_Dec_Array;
