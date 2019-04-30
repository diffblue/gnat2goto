package body Private_Dec_Array is
   procedure P (X : in out Integer) is
      MyArray : Buffer_Element_Array;
   begin
      MyArray(1) := 5;
   end P;
end Private_Dec_Array;
