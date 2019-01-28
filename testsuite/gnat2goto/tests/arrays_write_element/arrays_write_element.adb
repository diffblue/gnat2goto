
procedure Arrays_Write_Element is
   type Arr is array (1..3) of Integer;
   Actual : Arr := (1,2,3);
begin
   Actual(2) := 3;
end Arrays_Write_Element;
