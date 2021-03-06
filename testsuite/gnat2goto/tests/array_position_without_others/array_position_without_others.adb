procedure Array_Position_Without_Others is
   type My_Array_Type is array (1..5) of Integer;
   A : My_Array_Type := (1 => 1, 2 | 3 | 4 | 5 => 2);
--   B : My_Array_Type := (1 => 1, 3..4 => 3, 2 | 5 => 2);
begin
   pragma Assert(A(1) = 2);
end Array_Position_Without_Others;
