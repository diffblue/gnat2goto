procedure RHS_Array_Assign is
   type My_Array is array (1 .. 10) of Integer;

   A : My_Array;

begin
   A := My_Array'(others => 0);
   A (5) := 5;
   pragma Assert (A (5) = 5 and A (1) = 0 );
end RHS_Array_Assign;
