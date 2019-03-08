procedure Declaration_Rename is
   A : Integer := 1;
   B : Integer;
   New_Integer_With_Complicated_Name : Integer := 11;
   Simply_Named_Int : Integer renames New_Integer_With_Complicated_Name;
   function My_Plus(Left, Right : Integer) return Integer renames "+";
begin
   A := A + 1;
   B := A + 1;
   pragma Assert (B=3);
   pragma Assert (Simply_Named_Int=11);
   pragma Assert (My_Plus(A,B)=5);
end Declaration_Rename;
