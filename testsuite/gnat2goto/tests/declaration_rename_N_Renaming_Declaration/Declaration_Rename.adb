procedure Declaration_Rename is
   A : Integer := 1;
   B : Integer;
   function My_Plus(Left, Right : Integer) return Integer renames "+";
begin
   A := A + 1;
   B := A + 1;
   pragma Assert (B=3);
   pragma Assert (My_Plus(A,B)=5);
end Declaration_Rename;
