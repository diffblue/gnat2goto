with External_Types; use type External_Types.New_Integer;

procedure Process_Statement is
   A : External_Types.New_Integer := 1;
   function My_Plus(Left, Right : External_Types.New_Integer) return External_Types.New_Integer renames "+";
begin
   A := A + 1;
   declare
      B : External_Types.New_Integer := A + 1;
   begin
      pragma Assert (B=3);
   end;
   pragma Assert (My_Plus(A,A)=2);
end Process_Statement;
