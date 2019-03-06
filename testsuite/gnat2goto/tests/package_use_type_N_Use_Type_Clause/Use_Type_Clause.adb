with External_Types; use type External_Types.New_Integer;

procedure Use_Type_Clause is
   A : External_Types.New_Integer := 1;
begin
   A := A + 1;
   pragma Assert (A=2);
end Use_Type_Clause;
