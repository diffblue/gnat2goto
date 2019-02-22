procedure LHS_Op_Or is
   A : Boolean := True;
   B : Boolean := True;
   C : Boolean;
begin
   C := A or B;
   pragma Assert (C);
end LHS_Op_Or;
