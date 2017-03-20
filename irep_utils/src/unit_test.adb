with Ireps; use Ireps;

procedure Unit_Test is
   A : Irep;
begin
   A := New_Irep (I_Op_Add);
   Set_Overflow_Check (A, True);

   --  Print_Irep (A);
end Unit_Test;
