procedure Record_Extension is
   type R1 is tagged record
      A : Integer := 2;
      B : Boolean := False;
   end record;

   type R2 is new R1 with null record;
   Var : R1 := (A => 3, B => True);
begin
   pragma Assert (Var.B);
end Record_Extension;
