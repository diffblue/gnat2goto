procedure Use_Import is
   procedure P (X : Integer);
   pragma Import (C, P);

   procedure Q (X : Integer);
   pragma Import (Convention => C, Entity => Q);

   function "-" (X : Integer) return Integer;
   pragma Import (Convention => Intrinsic, Entity => "-");

   function "+" (Left, Right : Integer) return Integer;
   pragma Import (Intrinsic, "+");

   I : Integer := 1;
begin
   I := -I;
end Use_Import;
