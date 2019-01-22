procedure Ranges is
   type Int_Big is range 1 .. 10000000;
   type Int_Small is range -8 .. 8;
   A : Int_Big := 5;
   B : Int_Big := 900000;
   C : Int_Small;
begin
   C := Int_Small (A);
   C := Int_Small (B);
   C := -2;
   A := Int_Big (C);
end;
