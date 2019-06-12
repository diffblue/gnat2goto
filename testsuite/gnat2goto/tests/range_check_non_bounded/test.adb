procedure Test is
   type Uint_8 is mod 2**8;
   Uint_Var : Uint_8 := 5;
   Int_Var : Integer;
begin
   Int_Var := Integer'(Integer (Uint_Var));
   pragma Assert (Int_Var = 5);
end Test;
