procedure Statement_Block is
   Var1 : Integer;
   Var3 : Integer;
begin
   Var1 := 1;
   declare
      Var2 : Integer;
   begin
      Var2 := 2;
      Var3 := Var2;
   end;
   Var3 := Var1;
end Statement_Block;
