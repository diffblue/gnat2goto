function Statement_Block return Integer is
   Var1 : Integer :=1;
   Var2 : Integer;

   function Sum_Plus_Three(Left, Right : Integer) return Integer is
      Local1 : Integer;
   begin
      declare
         Local2 : Integer := 1;
      begin
         Local1 := Local2 + Var1 + 1;
         Pragma Assert (Local1=3);
      end;
   return (Left + Right + Local1);
   end Sum_Plus_Three;

begin
   ----------------------------------------
   --  the below nested function call works, but is out of scope
   --  and nested functions are not yet fully supported / tested
   ----------------------------------------
   --  Var1 := Sum_Plus_Three(3,4);
   --  pragma Assert (Var1=10);
   ----------------------------------------

   declare
      Var3 : Integer :=2;
   begin
      Var2 := Var3;
      pragma Assert (Var2=2);
      declare
         Var4 : Integer := Var3;
      begin
         Var2 := Var4 + Var2;
         Pragma Assert (Var2=4);
      end;
   end;

   declare
      Var5 : Integer := 6;
   begin
      Var2 := Var5 + Var2;
   end;

   pragma Assert (Var2=10);
   return Var2;
end Statement_Block;
