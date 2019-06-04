package body Const_Decs is
   procedure Add_46 (P : in out Integer) is
   begin
      P := P + Read_Only_Var;
   end Add_46;

end Const_Decs;
