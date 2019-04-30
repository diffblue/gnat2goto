package body Const_Decs is
   procedure Inc (P : in out Integer) is
   begin
      P := P + Read_Only_Var;
   end Inc;
   
end Const_Decs;
