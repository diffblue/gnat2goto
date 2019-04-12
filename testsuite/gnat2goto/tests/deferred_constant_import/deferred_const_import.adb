package body Deferred_Const_Import is
   procedure Inc (P : in out Priv) is
   begin
      P := P + Imp_Const;
   end Inc;
   
 end Deferred_Const_Import;
