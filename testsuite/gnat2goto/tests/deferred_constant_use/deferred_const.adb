package body Deferred_Const is
   procedure Inc (P : in out Priv) is
   begin
      P := P + Def_Const;
   end Inc;
   
end Deferred_Const;

