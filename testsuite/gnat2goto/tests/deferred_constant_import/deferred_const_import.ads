package Deferred_Const_Import is
   type Priv is private;
   
   Imp_Const : constant Priv;
   
   procedure Inc (P : in out Priv);
   
private
   type Priv is range 1 .. 10;
   pragma Import (C, Imp_Const);

end Deferred_Const_Import;
