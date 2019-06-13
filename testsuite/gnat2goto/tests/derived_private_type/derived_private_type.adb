with Private_Type; use Private_Type;
procedure Derived_Private_Type is
   type Derived_Priv is new Priv;
   V_Priv : Priv := Priv_Const;
   V_D_Priv : Derived_Priv := Derived_Priv (Priv_Const);
begin
   pragma Assert (Derived_Priv (V_Priv) = V_D_Priv);
   null;
end Derived_Private_Type;
