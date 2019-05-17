package Base is
   function Inc (Var : Integer) return Integer;
   pragma Export
  (Convention    => C,
   Entity        => Inc,
   External_Name => "Base_Inc" );
end Base;
