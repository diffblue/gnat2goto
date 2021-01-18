package body PragmaInit
is

   procedure Init is
   begin
      -- These asserts are expected to FAIL,
      -- but shows package is processed.
      pragma Assert (NewVar1 = PragmaInitVars.Variable1);
      pragma Assert (NewVar2 = PragmaInitVars.Variable2);
   end Init;

end PragmaInit;
