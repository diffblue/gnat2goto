with PragmaInitVars;
with PragmaInitNull;

package PragmaInit
is

   pragma Initializes ((NewVar1 => PragmaInitVars.Variable1));

   NewVar1 : Integer := PragmaInitVars.Variable1;
   NewVar2 : Integer := PragmaInitVars.Variable2;
   NewVar3 : Integer := PragmaInitVars.Variable3;

   NullVar1 : Integer := PragmaInitNull.Variable1;

   procedure Init;

end PragmaInit;
