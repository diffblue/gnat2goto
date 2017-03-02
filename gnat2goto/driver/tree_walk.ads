with Atree; use Atree;
with Sinfo; use Sinfo;
with Types; use Types;

package Tree_Walk is

   type IRep is null record;

   Dummy : IRep;

   function Do_Compilation_Unit (N  : Node_Id) return IRep
   with Pre => Nkind (N) = N_Compilation_Unit;

end Tree_Walk;
