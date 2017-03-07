with Atree; use Atree;
with Sinfo; use Sinfo;
with Types; use Types;
with Irep_Schemata; use Irep_Schemata;

package Tree_Walk is

   function Do_Compilation_Unit (N  : Node_Id) return Irep_Code_Block
   with Pre => Nkind (N) = N_Compilation_Unit;

end Tree_Walk;
