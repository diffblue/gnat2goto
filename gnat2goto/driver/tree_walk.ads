with Atree; use Atree;
with Sinfo; use Sinfo;
with Types; use Types;
with Irep_Schemata; use Irep_Schemata;
with Symbol_Table_Info; use Symbol_Table_Info;

package Tree_Walk is

   Global_Symbol_Table : Symbol_Table;
   function Do_Compilation_Unit (N : Node_Id) return Irep_Code_Block
   with Pre => Nkind (N) = N_Compilation_Unit;

end Tree_Walk;
