with Atree;             use Atree;
with Sinfo;             use Sinfo;
with Types;             use Types;
with Ireps;             use Ireps;
with Symbol_Table_Info; use Symbol_Table_Info;

package Tree_Walk is

   Global_Symbol_Table : Symbol_Table;

   function Do_Compilation_Unit (N : Node_Id) return Symbol
   with Pre => Nkind (N) = N_Compilation_Unit;

   function Make_Int_Type (Width : Positive) return Irep
   with Post => Kind (Make_Int_Type'Result) = I_Signedbv_Type;

end Tree_Walk;
