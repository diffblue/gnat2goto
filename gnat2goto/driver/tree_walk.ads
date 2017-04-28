with Atree;             use Atree;
with Sinfo;             use Sinfo;
with Types;             use Types;
with Symbol_Table_Info; use Symbol_Table_Info;
with Ada.Containers.Ordered_Maps;

with Ireps;                 use Ireps;

package Tree_Walk is

   Global_Symbol_Table : Symbol_Table;
   Case_Binder_Count : Positive := 1;
   Anonymous_Type_Count : Positive := 1;

   --  This maps syntax tree node-ids onto CBMC symbol Ireps
   --  Presently:
   --    an N_Variant_Part maps onto a union type with
   --      members for each variant
   --    an N_Variant maps onto a structure type for a particular
   --      variant.
   --  The values are all I_Symbol_Type.
   package Anonymous_Type_Maps
   is new Ada.Containers.Ordered_Maps
     (Element_Type => Irep,
      Key_Type => Node_Id);

   Anonymous_Type_Map : Anonymous_Type_Maps.Map;

   Check_Function_Symbol : Irep := Ireps.Empty;

   function Do_Compilation_Unit (N : Node_Id) return Symbol
   with Pre => Nkind (N) = N_Compilation_Unit;

end Tree_Walk;
