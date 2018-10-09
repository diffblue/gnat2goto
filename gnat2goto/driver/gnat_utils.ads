with Atree; use Atree;
with Sinfo; use Sinfo;
with Types; use Types;
with Namet; use Namet;

package GNAT_Utils is

   --  Utility routines temporarily copied from gnat2why, which perhaps should
   --  be moved to GNAT (or some other place to avoid duplicated effort, but we
   --  need to discuss this).
   --
   --  Intentially undocumented; see gnat2why originals for code
   --
   --  ??? fix license (Copyright AdaCore and Altran)

   generic
      with procedure Handle_Parameter (Formal : Entity_Id; Actual : Node_Id);
   procedure Iterate_Call_Parameters (Call : Node_Id)
     with Pre => Nkind (Call) in N_Subprogram_Call | N_Entry_Call_Statement;

   generic
      with procedure Handle_Arg
        (Arg_Pos : Positive; Arg_Name : Name_Id; Expr : Node_Id);
   procedure Iterate_Pragma_Parameters (The_Pragma : Node_Id)
     with Pre => Nkind (The_Pragma) in N_Pragma;

   function Get_Called_Entity (N : Node_Id) return Entity_Id;

   --  File_Name_Without_Suffix has been copied from
   --  gnat2why/spark/why/spark_util
   function File_Name_Without_Suffix (File_Name : String) return String;

end GNAT_Utils;
