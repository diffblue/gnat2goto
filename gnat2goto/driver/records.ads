with Types;          use Types;
with Atree;          use Atree;
with Sinfo;          use Sinfo;
with Ireps;          use Ireps;
package Records is
   function Do_Aggregate_Literal_Record (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Aggregate,
        Post => Kind (Do_Aggregate_Literal_Record'Result) = I_Struct_Expr;

   procedure Do_Record_Object_Declaration
     (Block       : Irep;
      Dec_Node    : Node_Id;
      Target_Type : Entity_Id;
      Record_Name : String;
      Init_Expr   : Node_Id);

   function Do_Record_Type_Definition (N : Node_Id; Discs : List_Id)
                                       return Irep
   with Pre  => Nkind (N) in N_Record_Definition | N_Variant,
     Post => Kind (Do_Record_Type_Definition'Result) = I_Struct_Type;

   function Do_Selected_Component (N : Node_Id) return Irep
   with Pre  => Nkind (N) = N_Selected_Component,
        Post => Kind (Do_Selected_Component'Result) in
                I_Member_Expr | I_Op_Comma;

end Records;
