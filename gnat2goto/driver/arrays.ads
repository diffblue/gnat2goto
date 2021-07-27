with Atree;             use Atree;
with Snames;            use Snames;
with Einfo;             use Einfo;
with Sinfo;             use Sinfo;

with Ireps;             use Ireps;
with Types;             use Types;

with GOTO_Utils;        use GOTO_Utils;

package Arrays is

   function Is_Bounded_Array (Expr : Irep) return Boolean;

   procedure Add_Array_Friends (Array_Name : String;
                                Array_Type : Entity_Id;
                                Param_List : Irep)
     with Pre => (Is_Array_Type (Array_Type) and then
          not Is_Constrained (Array_Type)) and
          Kind (Param_List) = I_Parameter_List;
   --  For an unconstrained array parameter adds the array friend variables
   --  Array_Name___first_<Dimension> and Array_Name___last_<Dimension>
   --  to the subprogram parameter list for each dimension of the array.

   function All_Dimensions_Static (The_Array : Entity_Id) return Boolean
     with Pre => Is_Array_Type (The_Array);

   function Do_Aggregate_Literal_Array (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_Aggregate;

   procedure Do_Array_Assignment_Op (Block       : Irep;
                                     Destination : Irep;
                                     Dest_Type   : Entity_Id;
                                     Source_Expr : Node_Id);

   function Do_String_Literal (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_String_Literal;

   procedure Do_Array_Object_Declaration (Block       : Irep;
                                          Dec_Node    : Node_Id;
                                          Target_Type : Entity_Id;
                                          Array_Name  : String;
                                          Init_Expr   : Node_Id)

     with Pre => Nkind (Dec_Node) = N_Object_Declaration and
                 Is_Array_Type (Target_Type) and
                 (if Present (Init_Expr) then
                      Is_Array_Type (Etype (Init_Expr))
                    else
                      True);
   --  In goto an array is not a type, objects may be arrays.
   --  Array types are entered into the global symbol. Some of these may
   --  be anonomous types introduced by the gnat front-end.
   --  If the object is a constrained array type then the type entry from
   --  the symbol table is used to define the goto array object.
   --  If the object is an unconstrained array subtype, then its first, last
   --  and length attributes must be determined from its mandatory
   --  initialization.
   --  If the initialization is not constrained it will not have a constrained
   --  subtype in the global symbol table and cannot be used to define the
   --  object.  In such cases the first, last and length attributes object
   --  have to be determined directly from the initialization expression
   --  and are use to define a goto array object of the correct length.

   function Do_Array_Subtype (Subtype_Node : Node_Id;
                              The_Entity   : Entity_Id) return Irep
     with Pre => Is_Array_Type (The_Entity),
     Post => Kind (Do_Array_Subtype'Result) in I_Array_Type | I_Struct_Type;
   --  Create an array subtype.  If the array subtype is constrained
   --  but the constraint is not static a new variable is inserted into the
   --  symol table and its value set to the goto expression representing
   --  the length of the array.

   function Do_Constrained_Array_Definition (N     : Node_Id) return Irep
     with Pre  => Nkind (N) in N_Array_Type_Definition,
     Post => Kind (Do_Constrained_Array_Definition'Result) = I_Array_Type;

   function Do_Unconstrained_Array_Definition (N : Node_Id) return Irep
     with Pre  => Nkind (N) in N_Array_Type_Definition,
     Post => Kind (Do_Unconstrained_Array_Definition'Result) = I_Struct_Type;

   procedure Do_Array_Assignment (Block : Irep; N : Node_Id)
     with Pre => Nkind (N) = N_Assignment_Statement;

   function Do_Array_Concatination (N : Node_Id) return Irep
   with Pre  =>  Nkind (N) = N_Op_Concat;
--      Post => Is_Unconstrained_Array_Result (Do_Array_Concatination'Result);

   function Do_Array_First_Last_Length (N : Node_Id; Attr : Attribute_Id)
                                        return Irep
     with Pre => Nkind (N) = N_Attribute_Reference and then
                 Attr in Attribute_First | Attribute_Last | Attribute_Length;

   function Do_Slice (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_Slice;

   function Do_Indexed_Component (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_Indexed_Component;

   function Get_Array_Reference (Array_Irep : Irep; Component_Irep : Irep)
                                 return Irep;

   function Get_Non_Array_Component_Type (A : Entity_Id) return Entity_Id
     with Pre => Is_Array_Type (A);
   --  When presented with an array repeatedly obtains the component
   --  type of the array until the component type is not an array subtype.
   --  It returns this component type.

   procedure Pass_Array_Friends (Actual_Array : Entity_Id;
                                 Array_Irep   : Irep;
                                 Args         : Irep)
     with Pre => Is_Array_Type (Underlying_Type (Etype (Actual_Array)));

   procedure Build_Unconstrained_Array_Result (Block       : Irep;
                                               Result_Var  : Irep;
                                               Return_Expr : Node_Id);

   function Make_Bounded_Array_Type (Dimensions : Pos; Comp_Type : Irep)
                                     return Irep;

   function Make_Unconstrained_Array_Result (Result_Expr : Node_Id)
                                             return Irep;

   function Make_Static_Array (Size : Pos; Array_Type : Node_Id) return Irep
     with Pre => Is_Array_Type (Array_Type);

private

   function Do_RHS_Array_Assign (N : Node_Id) return Irep_Array
     with Pre => Nkind (N) in N_Subexpr;

end Arrays;
