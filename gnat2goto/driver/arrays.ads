with Atree;             use Atree;
with Snames;            use Snames;
with Einfo;             use Einfo;
with Sinfo;             use Sinfo;

with Ireps;             use Ireps;
with Types;             use Types;

with Symbol_Table_Info; use Symbol_Table_Info;
with GOTO_Utils;        use GOTO_Utils;

package Arrays is

   procedure Add_Array_Friends (Array_Name : String;
                                Array_Type : Entity_Id;
                                Param_List : Irep)
     with Pre => Is_Array_Type (Array_Type) and then
                 not Is_Constrained (Array_Type);
   --  For an unconstrained array parameter adds the array friend variables
   --  Array_Name___first_<Dimension> and Array_Name___last_<Dimension>
   --  to the symbol table and to the subprogram parameter list for each
   --  dimension of the array.

   function Do_Aggregate_Literal_Array (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_Aggregate;

   procedure Do_Array_Object (Object_Node     : Node_Id;
                              Object_Ada_Type : Entity_Id;
                              Block           : Irep;
                              Subtype_Irep    : out Irep)
     with Pre  => Is_Array_Type (Object_Ada_Type),
          Post => Kind (Subtype_Irep) = I_Array_Type;
   --  In goto an array is not a type, objects may be arrays.
   --  An anonymous subtype has to be declared for each
   --  array object describing its format.
   --  The array subtype and the friend variables,
   --  First and Last for the array object
   --  must be created and inserted into the symbol table.
   --  Do_Array_Object creates an array subtype and its friendly variables.
   --  The declarations and initialisations of the friends are
   --  added to the block.  The anonymous array subtype is
   --  returned by the Subtype_Irep parameter.

   function Do_Array_Subtype (Subtype_Node   : Node_Id;
                              Parent_Type    : Entity_Id;
                              Is_Constrained : Boolean;
                              First_Index    : Node_Id;
                              Block          : Irep) return Irep
     with Pre => Is_Array_Type (Parent_Type),
     Post => Kind (Do_Array_Subtype'Result) = I_Array_Type;
   --  Create an array subtype and its friendly variables.

   function Do_Constrained_Array_Definition (N     : Node_Id;
                                             Block : Irep) return Irep
     with Pre  => Nkind (N) in N_Array_Type_Definition,
     Post => Kind (Do_Constrained_Array_Definition'Result) = I_Array_Type;

   function Do_Unconstrained_Array_Definition (N     : Node_Id;
                                               Block : Irep) return Irep
     with Pre  => Nkind (N) in N_Array_Type_Definition,
     Post => Kind (Do_Unconstrained_Array_Definition'Result) =
     I_Array_Type;

   function Do_Array_Assignment (N : Node_Id) return Irep
     with Pre => Nkind (N) = N_Assignment_Statement,
     Post => Kind (Do_Array_Assignment'Result) = I_Code_Assign;

   function Do_Array_First_Last_Length (N : Node_Id; Attr : Attribute_Id)
                                        return Irep
     with Pre => Nkind (N) = N_Attribute_Reference and then
                 Attr in Attribute_First | Attribute_Last | Attribute_Length;

   function Do_Array_Length (N : Node_Id) return Irep
     with Pre => Nkind (N) = N_Attribute_Reference;

   function Do_Array_First (N : Node_Id) return Irep
     with Pre => Nkind (N) = N_Attribute_Reference;

   function Do_Array_Last (N : Node_Id) return Irep
     with Pre => Nkind (N) = N_Attribute_Reference;

   function Get_Array_Component_Type (N : Node_Id) return Entity_Id
     with Post => Is_Type (Get_Array_Component_Type'Result);

   function Do_Slice (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_Slice;

   function Do_Indexed_Component (N : Node_Id) return Irep
     with Pre  => Nkind (N) = N_Indexed_Component;

   function Get_Data_Component_From_Type (Array_Struct_Type : Irep)
                                          return Irep
     with Pre => Kind (Array_Struct_Type) in I_Struct_Type,
     Post => Kind (Get_Type (Get_Data_Component_From_Type'Result))
       in I_Pointer_Type;

   function Get_First_Index (Array_Struct : Irep) return Irep
     with Pre => (Kind (Array_Struct) in Class_Expr
                  and then Kind (Get_Type (Array_Struct)) in
                    I_Symbol_Type | I_Struct_Type),
     Post => Kind (Get_First_Index'Result) = I_Member_Expr;

   function Get_Last_Index (Array_Struct : Irep) return Irep
     with Pre => (Kind (Array_Struct) in Class_Expr
                  and then Kind (Get_Type (Array_Struct)) in
                    I_Symbol_Type | I_Struct_Type),
     Post => Kind (Get_Last_Index'Result) = I_Member_Expr;

   function Get_Data_Member (Array_Struct : Irep;
                             A_Symbol_Table : Symbol_Table)
                             return Irep
     with Pre => (Kind (Array_Struct) in Class_Expr
                  and then Kind (Get_Type (Array_Struct)) in
                    I_Symbol_Type | I_Struct_Type),
       Post => Kind (Get_Data_Member'Result) = I_Member_Expr;

   function Get_Non_Array_Component_Type (A : Entity_Id) return Entity_Id
     with Pre => Is_Array_Type (A);
   --  When presented with an array repeatedly obtains the component
   --  type of the array until the component type is not an array subtype.
   --  It returns this component type.

   function Offset_Array_Data (Base : Irep; Offset : Irep) return Irep
     with Pre => (Kind (Base) in Class_Expr
                  and then Kind (Offset) in Class_Expr),
     Post => Kind (Offset_Array_Data'Result) in Class_Expr;

   function Make_Array_Default_Initialiser (E : Entity_Id) return Irep;

   procedure Pass_Array_Friends (Actual_Array : Entity_Id;  Args : Irep)
     with Pre => Is_Array_Type (Etype (Actual_Array));

private

   function Do_RHS_Array_Assign (N : Node_Id) return Irep_Array
     with Pre => Nkind (N) in N_Subexpr;

   function Make_Array_First_Expr
     (Base_Type : Node_Id; Base_Irep : Irep) return Irep;

   function Build_Array_Size (First : Irep; Last : Irep; Idx_Type : Irep)
                              return Irep
     with Pre => (Kind (First) in Class_Expr
                  and Kind (Last) in Class_Expr
                  and Kind (Idx_Type) in Class_Type),
     Post => Kind (Build_Array_Size'Result) = I_Op_Add;

   function Get_First_Index_Component (Array_Struct : Irep)
                                       return Irep;

   function Get_Last_Index_Component (Array_Struct : Irep) return Irep;

   function Get_Data_Component (Array_Struct : Irep;
                                A_Symbol_Table : Symbol_Table) return Irep
     with Pre => (Kind (Array_Struct) in Class_Expr
                  and then Kind (Get_Type (Array_Struct)) in
                    I_Symbol_Type | I_Struct_Type),
     Post => Kind (Get_Type (Get_Data_Component'Result)) = I_Pointer_Type;

end Arrays;
