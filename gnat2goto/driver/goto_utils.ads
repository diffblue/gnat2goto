with Ireps;             use Ireps;
with Types;             use Types;
with Atree;             use Atree;
with Sinfo;             use Sinfo;
with Symbol_Table_Info; use Symbol_Table_Info;
with Uintp;                 use Uintp;

package GOTO_Utils is

   function CProver_Size_T return Irep;

   --  Utility routines for high-level GOTO AST construction

   Pointer_Type_Width : constant Positive := 64;
   --  ??? this should be queried at runtime from GNAT

   Synthetic_Variable_Counter : Positive := 1;

   function Fresh_Var_Name (Infix : String) return String;
   function Fresh_Var_Symbol_Expr (Ty : Irep; Infix : String) return Irep;

   function Make_Int_Type (Width : Positive) return Irep;

   function Make_Pointer_Type (Base : Irep) return Irep;

   function Make_Address_Of (Base : Irep) return Irep;

   function Param_Symbol (Param : Irep) return Irep
   with Pre  => Kind (Param) = I_Code_Parameter,
        Post => Kind (Param_Symbol'Result) = I_Symbol_Expr;

   function Symbol_Expr (Sym : Symbol) return Irep
   with Post => Kind (Symbol_Expr'Result) = I_Symbol_Expr;

   procedure New_Parameter_Symbol_Entry (Name_Id :               Symbol_Id;
                                         BaseName :              String;
                                         Symbol_Type :           Irep;
                                         A_Symbol_Table : in out Symbol_Table);

   function New_Function_Symbol_Entry (Name : String; Symbol_Type : Irep;
                                       Value : Irep;
                                       A_Symbol_Table : in out Symbol_Table)
                                       return Symbol;

   function Create_Fun_Parameter (Fun_Name : String; Param_Name : String;
                                  Param_Type : Irep; Param_List : Irep;
                                  A_Symbol_Table : in out Symbol_Table;
                                  Source_Location : Source_Ptr := No_Location)
                                  return Irep
     with Pre => (Kind (Param_Type) in Class_Type
                  and then Kind (Param_List) = I_Parameter_List),
     Post => Kind (Create_Fun_Parameter'Result) = I_Code_Parameter;

   function Compute_Memory_Op_Size (Num_Elem : Irep; Element_Type_Size : Uint;
                                    Index_Type : Irep;
                                    Source_Loc : Source_Ptr := No_Location)
                                    return Irep
     with Pre => (Kind (Num_Elem) in Class_Expr
                  and then Kind (Index_Type) in Class_Type),
       Post => Kind (Compute_Memory_Op_Size'Result) = I_Op_Mul;

   function Name_Has_Prefix (N : Node_Id; Prefix : String) return Boolean;

   function Has_GNAT2goto_Annotation
     (Def_Id : Entity_Id;
      Annot  : String) return Boolean
   with Pre => Nkind (Def_Id) = N_Defining_Identifier;
   --  checks whether an entity has a certain GNAT2goto annotation.
   --  This can be either an aspect, or a pragma.

end GOTO_Utils;
