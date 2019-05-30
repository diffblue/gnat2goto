with Ireps;             use Ireps;
with Types;             use Types;
with Atree;             use Atree;
with Sinfo;             use Sinfo;
with Symbol_Table_Info; use Symbol_Table_Info;
with Uintp;                 use Uintp;

package GOTO_Utils is

   type Irep_Array is array (Integer range <>) of Irep;

   function CProver_Size_T return Irep;

   --  Utility routines for high-level GOTO AST construction

   Pointer_Type_Width : constant Positive := 64;
   Size_T_Width : constant Int := 64;
   --  ??? this should be queried at runtime from GNAT

   Synthetic_Variable_Counter : Positive := 1;

   function Fresh_Var_Name (Infix : String) return String;
   function Fresh_Var_Symbol_Expr (Ty : Irep; Infix : String) return Irep;

   function Make_Int_Type (Width : Positive) return Irep;

   function Make_Signedint_Type (Width : Positive) return Irep;

   function Make_Signedbv_Type (Width : Positive) return Irep;

   function Make_Unsignedbv_Type (Width : Positive) return Irep;

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
                                         A_Symbol_Table : in out Symbol_Table)
     with Pre => Kind (Symbol_Type) in Class_Type;

   function New_Function_Symbol_Entry (Name : String; Symbol_Type : Irep;
                                       Value : Irep;
                                       A_Symbol_Table : in out Symbol_Table)
                                       return Symbol
     with Pre => (Kind (Symbol_Type) = I_Code_Type
                  and then (Kind (Value) = I_Code_Block
                    or else Value = Ireps.Empty));

   function Create_Fun_Parameter (Fun_Name : String; Param_Name : String;
                                  Param_Type : Irep; Param_List : Irep;
                                  A_Symbol_Table : in out Symbol_Table;
                                  Source_Location : Source_Ptr := No_Location)
                                  return Irep
     with Pre => (Kind (Param_Type) in Class_Type
                  and then Kind (Param_List) = I_Parameter_List),
     Post => Kind (Create_Fun_Parameter'Result) = I_Code_Parameter;

   function Compute_Memory_Op_Size (Num_Elem : Irep; Element_Type_Size : Uint;
                                    Source_Loc : Source_Ptr := No_Location)
                                    return Irep
     with Pre => Kind (Num_Elem) in Class_Expr,
     Post => (Kind (Compute_Memory_Op_Size'Result) = I_Op_Mul
              and then Get_Type (Compute_Memory_Op_Size'Result)
              = CProver_Size_T);

   function Build_Function (Name : String; RType : Irep; Func_Params : Irep;
                            FBody : Irep; A_Symbol_Table : in out Symbol_Table)
                            return Symbol
     with Pre => (Kind (RType) in Class_Type
                  and then Kind (Func_Params) = I_Parameter_List
                  and then Kind (FBody) in Class_Code);

   function Build_Array_Size (Array_Comp : Irep) return Irep
     with Pre => Kind (Array_Comp) in Class_Expr,
     Post => Kind (Build_Array_Size'Result) = I_Op_Add;

   function Build_Array_Size (First : Irep; Last : Irep)
                              return Irep
     with Pre => (Kind (First) in Class_Expr
                  and then Get_Type (First) = CProver_Size_T
                  and then Kind (Last) in Class_Expr
                  and then Get_Type (Last) = CProver_Size_T),
     Post => Kind (Build_Array_Size'Result) = I_Op_Add;

   function Typecast_If_Necessary (Expr : Irep; New_Type : Irep;
                                   A_Symbol_Table : Symbol_Table) return Irep
     with Pre => (Kind (Expr) in Class_Expr
                  and then Kind (New_Type) in Class_Type);

   function Offset_Array_Data (Base : Irep; Offset : Irep; Pointer_Type : Irep;
                               Source_Loc : Source_Ptr) return Irep
     with Pre => (Kind (Base) in Class_Expr
                  and then Kind (Offset) in Class_Expr
                  and then Kind (Pointer_Type) = I_Pointer_Type),
     Post => Get_Type (Offset_Array_Data'Result) = Pointer_Type;

   type Float_Format is (IEEE_32_Bit, IEEE_64_Bit);

   function To_Float_Format (Float_Type : Irep) return Float_Format
     with Pre => Kind (Float_Type) in I_Floatbv_Type | I_Bounded_Floatbv_Type;

   function Float_Mantissa_Size (Float_Type : Irep) return Integer;

   function Build_Index_Constant (Value : Int;
                                  Source_Loc : Source_Ptr) return Irep;

   function Name_Has_Prefix (N : Node_Id; Prefix : String) return Boolean;

   function Has_GNAT2goto_Annotation
     (Def_Id : Entity_Id;
      Annot  : String) return Boolean
   with Pre => Nkind (Def_Id) = N_Defining_Identifier;
   --  checks whether an entity has a certain GNAT2goto annotation.
   --  This can be either an aspect, or a pragma.

   function Integer_Constant_To_Expr
     (Value : Uint;
      Expr_Type : Irep;
      Source_Location : Source_Ptr)
   return Irep
   with Pre => Kind (Expr_Type) in Class_Bitvector_Type,
        Post => Kind (Integer_Constant_To_Expr'Result) = I_Constant_Expr;

   function Make_Simple_Side_Effect_Expr_Function_Call
     (Arguments : Irep_Array;
      Function_Expr : Irep;
      Source_Location : Source_Ptr) return Irep;

   procedure Register_Identifier_In_Symbol_Table
      (N : Irep; Val : Irep; Symtab : in out Symbol_Table)
      with Pre => Kind (N) = I_Symbol_Expr;

end GOTO_Utils;
