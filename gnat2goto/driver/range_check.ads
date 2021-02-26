with Ada.Containers.Vectors;    use Ada.Containers;

with Types;                     use Types;
with Uintp;                     use Uintp;
with Urealp;                    use Urealp;
with Ada.Containers.Ordered_Maps;

with Ireps;                     use Ireps;

package Range_Check is

   type Bound_Low_Or_High is (Bound_Low, Bound_High);

   type Bound_Type_Nat is new Uint;
   type Bound_Type_Real is new Ureal;
   type Bound_Type_Symbol is new Irep;

   function Store_Nat_Bound (Number : Bound_Type_Nat) return Integer;
   function Store_Real_Bound (Number : Bound_Type_Real) return Integer;
   function Store_Symbol_Bound (Number : Bound_Type_Symbol) return Integer;

   function Get_Bound (N : Node_Id; Bound_Type : Irep; Pos : Bound_Low_Or_High)
                             return Irep
     with Pre => Kind (Bound_Type) in
     I_Bounded_Unsignedbv_Type | I_Bounded_Signedbv_Type
       | I_Bounded_Floatbv_Type | I_Unsignedbv_Type | I_Signedbv_Type
         | I_Floatbv_Type | I_C_Enum_Type,
     Post => Kind (Get_Bound'Result) in Class_Expr;

   function Get_Bound_Of_Bounded_Type (Bound_Type : Irep;
                                       Pos : Bound_Low_Or_High) return Irep
     with Pre => Kind (Bound_Type) in
     I_Bounded_Signedbv_Type
       | I_Bounded_Floatbv_Type
       | I_Bounded_Unsignedbv_Type
       | I_C_Enum_Type,
       Post => Kind (Get_Bound_Of_Bounded_Type'Result) in Class_Expr;

   function Make_Div_Zero_Assert_Expr (N : Node_Id;
                                       Value : Irep;
                                       Divisor : Irep) return Irep;

   function Make_Index_Assert_Expr (N : Node_Id; Index : Irep;
                                    First_Index : Irep; Last_Index : Irep)
                                    return Irep;

   function Make_Overflow_Assert_Expr (N : Node_Id; Value : Irep) return Irep;

   function Make_Range_Assert_Expr (N : Node_Id; Value : Irep;
                                    Lower_Bound : Irep; Upper_Bound : Irep;
                                    Expected_Return_Type : Irep;
                                    Check_Name : String)
                                    return Irep;
   function Make_Range_Assert_Expr (N : Node_Id; Value : Irep;
                                    Bounds_Type : Irep) return Irep
     with Pre => Kind (Bounds_Type) in
     I_Bounded_Signedbv_Type | I_Bounded_Floatbv_Type | I_Symbol_Type
       | I_Unsignedbv_Type | I_Signedbv_Type | I_Bounded_Unsignedbv_Type
       | I_Floatbv_Type;

   function Make_Range_Expression (Value_Expr : Irep; Lower_Bound : Irep;
                                   Upper_Bound : Irep)
                                   return Irep
     with Pre => Kind (Get_Type (Lower_Bound)) = Kind (Get_Type (Lower_Bound)),
       Post => Kind (Make_Range_Expression'Result) in Class_Expr;

private
   type Bound_Type is (Nat_Bound, Real_Bound, Symb_Bound);

   package All_Bounds_Vector is new
     Ada.Containers.Vectors (Index_Type   => Natural,
                             Element_Type => Bound_Type);

   package Nat_Bounds_Map is new
     Ada.Containers.Ordered_Maps (Key_Type     => Natural,
                                  Element_Type => Bound_Type_Nat);
   package Real_Bounds_Map is new
     Ada.Containers.Ordered_Maps (Key_Type     => Natural,
                                  Element_Type => Bound_Type_Real);
   package Symbol_Bounds_Map is new
     Ada.Containers.Ordered_Maps (Key_Type     => Natural,
                                  Element_Type => Bound_Type_Symbol);

   All_Bounds_Table : All_Bounds_Vector.Vector;
   Nat_Bounds_Table : Nat_Bounds_Map.Map;
   Real_Bounds_Table : Real_Bounds_Map.Map;
   Expr_Bounds_Table : Symbol_Bounds_Map.Map;

   function Get_Bound_Type (Bound_Index : Integer) return Bound_Type
     with Pre => Integer (All_Bounds_Table.Length) >= Bound_Index;

   function Load_Nat_Bound_In_Hex (Index : Integer; Actual_Type : Irep)
                               return String
     with Pre => Kind (Actual_Type) in
     I_Bounded_Signedbv_Type | I_Bounded_Unsignedbv_Type;

   function Load_Real_Bound_In_Hex (Index : Integer; Actual_Type : Irep)
                                    return String
     with Pre => Kind (Actual_Type) = I_Bounded_Floatbv_Type;

   function Load_Symbol_Bound (Index : Integer) return Irep;
end Range_Check;
