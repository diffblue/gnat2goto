with Ada.Containers.Vectors;    use Ada.Containers;

with Types;                     use Types;
with Uintp;                     use Uintp;
with Urealp;                    use Urealp;
with Ada.Containers.Ordered_Maps;

with Ireps;                     use Ireps;

package Range_Check is


   type Bound_Type_Nat is new Uint;
   type Bound_Type_Real is new Ureal;
   type Bound_Type_Symbol is new Irep;

   function Store_Nat_Bound (Number : Bound_Type_Nat) return Integer;
   function Store_Real_Bound (Number : Bound_Type_Real) return Integer;
   function Store_Symbol_Bound (Number : Bound_Type_Symbol) return Integer;

   function Make_Range_Assert_Expr (N : Node_Id; Value : Irep;
                                    Bounds_Type : Irep) return Irep;

   function Make_Range_Expression (Value_Expr : Irep; Val_Type : Irep)
                                   return Irep
     with Post => Kind (Make_Range_Expression'Result) in Class_Expr;

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
     with Pre => Kind (Actual_Type) = I_Bounded_Signedbv_Type;

   function Load_Real_Bound_In_Hex (Index : Integer; Actual_Type : Irep)
                                    return String
     with Pre => Kind (Actual_Type) = I_Bounded_Floatbv_Type;

   function Load_Symbol_Bound (Index : Integer) return Irep;

   --  might be best if this is moved to a utility package in future
   --  atm placement documents it is only used by range_check
   function Make_Assert_Call (N : Node_Id; Assertion : Irep;
                              Description : Irep) return Irep;

end Range_Check;
