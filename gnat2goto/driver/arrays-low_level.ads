with Uintp;              use Uintp;
with Tree_Walk;          use Tree_Walk;
package Arrays.Low_Level is
   --  The subprograms this package are not intended for use at the
   --  Tree_Walk package level, rather they ar intended to be used by the
   --  parent Arrays package and the Aggregates package.
   --
   --  The subprograms are concerned with manipulating the low-level, zero
   --  based array representation used by ASVAT.

   --  Arrays are represented in 3 ways:
   --  1. Simply as an Irep I_Array_Type.  This is the standard representation
   --  2. As a pointer a component of an array as in C.
   --        Used for formal array parameters.  If a formal array parameter
   --        is unconstrained supporting variables for each dimension of the
   --        array are added as extra parameters.
   --  3. As a structure equivalent to
   --       type Array_Struc  (Dimensions : Positive := 1) is
   --           record
   --               Bounds : Bounds_Array (0 .. 2 * Dimensions - 1);
   --               Data   : access Unconstrained_Array;
   --           end record;
   --    The Bounds array contains the lower and upper bounds for each
   --    dimension of the array.
   --        This representation is used for unconstrained arrays returned
   --        from functions.
   --
   --    In the future it might be sensible to merge type 2 and 3.

   --  The strings for defining first and last variables of an unconstrained
   --  array in type 2 representation.
   First_Var_Str : constant String := "___first_";
   Last_Var_Str  : constant String := "___last_";

   --  The strings for accessing the components of the Array_Struc in type 3
   --  representation.
   Array_Struc_Bounds : constant String := "bounds";
   Array_Struc_Data   : constant String := "data";
   Array_Bounds_No    : constant := 0;
   Array_Data_No      : constant := 1;
   Array_Struc_Tag    : constant String := "unconstr_array";
   Bounds_Component   : constant Irep := Int32_T;
   function Bounds_First (Dimension : Pos) return Uint is
     (UI_From_Int (Dimension * 2 - 2));
   function Bounds_Last (Dimension : Pos) return Uint is
     (UI_From_Int (Dimension * 2 - 1));
   function Bounds_Size (Dimensions : Pos) return Uint is
      (UI_From_Int (Dimensions * 2));

   --  An Ada type equivalent to the Bounds array of the Array_Struc.
   --  The component type is Bounds_Component.
   type Bounds_Array is array (Nat range <>) of Irep;

   function Get_Array_Struc_Type_Size (Dimensions : Pos) return Pos is
     (2 * Dimensions * Integer'Size + Pos (Pointer_Type_Width));

   --  When dealing with an array node it may be a reference to
   --  a function returning an array.
   --  It is important that a function returning an array is called only
   --  once (by applying Do_Expression) while the array is being processed.
   --  When the array node is a function call a goto variable is created
   --  and assigned the result of Do_Expression and this variable is
   --  placed in the Called_Array field of the Array_Reference.
   --  If the array node is not a function call the Called_Array Irep is
   --  set to Ireps.Empty.
   --  The Array)Node field is set to the given array node.
   --  Subsequent low_level array subprograms use the Array_Reference rather
   --  than the array node directly.
   type Array_Reference is record
      Array_Node   : Node_Id;
      Called_Array : Irep;
   end record;

   function Is_Unconstrained_Array_Result (Expr : Irep) return Boolean;

   --  Type for gathering the lower and upper bounds of an array dimension.
   type Dimension_Bounds is record
      Low, High : Irep;
   end record;

   type Static_And_Dynamic_Bounds is record
      Is_Unconstrained          : Boolean;
      Has_Static_Bounds         : Boolean;
      Low_Static, High_Static   : Int;
      Low_Dynamic, High_Dynamic : Irep;
   end record;

   --  The type used for index expressions.
   Index_T      : constant Irep := Int64_T;

   function Index_T_Zero return Irep renames Get_Int64_T_Zero;
   function Index_T_One  return Irep is
     (Integer_Constant_To_Expr (Uint_1, Index_T, Internal_Source_Location));

   type Static_And_Dynamic_Index is record
      Is_Static     : Boolean;
      Static_Index  : Uint;
      Dynamic_Index : Irep;
   end record;

   function Inc_Index (Ix : Irep) return Irep is
     (Make_Op_Add
        (Rhs             => Index_T_One,
         Lhs             => Ix,
         Source_Location => Get_Source_Location (Ix),
         I_Type          => Index_T));

   function Dec_Index (Ix : Irep) return Irep is
     (Make_Op_Sub
        (Rhs             => Index_T_One,
         Lhs             => Ix,
         Source_Location => Get_Source_Location (Ix),
         I_Type          => Index_T));

   function Add_One_To_Index (Index : Static_And_Dynamic_Index)
                              return Static_And_Dynamic_Index;

   procedure Add_One_To_Index (Index : in out Static_And_Dynamic_Index);

   function Sub_One_From_Index (Index : Static_And_Dynamic_Index)
                                return Static_And_Dynamic_Index;

   procedure Sub_One_From_Index (Index : in out Static_And_Dynamic_Index);

   function Add_To_Index (Index, Value_To_Add : Static_And_Dynamic_Index)
                          return Static_And_Dynamic_Index;

   procedure Add_To_Index (Index        : in out Static_And_Dynamic_Index;
                           Value_To_Add :        Static_And_Dynamic_Index);

   function Get_Dynamic_Index (Index : Static_And_Dynamic_Index) return Irep;

   Unconstrained_Bounds : constant Static_And_Dynamic_Bounds :=
     Static_And_Dynamic_Bounds'
       (Is_Unconstrained  => True,
        Has_Static_Bounds => False,
        Low_Static        => 0,
        High_Static       => 0,
        Low_Dynamic       => Ireps.Empty,
        High_Dynamic      => Ireps.Empty);

   function All_Dimensions_Static (The_Array : Entity_Id) return Boolean
     with Pre => Is_Array_Type (The_Array);

   procedure Assign_Array
     (Block         : Irep;
      Destination   : Irep;
      Dest_Bounds   : Static_And_Dynamic_Bounds;
      Source        : Irep;
      Source_Bounds : Static_And_Dynamic_Bounds);
   --  A low level arrauy assignment from Source_Irep to Destination Irep.
   --  The assignment can be simple if both arrays are constrained and
   --  have static bounds.

   procedure Assign_To_Array_Component (Block      : Irep;
                                        The_Array  : Irep;
                                        Zero_Index : Irep;
                                        Value_Expr : Irep;
                                        I_Type     : Irep;
                                        Location   : Irep);
   --  Assigns a value (Value_Expr, to an array element specified by the
   --  zero based index (Zero_Index).

   procedure Assign_Value_To_Dynamic_Array_Components
     (Block            : Irep;
      The_Array        : Irep;
      Zero_Based_First : Irep;
      Zero_Based_Last  : Irep;
      Value_Expr       : Irep;
      I_Type           : Irep;
      Location         : Irep);
   --  Assigns a single value to a contiguous set of elements of the array
   --  specified by the Zero_Based_First and Last values represented by Ireps.

   procedure Assign_Value_To_Static_Array_Components
     (Block            : Irep;
      The_Array        : Irep;
      Zero_Based_First : Int;
      Zero_Based_Last  : Int;
      Value_Expr       : Irep;
      I_Type           : Irep;
      Location         : Irep);
   --  Assigns a single value to a contiguous set of elements of the array
   --  specified by the statically determinable Zero_Based_First and Last
   --  Int values.

   function Calculate_Concat_Bounds
     (Target_Type   : Entity_Id;
      Concat_Length : Irep) return Dimension_Bounds
     with Pre => Is_Array_Type (Target_Type);
   --  Calculates the bounds of an array concatination.

   function Calculate_Concat_Length (N : Node_Id) return Irep
     with Pre => Nkind (N) = N_Op_Concat;
   --  Calculates the length of an array concatination.

   function Calculate_Dimension_Length (Bounds : Dimension_Bounds)
                                        return Irep;

   function Calculate_Index_Offset (Array_Node  : Node_Id;
                                    Array_Type  : Entity_Id;
                                    The_Indices : Node_Id) return Irep
   with Pre => Is_Array_Type (Array_Type) and
               Nkind (The_Indices) = N_Indexed_Component;
   --  Calculates the zero based index of a possibly multidimensional
   --  Ada array. Multidimensional Ada arrays are modelled as a
   --  one-dimensional array in row major order in ASVAT.

   function Calculate_Index_Offset_Static (Array_Node  : Node_Id;
                                            The_Indices : Node_Id) return Int
     with Pre => (Is_Array_Type (Etype (Array_Node)) and then
                 All_Dimensions_Static (Etype (Array_Node))) and
                 Nkind (The_Indices) = N_Indexed_Component;
   --  Similar to Calculate_Index_Offset above but can only be used if all
   --  the dimensions of the array are statically determinable.
   --  Returns an Int value rather than an Irep.

   function Calculate_Static_Dimension_Length (Dim_Range : Node_Id)
                                               return Uint;
   --  This function can be used to calculate the length of a dimension
   --  if it is known that the dimension bounds are static.

   function Calculate_Zero_Offset (Given_Index : Node_Id;
                                   Dim_Bounds  : Dimension_Bounds) return Irep
     with Pre => Nkind (Given_Index) in N_Subexpr;
   --  Calculates the zero offset from an Index represented by an Atree node.

   procedure Check_Equal_Array_Lengths
     (Block         : Irep;
      Source_Bounds : Static_And_Dynamic_Bounds;
      Dest_Bounds   : Static_And_Dynamic_Bounds);

   procedure Check_Equal_Array_Lengths
     (Block         : Irep;
      Source_Length : Irep;
      Dest_Length   : Irep);

   function Compute_Array_Byte_Size (Array_Type : Entity_Id) return Irep;

   procedure Copy_Array (Block          : Irep;
                         Dest_Bounds    : Static_And_Dynamic_Bounds;
                         Source_Bounds  : Static_And_Dynamic_Bounds;
                         Dest_Irep      : Irep;
                         Source_Irep    : Irep)
     with Pre => Kind (Get_Type (Dest_Irep)) in I_Array_Type | I_Pointer_Type
             and
                Kind (Get_Type (Source_Irep)) in I_Array_Type | I_Pointer_Type;

   function Flat_Bounds_From_Array_Struc (Array_Struc  : Irep;
                                          N_Dimensions : Pos)
                                          return Static_And_Dynamic_Bounds
     with Pre => Kind (Get_Type (Array_Struc)) = I_Struct_Tag_Type;

   function Get_Array_From_Struc (Array_Struc : Irep;
                                  Comp_Type   : Irep) return Irep
     with Pre  => Kind (Get_Type (Array_Struc)) = I_Struct_Tag_Type and
                  Kind (Comp_Type) in Class_Type,
          post => Kind (Get_Type (Get_Array_From_Struc'Result)) =
                  I_Pointer_Type;
   --  Retrieves the pointer to an array from an unonstrained array result.

   function Get_Array_Flat_Size (The_Array : Node_Id) return Irep
     with Pre => Is_Array_Type (Etype (The_Array));

   function Get_Array_Size_From_Bounds (Bounds : Static_And_Dynamic_Bounds)
                                        return Static_And_Dynamic_Index;

   function Get_Bounds_From_Struc (Array_Struc : Irep; Dimension : Pos)
                                   return Dimension_Bounds
     with Pre => Kind (Get_Type (Array_Struc)) = I_Struct_Tag_Type;

   function Get_Bounds_From_Index (Index : Node_Id) return Dimension_Bounds;
   --  If the array Index is constrained, returns the lower and upper bounds of
   --  the index constraint.

   function Get_Dimension_Bounds (N : Node_Id; Dim : Pos; Index : Node_Id)
                                  return Dimension_Bounds;
   --  The pre-condition is rather complex and so is provided by
   --  an assertion in the body.
   --  Baiscally, the Node N has to represent an array either directly
   --  or as an access to an array type.
   --  If N does not represent an object the array must be constrained.
   --  If N repesents an object it need not be constrained.
   --  Returns the bounds of a dimension of an array - the array may be of
   --  unconstrained type but then N must refer to a (constrained) object.

   function Get_Pointer_To_Array (The_Array : Irep; Comp_I_Type : Irep)
                                  return Irep
     with Pre  => Kind (Get_Type (The_Array)) in
                     I_Array_Type | I_Pointer_Type | I_Struct_Tag_Type |
                     I_String_Type,
       Post => Kind (Get_Type (Get_Pointer_To_Array'Result)) =
                                                             I_Pointer_Type;

   function Get_Range (Index : Node_Id) return Node_Id;

   function Get_Size_From_Array_Struc (Array_Struc  : Irep;
                                       N_Dimensions : Pos) return Irep
     with Pre => Kind (Array_Struc) in Class_Expr and then
     Kind (Get_Type (Array_Struc)) = I_Struct_Tag_Type;

   procedure Init_Array_Struc (Block       : Irep;
                               Array_Struc : Irep;
                               Array_Ptr   : Irep;
                               Location    : Irep;
                               Bounds      : Bounds_Array)
     with Pre => Kind (Array_Struc) in Class_Expr and
                 Kind (Get_Type (Array_Struc)) = I_Struct_Tag_Type and
                 Kind (Get_Type (Array_Ptr)) = I_Pointer_Type and
                 Bounds'Length > 1 and Bounds'Length mod 2 = 0 and
                 Bounds'First = 0;

   function Make_Array_Struc_Type (Comp_Type  : Irep;
                                   Location   : Irep;
                                   Dimensions : Pos) return Irep
     with Pre  => Kind (Comp_Type) in Class_Type,
          Post => Kind (Make_Array_Struc_Type'Result) = I_Struct_Type;

   function Make_Resolved_Index_Expr (The_Array  : Irep;
                                      Zero_Index : Irep;
                                      I_Type     : Irep;
                                      Location   : Irep) return Irep;

   function Make_Simple_For_Loop (Loop_Var,  --  The loop variable
                                  First,     --  The initial value of loop var
                                  Last,      --  The final value of loop var
                                  Loop_Body, --  The body, using loop var
                                  Source_Location : Irep) return Irep;

   function Make_Zero_Index (Index, First, Location : Irep) return Irep;
   --  Calculate a zero offset index from variables represented as Ireps.
   function Make_Zero_Index (Index : Irep; First : Int; Location : Irep)
                             return Irep;
   --  Calculate a zero offset index from an Ada index represented as an Irep
   --  and the lower bound given as an Int constant.

   function Multi_Dimension_Flat_Bounds (Array_Node : Node_Id)
                                         return Static_And_Dynamic_Bounds;
   --  Pre-conditopn is complex and is placed as a check in the body to assist
   --  with error recovery, but it has the form:
--       with Pre => ((Nkind (Array_Node) in N_Full_Type_Declaration |
--                                        N_Subtype_Declaration and then
--                        Is_Array_Type (Defining_Identifier (Array_Node)))
--                 or else (Nkind (Array_Node) in N_Object_Declaration |
--                        N_Object_Renaming_Declaration and then
--                          Is_Array_Type (Underlying_Type
--                        (Etype (Defining_Identifier (Array_Node)))))
--                 or else (Nkind (Array_Node) in N_Has_Etype and then
--                        Is_Array_Type (Underlying_Type
--                          (Etype (Array_Node))))
--                 or else (Nkind (Array_Node) = N_Attribute_Reference and then
--                   Get_Attribute_Id (Attribute_Name (N)) = Attribute_Image));
   --  In goto Ada multidimensional arrays are flattenned into one dimensional
   --  arrays. This function calculates the zero based bounds of a flattened
   --  multi-dimentional array

   function Zero_Based_Bounds (The_Array : Node_Id)
                               return Static_And_Dynamic_Bounds
     with Pre => Is_Array_Type (Underlying_Type (Etype (The_Array)));
   --  Calculate the zero based bounds of an array taking in to account
   --  any adjustment required if The_Array is a slice.
   --  For a slice Indexing is performed on the underlying array on which the
   --  slice is defined.
   --  If The_Array is not constrained then the result is
   --  Static_And_Dynamic_Bounds'
   --  (Is_Unconstrained  => True,
   --   Has_Static_Bounds => False,
   --   Low_Static  | High_Static  => 0,
   --   Low_Dynamic | High_Dynamic => Ireps.Empty).
   --  If the array, and any underlying array, if it is a slice, have static
   --  bounds, then result is
   --  If The_Array is not constrained then the result is
   --  Static_And_Dynamic_Bounds'
   --  (Is_Unconstrained  => False,
   --   Has_Static_Bounds => True,
   --   Low_Static   => <zero based lower bound with any slice adjustment>,
   --   High_Static  => <zero based upper bound with any slice
   --                    and multi-dimension adjustment>,
   --   Low_Dynamic  => <Irep representation of Low_Static>,
   --   High_Dynamic => <Irep representaion of High_Static>).
   --  If the array is constrained but does not have static bounds the
   --  the result is
   --  Static_And_Dynamic_Bounds'
   --  (Is_Unconstrained  => False,
   --   Has_Static_Bounds => False,
   --   Low_Static   => 0,
   --   High_Static  => 0,
   --   Low_Dynamic  => <zero based lower bound with any slice adjustment>,
   --   High_Dynamic => <zero based upper bound with any slice
   --                    and multi-dimension adjustment>.

   function Zero_Based_Slice_Bounds (The_Slice        : Node_Id;
                                     Underlying_Array : Entity_Id)
                                     return Static_And_Dynamic_Bounds
   with Pre => (Nkind (The_Slice) = N_Slice and
                Is_Array_Type (Underlying_Array));
   --  A slice is represented by its underlying array with the zero based
   --  lower and upper bounds adjusted so that they index into just the
   --  components defined by the slice.
   --  A slice is always one-dimensional and is constrained and its underlying
   --  array also must have a constraint even if it is of an unconstrained
   --  subtype.  At some point it will have been initialized and constrained.

end Arrays.Low_Level;
