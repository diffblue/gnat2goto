with Stand;                   use Stand;
with Nlists;                  use Nlists;
with Sem_Util;                use Sem_Util;
with Sem_Eval;                use Sem_Eval;
with ASVAT.Size_Model;        use ASVAT.Size_Model;
with Symbol_Table_Info;       use Symbol_Table_Info;
with Range_Check;             use Range_Check;
with Ada.Text_IO; use Ada.Text_IO;
with Follow; use Follow;

package body Arrays.Low_Level is

   --  A Irep of an array could be of one of the following sorts:
   --  (1) an I_Array_Type,
   --  (2) a Pointer to an array element,
   --  (3) a Bounded array structure, or,
   --  (4) a pointer to a bounded array structure.
   --  (5) an I_String_Type
   --  Any other entity is not expected.
   type Array_Sort is (An_I_Array, A_Pointer_To_Elem,
                       A_Bounded, A_Pointer_To_Bounded,
                       An_I_String, Unexpected);
   function Get_Array_Sort (The_Array : Irep) return Array_Sort;
   function Get_Array_Sort (The_Array : Irep) return Array_Sort is
      Array_I_Type     : constant Irep := Get_Type (The_Array);
      Is_Pointer       : constant Boolean :=
        Kind (Array_I_Type) = I_Pointer_Type;
      Is_Bounded_Arr  : constant Boolean :=
        Is_Unconstrained_Array_Result (Array_I_Type);

      Arr_Sort         : constant Array_Sort :=
        (if Kind (Array_I_Type) = I_Array_Type then
              An_I_Array
         elsif Kind (Array_I_Type) = I_String_Type then
              An_I_String
         elsif Is_Pointer and not Is_Bounded_Arr then
            A_Pointer_To_Elem
         elsif not Is_Pointer and Is_Bounded_Arr then
            A_Bounded
         elsif Is_Pointer and Is_Bounded_Arr then
            A_Pointer_To_Bounded
         else
            Unexpected);
   begin
      pragma Assert (Arr_Sort /= Unexpected);
      return Arr_Sort;
   end Get_Array_Sort;

   function Add_One_To_Index (Index : Static_And_Dynamic_Index)
                              return Static_And_Dynamic_Index is
      Result : Static_And_Dynamic_Index := Index;
   begin
      Add_One_To_Index (Result);
      return Result;
   end Add_One_To_Index;

   procedure Add_One_To_Index (Index : in out Static_And_Dynamic_Index) is
   begin
      if Index.Is_Static then
         Index.Static_Index := Index.Static_Index + Uint_1;
      else
         Index.Dynamic_Index := Inc_Index (Index.Dynamic_Index);
      end if;
   end Add_One_To_Index;

   function Sub_One_From_Index (Index : Static_And_Dynamic_Index)
                                return Static_And_Dynamic_Index is
      Result : Static_And_Dynamic_Index := Index;
   begin
      Sub_One_From_Index (Result);
      return Result;
   end Sub_One_From_Index;

   procedure Sub_One_From_Index (Index : in out Static_And_Dynamic_Index) is
   begin
      if Index.Is_Static then
         Index.Static_Index := Index.Static_Index - Uint_1;
      else
         Index.Dynamic_Index := Dec_Index (Index.Dynamic_Index);
      end if;
   end Sub_One_From_Index;

   function Add_To_Index (Index, Value_To_Add : Static_And_Dynamic_Index)
                          return Static_And_Dynamic_Index is
      Result : Static_And_Dynamic_Index := Index;
   begin
      Add_To_Index (Result, Value_To_Add);
      return Result;
   end Add_To_Index;

   procedure Add_To_Index (Index        : in out Static_And_Dynamic_Index;
                           Value_To_Add :        Static_And_Dynamic_Index) is
   begin
      if Index.Is_Static and Value_To_Add.Is_Static then
         Index.Static_Index := Index.Static_Index + Value_To_Add.Static_Index;
      else
         if Index.Is_Static then
            Index.Is_Static := False;
            Index.Dynamic_Index :=
              Integer_Constant_To_Expr
                (Value           => Index.Static_Index,
                 Expr_Type       => Index_T,
                 Source_Location => Internal_Source_Location);
         end if;

         Index.Dynamic_Index :=
           Make_Op_Add
             (Rhs             => Value_To_Add.Dynamic_Index,
              Lhs             => Index.Dynamic_Index,
              Source_Location => Internal_Source_Location,
              Overflow_Check  => False,
              I_Type          => Index_T,
              Range_Check     => False);
      end if;
   end Add_To_Index;

   function Is_Unconstrained_Array_Result (Expr : Irep) return Boolean is
      Expr_Type : constant Irep :=
        (if Kind (Expr) in Class_Expr then
              Get_Type (Expr)
         elsif Kind (Expr) in Class_Type then
              Expr
         else
            Ireps.Empty);

      function Is_Bounded (Arr_Type : Irep) return Boolean;
      function Is_Bounded (Arr_Type : Irep) return Boolean is
         Base_I_Type   : constant Irep :=
           Get_Base_I_Type (Arr_Type, Global_Symbol_Table);
         Base_Type_Kind : constant Irep_Kind := Kind (Base_I_Type);
      begin
         return
           Base_Type_Kind = I_Struct_Type and then
           Get_Tag (Base_I_Type) = Array_Struc_Tag;
      end Is_Bounded;

   begin
      return
        (Kind (Expr_Type) = I_Pointer_Type and then
         Is_Bounded (Get_Subtype (Expr_Type))) or else
           (Is_Bounded (Expr_Type));
   end Is_Unconstrained_Array_Result;

   procedure Assign_Array
     (Block         : Irep;
      Destination   : Irep;
      Dest_Bounds   : Static_And_Dynamic_Bounds;
      Source        : Irep;
      Source_Bounds : Static_And_Dynamic_Bounds)
   is
      Source_Location  : constant Irep := Get_Source_Location (Destination);
      Target_I_Type    : constant Irep := Get_Type (Destination);
      Component_I_Type : constant Irep := Get_Subtype (Target_I_Type);
   begin
      if not Dest_Bounds.Is_Unconstrained then
         if Dest_Bounds.Has_Static_Bounds and
           Source_Bounds.Is_Unconstrained and
           Source_Bounds.Has_Static_Bounds and
           not Is_Unconstrained_Array_Result (Source)
         then
            --  Both source and destination have static bounds.
            --   A simple assignment should work.
            declare
               Assignment : constant Irep :=
                 Make_Code_Assign
                   (Rhs             => Typecast_If_Necessary
                      (Expr           => Source,
                       New_Type       => Target_I_Type,
                       A_Symbol_Table => Global_Symbol_Table),
                    Lhs             => Destination,
                    Source_Location => Source_Location,
                    I_Type          => Target_I_Type,
                    Range_Check     => False);
            begin
               Append_Op (Block, Assignment);
            end;
         else
            declare
               Resolved_Dest   : constant Irep := Get_Pointer_To_Array
                 (Destination, Component_I_Type);
               Resolved_Source : constant Irep := Get_Pointer_To_Array
                 (Source, Component_I_Type);
            begin
               Copy_Array
                 (Block         => Block,
                  Dest_Bounds   => Dest_Bounds,
                  Source_Bounds => Source_Bounds,
                  Dest_Irep     => Resolved_Dest,
                  Source_Irep   => Resolved_Source);
            end;
         end if;
      else
         Report_Unhandled_Node_Empty
           (N        => Types.Empty,
            Fun_Name => "Assign_Array",
            Message  => "Assignment to an unconstrained array object " &
              Get_Identifier (Destination) &
              " is unsupported");
      end if;
   end Assign_Array;

   function Get_Dynamic_Index (Index : Static_And_Dynamic_Index) return Irep
   is
     (if Index.Is_Static then
         Integer_Constant_To_Expr
        (Value           => Index.Static_Index,
         Expr_Type       => Index_T,
         Source_Location => Internal_Source_Location)
      else
         Index.Dynamic_Index
      );

   procedure Copy_Array_Dynamic
     (Block            : Irep;
      Dest_Low         : Irep;
      Dest_High        : Irep;
      Source_Low       : Irep;
      Source_High      : Irep;
      Dest_Irep        : Irep;
      Source_Irep      : Irep);

   procedure Copy_Array_Static
     (Block            : Irep;
      Dest_Low         : Int;
      Dest_High        : Int;
      Source_Low       : Int;
      Source_High      : Int;
      Dest_Irep        : Irep;
      Source_Irep      : Irep);

   ---------------------------
   -- All_Dimensions_Static --
   ---------------------------

   function All_Dimensions_Static (The_Array : Entity_Id) return Boolean is
      Next_Dim : Node_Id := First_Index (The_Array);
      Result : Boolean := True;
   begin
      while Present (Next_Dim) loop
         if not Is_OK_Static_Range (Get_Range (Next_Dim)) then
            Result := False;
            exit;
         end if;
         Next_Dim := Next_Index (Next_Dim);
      end loop;
      return Result;
   end All_Dimensions_Static;

   -------------------------------
   -- Assign_To_Array_Component --
   -------------------------------

   procedure Assign_To_Array_Component (Block      : Irep;
                                        The_Array  : Irep;
                                        Zero_Index : Irep;
                                        Value_Expr : Irep;
                                        I_Type     : Irep;
                                        Location   : Irep)
   is
   begin
      Append_Op
        (Block,
         Make_Code_Assign
           (Rhs             => Value_Expr,
            Lhs             =>
              Make_Resolved_Index_Expr
                (The_Array  => The_Array,
                 Zero_Index => Zero_Index,
                 I_Type => I_Type,
                 Location   => Location),
            Source_Location => Location,
            I_Type          => I_Type,
            Range_Check     => False));
   end Assign_To_Array_Component;

   ----------------------------------------------
   -- Assign_Value_To_Dynamic_Array_Components --
   ----------------------------------------------

   procedure Assign_Value_To_Dynamic_Array_Components
     (Block            : Irep;
      The_Array        : Irep;
      Zero_Based_First : Irep;
      Zero_Based_Last  : Irep;
      Value_Expr       : Irep;
      I_Type           : Irep;
      Location         : Irep)
   is
      Loop_Var : constant Irep :=
        Fresh_Var_Symbol_Expr (Index_T, "avd_loop_var");
      Loop_Body : constant Irep :=
        Make_Code_Block (Source_Location => Location);
   begin
      --  The body of the loop is just the assignment of the value expression
      --  to the indexed component.

      Append_Op (Block,
                 Make_Code_Decl (Loop_Var, Location));
      Assign_To_Array_Component
        (Block      => Loop_Body,
         The_Array  => The_Array,
         Zero_Index => Loop_Var,
         Value_Expr => Value_Expr,
         I_Type     => I_Type,
         Location   => Location);

      --  Now the loop can be constructed.
      Append_Op (Block,
                 Make_Simple_For_Loop
                   (Loop_Var        => Loop_Var,
                    First           => Zero_Based_First,
                    Last            => Zero_Based_Last,
                    Loop_Body       => Loop_Body,
                    Source_Location => Location));
   end Assign_Value_To_Dynamic_Array_Components;

   ---------------------------------------------
   -- Assign_Value_To_Static_Array_Components --
   ---------------------------------------------

   procedure Assign_Value_To_Static_Array_Components
     (Block            : Irep;
      The_Array        : Irep;
      Zero_Based_First : Int;
      Zero_Based_Last  : Int;
      Value_Expr       : Irep;
      I_Type           : Irep;
      Location         : Irep)
   is
   begin
      for I in Zero_Based_First .. Zero_Based_Last loop
         Assign_To_Array_Component
           (Block      => Block,
            The_Array  => The_Array,
            Zero_Index =>
              Integer_Constant_To_Expr
                (Value           => UI_From_Int (I),
                 Expr_Type       => Index_T,
                 Source_Location => Location),
            Value_Expr => Value_Expr,
            I_Type     => I_Type,
            Location   => Location);
      end loop;
   end Assign_Value_To_Static_Array_Components;

   -----------------------------
   -- Calculate_Concat_Bounds --
   -----------------------------

   function Calculate_Concat_Bounds
     (Target_Type   : Entity_Id;
      Concat_Length : Irep) return Dimension_Bounds is
   begin
      --  A concatination is always 1 dimensional.
      if Is_Constrained (Target_Type) then
         return Get_Bounds_From_Index (First_Index (Target_Type));
      end if;

      declare
         Index      : constant Node_Id := First_Index (Target_Type);
         Index_Type : constant Entity_Id := Base_Type (Etype (Index));

--           Type_Irep : constant Irep :=
--             Do_Type_Reference (Index_Type);

         --  For unconstrained array type, a concatination takes the
         --  first value of the scalar index type.
         Lower_Bound  : constant Node_Id := Type_Low_Bound (Index_Type);
         Low_Irep     : constant Irep := Do_Expression (Lower_Bound);

         Low_Index    : constant Irep :=
           Typecast_If_Necessary
             (Expr           => Low_Irep,
              New_Type       => Index_T,
              A_Symbol_Table => Global_Symbol_Table);

         High_Index : constant Irep :=
           Make_Op_Sub
             (Rhs             => Index_T_One,
              Lhs             => Make_Op_Add
                (Rhs             => Concat_Length,
                 Lhs             => Low_Index,
                 Source_Location => Internal_Source_Location,
                 Overflow_Check  => False,
                 I_Type          => Index_T,
                 Range_Check     => False),
              Source_Location => Internal_Source_Location,
              Overflow_Check  => False,
              I_Type          => Index_T,
              Range_Check     => False);

--           High_Irep : constant Irep :=
--             Typecast_If_Necessary
--               (Expr           => High_Index,
--                New_Type       => Type_Irep,
--                A_Symbol_Table => Global_Symbol_Table);
      begin
         return (Low_Index, High_Index);
      end;
   end Calculate_Concat_Bounds;

   -----------------------------
   -- Calculate_Concat_Length --
   -----------------------------

   function Calculate_Concat_Length (N : Node_Id) return Irep is

      procedure Calc_Length (N             : Node_Id;
                             Accum_Length  : in out Static_And_Dynamic_Index);

      procedure Calc_Length (N             : Node_Id;
                             Accum_Length  : in out Static_And_Dynamic_Index)
      is
      begin
         if Nkind (N) = N_Op_Concat then
            if Is_Component_Left_Opnd (N) then
               Add_One_To_Index (Accum_Length);
            else
               Calc_Length
                 (N            => Left_Opnd (N),
                  Accum_Length => Accum_Length);
            end if;
            if Is_Component_Right_Opnd (N) then
               Add_One_To_Index (Accum_Length);
            else
               Calc_Length
                 (N            => Right_Opnd (N),
                  Accum_Length => Accum_Length);
            end if;
         else
            declare
               Next_Bounds : constant Static_And_Dynamic_Bounds :=
                 Multi_Dimension_Flat_Bounds (N);
               Next_Length : constant Static_And_Dynamic_Index :=
                 Get_Array_Size_From_Bounds (Next_Bounds);
            begin
               if not Next_Bounds.Is_Unconstrained then
                  Add_To_Index (Index        => Accum_Length,
                                Value_To_Add => Next_Length);
               else
                  Report_Unhandled_Node_Empty
                    (N        => N,
                     Fun_Name => "Calculate_Concat_Length",
                     Message  => "Unconstrained array expressions in " &
                       "concatinations are unsupported");
               end if;
            end;
         end if;
      end Calc_Length;

      Accum_Length : Static_And_Dynamic_Index :=
        Static_And_Dynamic_Index'
          (Is_Static     => True,
           Static_Index  => Uint_0,
           Dynamic_Index => Ireps.Empty);
      Result : Irep;
   begin
      Calc_Length
        (N            => N,
         Accum_Length => Accum_Length);

      if Accum_Length.Is_Static then
         Result := Integer_Constant_To_Expr
           (Value           => Accum_Length.Static_Index,
            Expr_Type       => Index_T,
            Source_Location => Internal_Source_Location);
      else
         Result := Accum_Length.Dynamic_Index;
      end if;
      return Result;
   end Calculate_Concat_Length;

   --------------------------------
   -- Calculate_Dimension_Length --
   --------------------------------

   function Calculate_Dimension_Length (Bounds : Dimension_Bounds)
                                        return Irep is
      First_Val : constant Irep :=
        Typecast_If_Necessary
          (Expr           => Bounds.Low,
           New_Type       => Index_T,
           A_Symbol_Table => Global_Symbol_Table);
      Last_Val : constant Irep :=
        Typecast_If_Necessary
          (Expr           => Bounds.High,
           New_Type       => Index_T,
           A_Symbol_Table => Global_Symbol_Table);

      Diff : constant Irep :=
        Make_Op_Sub
          (Rhs             => First_Val,
           Lhs             => Last_Val,
           Source_Location => Internal_Source_Location,
           Overflow_Check  => False,
           I_Type          => Index_T,
           Range_Check     => False);

      Length_Val : constant Irep :=
        Make_Op_Add
          (Rhs             => Index_T_One,
           Lhs             => Diff,
           Source_Location => Internal_Source_Location,
           Overflow_Check  => False,
           I_Type          => Index_T,
           Range_Check     => False);
   begin
      return Length_Val;
   end Calculate_Dimension_Length;

   ----------------------------
   -- Calculate_Index_Offset --
   ----------------------------

   function Calculate_Index_Offset (Array_Node  : Node_Id;
                                    Array_Type  : Entity_Id;
                                    The_Indices : Node_Id) return Irep
   is
      Source_Location : constant Irep := Get_Source_Location (The_Indices);
      No_Of_Dims  : constant Pos := Number_Dimensions (Array_Type);
      Index_Iter  : Node_Id := First (Expressions (The_Indices));
      Dim_Iter    : Node_Id := First_Index (Array_Type);
      Bounds_Iter : Dimension_Bounds :=
        Get_Dimension_Bounds (Array_Node, 1, Dim_Iter);
      Offset      : Irep :=
        Calculate_Zero_Offset (Index_Iter, Bounds_Iter);
   begin
      for I in 2 .. No_Of_Dims loop
         Index_Iter := Next (Index_Iter);
         Dim_Iter := Next_Index (Dim_Iter);
         Bounds_Iter := Get_Dimension_Bounds (Array_Node, I, Dim_Iter);
         Offset :=
           Make_Op_Add
             (Rhs             => Calculate_Zero_Offset
                (Index_Iter, Bounds_Iter),
              Lhs             => Make_Op_Mul
                (Rhs             => Offset,
                 Lhs             =>
                   Typecast_If_Necessary
                     (Expr           =>
                          Calculate_Dimension_Length (Bounds_Iter),
                      New_Type       => Index_T,
                      A_Symbol_Table => Global_Symbol_Table),
                 Source_Location => Source_Location,
                 Overflow_Check  => False,
                 I_Type          => Index_T,
                 Range_Check     => False),
              Source_Location => Source_Location,
              Overflow_Check  => False,
              I_Type          => Index_T,
              Range_Check     => False);
      end loop;
      return Offset;
   end Calculate_Index_Offset;

   -----------------------------------
   -- Calculate_Index_Offset_Static --
   -----------------------------------

   function Calculate_Index_Offset_Static (Array_Node  : Node_Id;
                                           The_Indices : Node_Id) return Int
   is
      Array_Type  : constant Entity_Id := Etype (Array_Node);
      No_Of_Dims  : constant Positive :=
        Positive (Number_Dimensions (Array_Type));
      Index_Iter  : Node_Id := First (Expressions (The_Indices));
      Dim_Iter    : Node_Id := First_Index (Array_Type);
      Offset      : Uint :=
        Expr_Value (Index_Iter) - Expr_Value (Low_Bound (Dim_Iter));
   begin
      for I in 2 .. No_Of_Dims loop
         Index_Iter := Next (Index_Iter);
         Dim_Iter := Next_Index (Dim_Iter);
         Offset :=
           (Calculate_Static_Dimension_Length (Get_Range (Dim_Iter)) *
             Offset) +
           (Expr_Value (Index_Iter) - Expr_Value (Low_Bound (Dim_Iter)));
      end loop;
      return UI_To_Int (Offset);
   end Calculate_Index_Offset_Static;

   ---------------------------------------
   -- Calculate_Static_Dimension_Length --
   ---------------------------------------

   function Calculate_Static_Dimension_Length (Dim_Range : Node_Id)
                                               return Uint
   is
     (Expr_Value (High_Bound (Dim_Range)) -
        Expr_Value (Low_Bound (Dim_Range)) + Uint_1);

   ---------------------------
   -- Calculate_Zero_Offset --
   ---------------------------

   function Calculate_Zero_Offset (Given_Index : Node_Id;
                                   Dim_Bounds  : Dimension_Bounds) return Irep
   is
      Index_Irep    : constant Irep :=
        Typecast_If_Necessary
          (Expr           => Do_Expression (Given_Index),
           New_Type       => Index_T,
           A_Symbol_Table => Global_Symbol_Table);
      First_Irep    : constant Irep :=
        Typecast_If_Necessary
          (Expr           => Dim_Bounds.Low,
           New_Type       => Index_T,
           A_Symbol_Table => Global_Symbol_Table);
      Last_Irep    : constant Irep :=
        Typecast_If_Necessary
          (Expr           => Dim_Bounds.High,
           New_Type       => Index_T,
           A_Symbol_Table => Global_Symbol_Table);
      Checked_Index : constant Irep :=
        Make_Index_Assert_Expr (N           => Given_Index,
                                Index       => Index_Irep,
                                First_Index => First_Irep,
                                Last_Index  => Last_Irep);
   begin
      return
        Make_Op_Sub
          (Rhs             => First_Irep,
           Lhs             => Typecast_If_Necessary
             (Expr           => Checked_Index,
              New_Type       => Index_T,
              A_Symbol_Table => Global_Symbol_Table),
           Source_Location => Get_Source_Location (Given_Index),
           Overflow_Check  => False,
           I_Type          => Index_T,
           Range_Check     => False);
   end Calculate_Zero_Offset;

   -------------------------------
   -- Check_Equal_Array_Lengths --
   -------------------------------

   procedure Check_Equal_Array_Lengths
     (Block         : Irep;
      Source_Bounds : Static_And_Dynamic_Bounds;
      Dest_Bounds   : Static_And_Dynamic_Bounds)
   is
      pragma Unreferenced (Block);
   begin
      if Source_Bounds.Has_Static_Bounds and Dest_Bounds.Has_Static_Bounds then
         declare
            Source_Length : constant Nat :=
              Source_Bounds.High_Static - Source_Bounds.Low_Static + 1;
            Dest_Length   : constant Nat :=
              Dest_Bounds.High_Static - Dest_Bounds.Low_Static + 1;
         begin
            if Source_Length /= Dest_Length then
               Put_Line ("Dynamic_Length check failed");
            end if;
         end;
      end if;
   end Check_Equal_Array_Lengths;

   procedure Check_Equal_Array_Lengths
     (Block         : Irep;
      Source_Length : Irep;
      Dest_Length   : Irep)
   is
      pragma Unreferenced (Block);
   begin
      null;
   end Check_Equal_Array_Lengths;

   ------------------------------
   -- Compute_Array_Byte_Size  --
   ------------------------------

   function Compute_Array_Byte_Size (Array_Type : Entity_Id) return Irep
   is
      Location       : constant Irep := Get_Source_Location (Array_Type);
      Array_Bounds   : constant Static_And_Dynamic_Bounds :=
        Multi_Dimension_Flat_Bounds (Array_Type);
      Array_Length   : constant Irep :=
        Calculate_Dimension_Length
          (Dimension_Bounds'
             (Low  => Array_Bounds.Low_Dynamic,
              High => Array_Bounds.High_Dynamic));
      Comp_Type      : constant Entity_Id :=
        Underlying_Type (Component_Type (Array_Type));
      Comp_Size      : constant Irep := Typecast_If_Necessary
        (Expr           => ASVAT.Size_Model.Computed_Size (Comp_Type),
         New_Type       => Index_T,
         A_Symbol_Table => Global_Symbol_Table);
      --  Component size is in bits a byte size but the model size should be
      --  a multiple of 8.
      --  Just in case calculate number of bytes assuming the bit size is
      --  not a multiple of 8.
      --  ((Bitsize - 1) / 8) + 1
      Comp_Byte_Size : constant Irep :=
        Make_Op_Add
          (Rhs             => Index_T_One,
           Lhs             => Make_Op_Div
             (Rhs               => Integer_Constant_To_Expr
                (Value           => Uint_8,
                 Expr_Type       => Index_T,
                 Source_Location => Location),
              Lhs               => Make_Op_Sub
                (Rhs             => Index_T_One,
                 Lhs             => Comp_Size,
                 Source_Location => Location,
                 I_Type          => Index_T),
              Source_Location   => Location,
              I_Type            => Index_T,
              Div_By_Zero_Check => False),
           Source_Location => Location,
           I_Type          => Index_T);
   begin
      return Make_Op_Mul
        (Rhs             => Comp_Byte_Size,
         Lhs             => Array_Length,
         Source_Location => Location,
         I_Type          => Index_T);
   end Compute_Array_Byte_Size;

   ----------------
   -- Copy_Array --
   ----------------

   procedure Copy_Array (Block          : Irep;
                         Dest_Bounds    : Static_And_Dynamic_Bounds;
                         Source_Bounds  : Static_And_Dynamic_Bounds;
                         Dest_Irep      : Irep;
                         Source_Irep    : Irep)
   is
      Static_Limit        : constant := 16;
   begin
      if (Dest_Bounds.Has_Static_Bounds and Source_Bounds.Has_Static_Bounds)
        and then
          Dest_Bounds.High_Static - Dest_Bounds.Low_Static + 1 <= Static_Limit
      then
         Copy_Array_Static
           (Block       => Block,
            Dest_Low    => Dest_Bounds.Low_Static,
            Dest_High   => Dest_Bounds.High_Static,
            Source_Low  => Source_Bounds.Low_Static,
            Source_High => Source_Bounds.High_Static,
            Dest_Irep   => Dest_Irep,
            Source_Irep => Source_Irep);
      else
         declare
            Dest_Location : constant Irep :=
              Get_Source_Location (Dest_Irep);
            Src_Location  : constant Irep :=
              Get_Source_Location (Source_Irep);
            Dest_Low_Var  : constant Irep :=
              Fresh_Var_Symbol_Expr (Index_T, "dest_low");
            Dest_High_Var : constant Irep :=
              Fresh_Var_Symbol_Expr (Index_T, "dest_high");

            Source_Low_Var  : constant Irep :=
              Fresh_Var_Symbol_Expr (Index_T, "source_low");
            Source_High_Var : constant Irep :=
              Fresh_Var_Symbol_Expr (Index_T, "source_high");
         begin
            Append_Declare_And_Init
              (Symbol     => Dest_Low_Var,
               Value      => Typecast_If_Necessary
                 (Expr           => Dest_Bounds.Low_Dynamic,
                  New_Type       => Index_T,
                  A_Symbol_Table => Global_Symbol_Table),
               Block      => Block,
               Source_Loc => Dest_Location);
            Append_Declare_And_Init
              (Symbol     => Dest_High_Var,
               Value      => Typecast_If_Necessary
                 (Expr           => Dest_Bounds.High_Dynamic,
                  New_Type       => Index_T,
                  A_Symbol_Table => Global_Symbol_Table),
               Block      => Block,
               Source_Loc => Dest_Location);
            Append_Declare_And_Init
              (Symbol     => Source_Low_Var,
               Value      => Typecast_If_Necessary
                 (Expr           => Source_Bounds.Low_Dynamic,
                  New_Type       => Index_T,
                  A_Symbol_Table => Global_Symbol_Table),
               Block      => Block,
               Source_Loc => Src_Location);
            Append_Declare_And_Init
              (Symbol     => Source_High_Var,
               Value      => Typecast_If_Necessary
                 (Expr           => Source_Bounds.High_Dynamic,
                  New_Type       => Index_T,
                  A_Symbol_Table => Global_Symbol_Table),
               Block      => Block,
               Source_Loc => Src_Location);

            Copy_Array_Dynamic
              (Block          => Block,
               Dest_Low       => Dest_Low_Var,
               Dest_High      => Dest_High_Var,
               Source_Low  => Source_Low_Var,
               Source_High => Source_High_Var,
               Dest_Irep   => Dest_Irep,
               Source_Irep => Source_Irep);
         end;
      end if;
   end Copy_Array;

   ------------------------
   -- Copy_Array_Dynamic --
   ------------------------

   procedure Copy_Array_Dynamic
     (Block            : Irep;
      Dest_Low         : Irep;
      Dest_High        : Irep;
      Source_Low       : Irep;
      Source_High      : Irep;
      Dest_Irep        : Irep;
      Source_Irep      : Irep)
   is
      pragma Unreferenced (Source_High);  -- Used in precondition.
      Source_Location : constant Irep := Get_Source_Location (Dest_Irep);

      Component_Source : constant Irep := Get_Subtype (Get_Type (Source_Irep));
      Component_Dest   : constant Irep := Get_Subtype (Get_Type (Dest_Irep));

      Loop_Var : constant Irep :=
        Fresh_Var_Symbol_Expr (Index_T, "cad_loop_var");
      Loop_Body : constant Irep :=
        Make_Code_Block (Source_Location => Source_Location);

      Dest_Idx   : constant Irep :=
        Make_Op_Add
          (Rhs             => Dest_Low,
           Lhs             => Loop_Var,
           Source_Location => Source_Location,
           Overflow_Check  => False,
           I_Type          => Index_T,
           Range_Check     => False);
      Source_Idx : constant Irep :=
        Make_Op_Add
          (Rhs             => Source_Low,
           Lhs             => Loop_Var,
           Source_Location => Source_Location,
           Overflow_Check  => False,
           I_Type          => Index_T,
           Range_Check     => False);

      Loop_First : constant Irep := Index_T_Zero;
      Loop_Last  : constant Irep := Make_Op_Sub
        (Rhs             => Dest_Low,
         Lhs             => Dest_High,
         Source_Location => Source_Location,
         I_Type          => Index_T);
--          Make_Op_Sub
--            (Rhs             => Dest_Low,
--             Lhs             => Dest_High,
--             Source_Location => Source_Location,
--             Overflow_Check  => False,
--             I_Type          => Index_T,
--             Range_Check     => False);

      Loop_First_Var   : constant Irep :=
        Fresh_Var_Symbol_Expr (Index_T, "loop_first");

      Loop_Last_Var    : constant Irep :=
        Fresh_Var_Symbol_Expr (Index_T, "loop_last");

      Indexed_Source   : constant Irep :=
        Make_Resolved_Index_Expr
          (The_Array  => Source_Irep,
           Zero_Index => Source_Idx,
           I_Type     => Component_Source,
           Location   => Source_Location);

      Assign_Value     : constant Irep :=
        (if Component_Source = Component_Dest then
            Indexed_Source
         else
            Make_Op_Typecast
           (Op0             => Indexed_Source,
            Source_Location => Source_Location,
            I_Type          => Component_Dest,
            Range_Check     => False));
   begin
      Append_Op (Block,
                 Make_Code_Decl
                   (Symbol          => Loop_Var,
                    Source_Location => Source_Location));
      Append_Declare_And_Init
        (Symbol     => Loop_First_Var,
         Value      => Loop_First,
         Block      => Block,
         Source_Loc => Source_Location);
      Append_Declare_And_Init
        (Symbol     => Loop_Last_Var,
         Value      => Loop_Last,
         Block      => Block,
         Source_Loc => Source_Location);
      --  The body of the loop is just the assignment of the indexed source
      --  element to the indexed destination element.
      Assign_To_Array_Component
        (Block      => Loop_Body,
         The_Array  => Dest_Irep,
         Zero_Index => Dest_Idx,
         Value_Expr => Assign_Value,
         I_Type     => Component_Dest,
         Location   => Source_Location);

      --  Now the loop can be constructed.
      Append_Op (Block,
                 Make_Simple_For_Loop
                   (Loop_Var        => Loop_Var,
                    First           => Loop_First_Var,
                    Last            => Loop_Last_Var,
                    Loop_Body       => Loop_Body,
                    Source_Location => Source_Location));
   end Copy_Array_Dynamic;

   -----------------------
   -- Copy_Array_Static --
   -----------------------

   procedure Copy_Array_Static
     (Block            : Irep;
      Dest_Low         : Int;
      Dest_High        : Int;
      Source_Low       : Int;
      Source_High      : Int;
      Dest_Irep        : Irep;
      Source_Irep      : Irep)
   is
      --  Currently Source_High is only referenced in the precondition.
      pragma Unreferenced (Source_High);
      Source_Location : constant Irep := Get_Source_Location (Dest_Irep);

      Component_Source : constant Irep := Get_Subtype (Get_Type (Source_Irep));
      Component_Dest : constant Irep := Get_Subtype (Get_Type (Dest_Irep));

   begin
      for I in 0 .. Dest_High - Dest_Low  loop
         Assign_To_Array_Component
           (Block      => Block,
            The_Array  => Dest_Irep,
            Zero_Index =>
              Integer_Constant_To_Expr
                (Value           => UI_From_Int (I + Dest_Low),
                 Expr_Type       => Index_T,
                 Source_Location =>
                   Internal_Source_Location),
            Value_Expr =>
              Typecast_If_Necessary
                (Expr           => Make_Resolved_Index_Expr
                   (The_Array  => Source_Irep,
                    Zero_Index => --  Var_Index_Dest,
                      Integer_Constant_To_Expr
                        (Value           => UI_From_Int (I + Source_Low),
                         Expr_Type       => Index_T,
                         Source_Location =>
                           Internal_Source_Location),
                    I_Type     => Component_Source,
                    Location   => Internal_Source_Location),
                 New_Type       => Component_Dest,
                 A_Symbol_Table => Global_Symbol_Table),
            I_Type     => Component_Dest,
            Location   => Source_Location);
      end loop;
   end Copy_Array_Static;

   -------------------------
   -- Get_Array_Flat_Size --
   -------------------------

   function Get_Array_Flat_Size (The_Array : Node_Id) return Irep is
      Bounds : constant Static_And_Dynamic_Bounds :=
        Multi_Dimension_Flat_Bounds (The_Array);
   begin
      return Get_Array_Size_From_Bounds (Bounds).Dynamic_Index;
   end Get_Array_Flat_Size;

   --------------------------------
   -- Get_Array_Size_From_Bounds --
   --------------------------------

   function Get_Array_Size_From_Bounds (Bounds : Static_And_Dynamic_Bounds)
                                        return Static_And_Dynamic_Index
   is
      Result : Static_And_Dynamic_Index;
   begin
      if Bounds.Has_Static_Bounds then
         Result.Is_Static     := True;
         Result.Static_Index  :=
           UI_From_Int (Bounds.High_Static - Bounds.Low_Static + 1);
         Result.Dynamic_Index :=
           Integer_Constant_To_Expr
             (Value           => Result.Static_Index,
              Expr_Type       => Index_T,
              Source_Location => Internal_Source_Location);
      else
         Result.Is_Static     := False;
         Result.Static_Index  := Uint_0;
         Result.Dynamic_Index :=
           Make_Op_Add
             (Rhs             => Integer_Constant_To_Expr
                (Value           => Uint_1,
                 Expr_Type       => Index_T,
                 Source_Location => Internal_Source_Location),
              Lhs             => Make_Op_Sub
                (Rhs             => Bounds.Low_Dynamic,
                 Lhs             => Bounds.High_Dynamic,
                 Source_Location => Internal_Source_Location,
                 I_Type          => Index_T),
              Source_Location => Internal_Source_Location,
              I_Type          => Index_T);
      end if;
      return Result;
   end Get_Array_Size_From_Bounds;

   -----------------
   -- Get_Bounds --
   ---------------

   function Get_Bounds_From_Index (Index : Node_Id) return Dimension_Bounds
   is
      Bounds : constant Node_Id := Get_Range (Index);
      --  The front-end sometimes rewrites nodes giving them a different
      --  Goto requires the original identifier.
      Low  : constant Irep :=
        Do_Expression (Original_Node (Low_Bound (Bounds)));
      High : constant Irep :=
        Do_Expression (Original_Node (High_Bound (Bounds)));
   begin
      return (Low =>
                Typecast_If_Necessary
                  (Expr           => Low,
                   New_Type       => Index_T,
                   A_Symbol_Table => Global_Symbol_Table),
              High =>
                Typecast_If_Necessary
                  (Expr           => High,
                   New_Type       => Index_T,
                   A_Symbol_Table => Global_Symbol_Table));
   end Get_Bounds_From_Index;

   --------------------------
   -- Get_Dimension_Bounds --
   --------------------------

   function Get_Dimension_Bounds (N : Node_Id; Dim : Pos; Index : Node_Id)
                                  return Dimension_Bounds
   is
      Source_Location  : constant Irep := Get_Source_Location (N);
      Array_Node       : constant Node_Id :=
        (if Nkind (N) = N_Explicit_Dereference then
              Prefix (N)
         else
            N);
      N_Kind           : constant Node_Kind := Nkind (Array_Node);
      N_Entity         : constant Entity_Id :=
        (if N_Kind = N_Attribute_Reference then
           (if Get_Attribute_Id (Attribute_Name (Array_Node)) =
                Attribute_Image
            then
               Standard_String
            else
               Etype (Array_Node))
         elsif N_Kind = N_Defining_Identifier then
              Array_Node
         elsif N_Kind in N_Has_Entity then
            Entity (Array_Node)
         elsif N_Kind in N_Has_Etype then
            Etype (Array_Node)
         else
            Defining_Identifier (Array_Node));

      Pre_1_Array_Type  : constant Entity_Id :=
        (if Is_Type (N_Entity) then
              N_Entity
         else
            Etype (N_Entity));
      Pre_2_Array_Type : constant Entity_Id :=
        (if Is_Access_Type (Pre_1_Array_Type) then
              Designated_Type (Pre_1_Array_Type)
         else
            Pre_1_Array_Type);
      Array_Type       : constant Entity_Id :=
        Underlying_Type (Pre_2_Array_Type);

      --  If the Node represents an entity with an unconstrained array type
      --  and is a function call or an access entity then an analysis time
      --  view of the node is required to obtain the actual bounds.
      --  If the entity is unconstrained and an access type a dereference is
      --  required.
      --  Restricting the application of Do_Expression to just these
      --  scenarios avoids infinite recursion.
      Array_Irep       : constant Irep :=
        (if not Is_Constrained (Array_Type) then
             (if Is_Access_Type (Pre_1_Array_Type) then
                     Make_Dereference_Expr
                (Object          => Do_Expression (Array_Node),
                 Source_Location => Source_Location,
                 I_Type          => Do_Type_Reference (Array_Type))
              elsif N_Kind = N_Function_Call then
                 Do_Expression (Array_Node)
              else
                 Ireps.Empty)
         else
            Ireps.Empty);
   begin
      --  In lieu of pre-condition
      pragma Assert (Is_Array_Type (Array_Type));
      if not (Is_Object (N_Entity) or else Nkind (Index) = N_Range
                     or else Is_Constrained (Array_Type)
                     or else N_Kind = N_Function_Call
              or else Is_Access_Type (Pre_1_Array_Type))
      then
         Report_Unhandled_Node_Empty
           (N        => N,
            Fun_Name => "Get_Dimension_Bounds",
            Message  => "Unhandled node sort");
         return Dimension_Bounds'
           (Low  => Index_T_Zero,
            High => Index_T_One);
      end if;
      if Ekind (Array_Type) = E_String_Literal_Subtype then
         declare
            Lower : constant Irep := Typecast_If_Necessary
              (Expr           => Do_Expression
                 (String_Literal_Low_Bound (Array_Type)),
               New_Type       => Index_T,
               A_Symbol_Table => Global_Symbol_Table);
            Len   : constant Irep := Integer_Constant_To_Expr
              (Value           => String_Literal_Length (Array_Type),
               Expr_Type       => Index_T,
               Source_Location => Source_Location);
            Upper : constant Irep := Make_Op_Add
              (Rhs             => Index_T_One,
               Lhs             => Make_Op_Sub
                 (Rhs             => Lower,
                  Lhs             => Len,
                  Source_Location => Source_Location,
                  I_Type          => Index_T),
               Source_Location => Source_Location,
               I_Type          => Source_Location);
         begin
            return Dimension_Bounds'
              (Low  => Lower,
               High => Upper);
         end;
      elsif Is_Constrained (Array_Type) then
         return Get_Bounds_From_Index (Index);
      elsif N_Kind = N_Function_Call or else
        Is_Access_Type (Pre_1_Array_Type)
      then
         --  It is a call to a function with an unconstrained array result
         --  or a dereferenced pointer to an unconstrained array entity.
         if Is_Unconstrained_Array_Result (Array_Irep) then
            return Get_Bounds_From_Struc (Array_Irep, Dim);
         else
            Report_Unhandled_Node_Empty
              (N        => N,
               Fun_Name => "Get_Dimension_Bounds",
               Message  =>
                 "Unconstrained array result/access has unknown bounds");
            return Dimension_Bounds'
              (Low  => Index_T_Zero,
               High => Index_T_One);
         end if;
      elsif Is_Object (N_Entity) then
         declare
            Dim_String_Pre : constant String := Pos'Image (Dim);
            Dim_String     : constant String :=
              Dim_String_Pre (2 .. Dim_String_Pre'Last);

            Index_Etype : constant Entity_Id :=
              Etype (case Nkind (Index) is
                        when N_Defining_Identifier | N_Range =>
                           Index,
                        when N_Identifier | N_Expanded_Name =>
                           Entity (Index),
                        when others =>
                           Defining_Identifier (Index));
            Object_Name    : constant String := Unique_Name (N_Entity);

            First_Var      : constant String :=
              Object_Name & First_Var_Str & Dim_String;
            Last_Var       : constant String :=
              Object_Name & Last_Var_Str & Dim_String;

            First_Sym      : constant Irep :=
              Make_Symbol_Expr
                (Source_Location => Source_Location,
                 I_Type          => Do_Type_Reference (Index_Etype),
                 Identifier      => First_Var);
            Last_Sym      : constant Irep :=
              Make_Symbol_Expr
                (Source_Location => Source_Location,
                 I_Type          => Do_Type_Reference (Index_Etype),
                 Identifier      => Last_Var);
         begin

            if Global_Symbol_Table.Contains (Intern (First_Var)) and
              Global_Symbol_Table.Contains (Intern (Last_Var))
            then
               return Dimension_Bounds'
                 (Low  => Typecast_If_Necessary
                    (Expr           => First_Sym,
                     New_Type       => Index_T,
                     A_Symbol_Table => Global_Symbol_Table),
                  High => Typecast_If_Necessary
                    (Expr           => Last_Sym,
                     New_Type       => Index_T,
                     A_Symbol_Table => Global_Symbol_Table));
            else
               Report_Unhandled_Node_Empty
                 (N        => N,
                  Fun_Name => "Get_Dimension_Bounds",
                  Message  => "Unconstrained array has no defined bounds");
               return Dimension_Bounds'
                 (Low  => Index_T_Zero,
                  High => Index_T_Zero);
            end if;
         end;
      else
         Report_Unhandled_Node_Empty
           (N        => N,
            Fun_Name => "Get_Dimension_Bounds",
            Message  => "Unsupported unconstrained array");
         return Dimension_Bounds'
           (Low  => Index_T_Zero,
            High => Index_T_One);
      end if;
   end Get_Dimension_Bounds;

   --------------------------
   -- Get_Pointer_To_Array --
   --------------------------

   function Get_Pointer_To_Array (The_Array : Irep; Comp_I_Type : Irep)
                                  return Irep
   is
      Source_Location : constant Irep := Get_Source_Location (The_Array);
      Res : Irep;
   begin
      case Get_Array_Sort (The_Array) is
         when An_I_Array =>
            Res := Make_Address_Of_Expr
              (Object          => Make_Index_Expr
                 (I_Array         => The_Array,
                  Index           => Index_T_Zero,
                  Source_Location => Source_Location,
                  I_Type          => Comp_I_Type,
                  Range_Check     => False),
               Source_Location => Source_Location,
               I_Type          => Make_Pointer_Type (Comp_I_Type),
               Range_Check     => False);
            if Kind (Get_Type (Res)) /= I_Pointer_Type then
               Report_Unhandled_Node_Empty
                 (N        => Types.Empty,
                  Fun_Name => "Get_Pointer_To_Array",
                  Message  => "Not getting ponter from An_I_Array");
            end if;
         when A_Pointer_To_Elem =>
            --  The_Array is already a pointer - nothing to be done.
            Res := The_Array;
            if Kind (Get_Type (Res)) /= I_Pointer_Type then
               Report_Unhandled_Node_Empty
                 (N        => Types.Empty,
                  Fun_Name => "Get_Pointer_To_Array",
                  Message  => "Not getting ponter from A_Pointer_To_Elem");
            end if;
         when A_Bounded =>
            if Kind (Get_Base_I_Type (The_Array, Global_Symbol_Table)) /=
              I_Struct_Type
            then
               Report_Unhandled_Node_Empty
                 (N        => Types.Empty,
                  Fun_Name => "Get_Pointer_To_Array",
                  Message  => "A_Bounded is not a struc type");
            end if;
            Res := Get_Array_From_Struc (The_Array, Comp_I_Type);
            if Kind (Get_Type (Res)) /= I_Pointer_Type then
               Report_Unhandled_Node_Empty
                 (N        => Types.Empty,
                  Fun_Name => "Get_Pointer_To_Array",
                  Message  => "Not getting ponter from A_Bounded");
            end if;
         when A_Pointer_To_Bounded =>
            if Kind (Get_Type
                     (Make_Dereference_Expr
                        (Object          => The_Array,
                         Source_Location => Source_Location,
                         I_Type          =>
                           Make_Pointer_Type (Make_Void_Type))))
              /= I_Struct_Tag_Type
            then
               Report_Unhandled_Node_Empty
              (N        => Types.Empty,
               Fun_Name => "Get_Pointer_To_Array",
               Message  => "A_Pointer_To_Bounded is not a struc type");
            end if;
            Res := Get_Array_From_Struc
              (Make_Dereference_Expr
                 (Object          => The_Array,
                  Source_Location => Source_Location,
                  I_Type          => Make_Pointer_Type (Make_Void_Type)),
               Comp_I_Type);
            if Kind (Get_Type (Res)) /= I_Pointer_Type then
               Report_Unhandled_Node_Empty
                 (N        => Types.Empty,
                  Fun_Name => "Get_Pointer_To_Array",
                  Message  => "Not getting ponter from A_Pointer_To_Bounded");
            end if;
         when An_I_String =>
            Res := Make_Address_Of_Expr
              (Object          => The_Array,
               Source_Location => Source_Location,
               I_Type          => Make_Pointer_Type (Make_String_Type));
         when Unexpected =>
            Res := Report_Unhandled_Node_Irep
              (N        => Types.Empty,
               Fun_Name => "Get_Pointer_To_Array",
               Message  =>
                  "Attempting to get a pointer to an unexpected array sort");
      end case;
      return Res;
   end Get_Pointer_To_Array;

   ---------------
   -- Get_Range --
   ---------------

   function Get_Range (Index : Node_Id) return Node_Id is
     (if Nkind (Index) = N_Range
      then
      --  It is a range
         Index
      elsif Nkind (Index) = N_Subtype_Indication then
         --  It is a subtype with constraint
         Scalar_Range (Etype (Index))
      else
      --  It is a subtype mark
         Scalar_Range (Entity (Index)));

   ---------------------------
   -- Get_Bounds_From_Struc --
   ---------------------------

   function Get_Bounds_From_Struc (Array_Struc : Irep; Dimension   : Pos)
                                   return Dimension_Bounds
   is
      Source_Location : constant Irep := Get_Source_Location (Array_Struc);
      Unconstr_I_Type : constant Irep :=
        Get_Base_I_Type (Array_Struc, Global_Symbol_Table);
      Comp_List  : constant Irep_List :=
        Get_Component (Get_Components (Unconstr_I_Type));
      List_Cur        : constant List_Cursor := List_First (Comp_List);
      Bounds_Comp     : constant Irep := List_Element (Comp_List, List_Cur);
      Bounds_Array    : constant Irep := Make_Member_Expr
        (Compound         => Array_Struc,
         Source_Location  => Source_Location,
         I_Type           => Get_Type (Bounds_Comp),
         Component_Name   => Array_Struc_Bounds);
      First_Expr   : constant Irep :=
        Make_Index_Expr
          (I_Array         => Bounds_Array,
           Index           => Integer_Constant_To_Expr
             (Value           => Bounds_First (Dimension),
              Expr_Type       => Index_T,
              Source_Location => Source_Location),
           Source_Location => Source_Location,
           I_Type          => Bounds_Component);
      Last_Expr   : constant Irep :=
        Make_Index_Expr
          (I_Array         => Bounds_Array,
           Index           => Integer_Constant_To_Expr
             (Value           => Bounds_Last (Dimension),
              Expr_Type       => Index_T,
              Source_Location => Source_Location),
           Source_Location => Source_Location,
           I_Type          => Bounds_Component);
   begin
      return Dimension_Bounds'
        (Low  => First_Expr,
        High => Last_Expr);
   end Get_Bounds_From_Struc;

  --------------------------
  -- Get_Array_From_Struc --
  --------------------------

   function Get_Array_From_Struc (Array_Struc : Irep;
                                  Comp_Type    : Irep) return Irep is
      Actual_Type : constant Irep :=
        (if Kind (Follow_Symbol_Type (Comp_Type, Global_Symbol_Table)) =
           I_C_Enum_Type
         then
         --  The actual type will be a unsignedbv but the size is unknown.
         --  Use the maximium for an enumeration.
            Make_Unsignedbv_Type (32)
         else
            Comp_Type);
   begin
      return
        (Make_Member_Expr
        (Compound         => Array_Struc,
         Source_Location  => Get_Source_Location (Array_Struc),
         I_Type           => Make_Pointer_Type (Actual_Type),
         Component_Name   => Array_Struc_Data,
         Range_Check      => False));
   end Get_Array_From_Struc;

  ----------------------
  -- Init_Array_Struc --
  ----------------------

   procedure Init_Array_Struc (Block       : Irep;
                               Array_Struc : Irep;
                               Array_Ptr   : Irep;
                               Location    : Irep;
                               Bounds      : Bounds_Array)
   is
      Array_Ptr_Type      : constant Irep := Get_Type (Array_Ptr);
      --  Define the Array_Struc_Bounds array type
      Bounds_Array_Type   : constant Irep :=
        Make_Array_Type
          (I_Subtype => Bounds_Component,
           Size      => Integer_Constant_To_Expr
             (Value           => UI_From_Int (Int (Bounds'Length)),
              Expr_Type       => Index_T,
              Source_Location => Location));

      --  Get the bounds field from the Array_Struc.
      Bounds_Field        : constant Irep :=
        Make_Member_Expr
          (I_Type   => Bounds_Array_Type,
           Compound => Array_Struc,
           Component_Name => Array_Struc_Bounds,
           Source_Location => Location);

      --  Get the data field from the Array_Struc.
      Data_Field           : constant Irep :=
        Make_Member_Expr
          (I_Type   => Array_Ptr_Type,
           Compound => Array_Struc,
           Component_Name => Array_Struc_Data,
           Source_Location => Location);
   begin
      --  First fill in the Array_Struc_Bounds Array.
      --  The precondition guarantees the Ada Bounds array parameter
      --  is based at 0
      for I in Bounds'Range loop
         Append_Op (Block,
                    Make_Code_Assign
                      (Rhs             => Bounds (I),
                       Lhs             =>
                         Make_Index_Expr
                           (I_Array         => Bounds_Field,
                            Index           => Integer_Constant_To_Expr
                              (Value           =>  UI_From_Int (Int (I)),
                               Expr_Type       => Index_T,
                               Source_Location => Location),
                            Source_Location => Location,
                            I_Type          => Bounds_Component,
                            Range_Check     => False),
                       Source_Location => Location,
                       I_Type          => Bounds_Component,
                       Range_Check     => False));
      end loop;

      --  Now assign the array pointer.
      Append_Op (Block,
                 Make_Code_Assign
                   (Rhs             => Array_Ptr,
                    Lhs             => Data_Field,
                    Source_Location => Location,
                    I_Type          => Array_Ptr_Type,
                    Range_Check     => False));
   end Init_Array_Struc;

  ---------------------------
  -- Make_Array_Struc_Type --
  ---------------------------

   function Make_Array_Struc_Type (Comp_Type  : Irep;
                                   Location   : Irep;
                                   Dimensions : Pos) return Irep
   is
      --  The Array_Struc has the form
      --  {Bounds_Component bounds[N_Dimensions * 2], Comp_Type data*}
      Bound_Pairs    : constant Irep :=
        Integer_Constant_To_Expr
          (Value           => Bounds_Size (Dimensions),
           Expr_Type       => Bounds_Component,
           Source_Location => Location);
      Bounds_Array   : constant Irep :=
        Make_Array_Type
          (I_Subtype => Bounds_Component,
           Size      => Bound_Pairs);

      Array_Struc_Comps : constant Irep := Make_Struct_Union_Components;

      Array_Struc : constant Irep :=
        Make_Struct_Type (Tag        => Array_Struc_Tag,
                          Components => Array_Struc_Comps);

      Bounds_Comp    : constant Irep :=
        Make_Struct_Component
          (Name => Array_Struc_Bounds,
           Ty   => Bounds_Array);

      Data_Comp      : constant Irep :=
        Make_Struct_Component
          (Name => Array_Struc_Data,
           Ty   => Make_Pointer_Type (Comp_Type));

   begin
      Append_Component (Array_Struc_Comps, Bounds_Comp);
      Append_Component (Array_Struc_Comps, Data_Comp);
      return Array_Struc;
   end Make_Array_Struc_Type;

  ------------------------------
  -- Make_Resolved_Index_Expr --
  ------------------------------

   function Make_Resolved_Index_Expr (The_Array  : Irep;
                                      Zero_Index : Irep;
                                      I_Type     : Irep;
                                      Location   : Irep) return Irep
   is
      Raw_Array_I_Type   : constant Irep := Get_Type (The_Array);
      Deref_Array_I_Type : constant Irep :=
        (if Kind (Raw_Array_I_Type) = I_Dereference_Expr then
              Get_Subtype (Raw_Array_I_Type)
         else
            Raw_Array_I_Type);
      Arr_Sort : constant Array_Sort := Get_Array_Sort (The_Array);
      --  If the array components represent an enumeration they are
      --  modelled as an unsigned bit vector within the array.  An element
      --  has to be extracted from the array as an unsignedbv component and
      --  then type converted to the correct enumeration type.
      Is_An_Enum       : constant Boolean :=
        Kind (Follow_Symbol_Type
              (I_Type, Global_Symbol_Table)) = I_C_Enum_Type;
      --  The number of bits used by the enumertion type is not known here
      --  for the purposes of accessing a bounded array,
      --  so the maximum of 32 is assumed.
      Max_Enum         : constant Irep := Make_Unsignedbv_Type (32);

      Raw_Elem_I_Type  : constant Irep :=
        (if not Is_An_Enum then
            I_Type
         else
           (case Arr_Sort is
               when An_I_String =>
                  I_Type,
               when An_I_Array =>
                  Get_Subtype (Get_Type (The_Array)),
               when A_Pointer_To_Elem =>
                  Get_Subtype (Deref_Array_I_Type),
               when A_Bounded | A_Pointer_To_Bounded =>
                  Get_Subtype (Get_Pointer_To_Array (The_Array, Max_Enum)),
               when Unexpected =>
                  I_Type));

      function Make_Array_Pointer_Index (Arr_Pointer : Irep;
                                         Zero_Index  : Irep) return Irep;
      function Make_Array_Pointer_Index (Arr_Pointer : Irep;
                                         Zero_Index  : Irep) return Irep is
      begin
         return (Make_Dereference_Expr
                 (Object          => Typecast_If_Necessary
                  (Expr           => Make_Op_Add
                   (Rhs             => Zero_Index,
                    Lhs             => Arr_Pointer,
                    Source_Location => Location,
                    I_Type          => Make_Pointer_Type (Raw_Elem_I_Type)),
                   New_Type       => Make_Pointer_Type (Raw_Elem_I_Type),
                   A_Symbol_Table => Global_Symbol_Table),
                  Source_Location => Location,
                  I_Type          => Make_Pointer_Type (Raw_Elem_I_Type)));
      end Make_Array_Pointer_Index;

      Indexed_Data_Pre : constant Irep :=
        (case Arr_Sort is
            when An_I_Array =>
               Make_Index_Expr
           (I_Array         => The_Array,
            Index           => Zero_Index,
            Source_Location => Location,
            I_Type          => Raw_Elem_I_Type,
            Range_Check     => False),
            when A_Pointer_To_Elem | A_Bounded | A_Pointer_To_Bounded =>
               Make_Array_Pointer_Index
           (Get_Pointer_To_Array (The_Array, Raw_Elem_I_Type), Zero_Index),
            when An_I_String =>
               Report_Unhandled_Node_Irep
           (N        => Types.Empty,
            Fun_Name => "Make_Resolved_Index_Expr",
            Message  => "Attempting to index into an I_String_Type"),
            when Unexpected =>
               Report_Unhandled_Node_Irep
           (N        => Types.Empty,
            Fun_Name => "Make_Resolved_Index_Expr",
            Message  => "Attempting to index an unexpected array sort"));

      Indexed_Data     : constant Irep :=
        (if Is_An_Enum then
            Make_Op_Typecast
           (Op0             => Indexed_Data_Pre,
            Source_Location => Location,
            I_Type          => I_Type)
         else
            Indexed_Data_Pre);
   begin
      return Indexed_Data;
   end Make_Resolved_Index_Expr;

   --------------------------
   -- Make_Simple_For_Loop --
   --------------------------

   function Make_Simple_For_Loop (Loop_Var,  --  The loop variable
                                  First,     --  The initial value of loop var
                                  Last,      --  The final value of loop var
                                  Loop_Body, --  The body, using loop var
                                  Source_Location : Irep) return Irep
   is
      Loop_Init : constant Irep := Make_Code_Assign
        (Lhs => Loop_Var,
         Rhs => First,
         Source_Location => Source_Location);

      Loop_Cond : constant Irep :=
        Make_Op_Geq (Rhs             => Loop_Var,
                     Lhs             => Last,
                     Source_Location => Source_Location,
                     Overflow_Check  => False,
                     I_Type          => Make_Bool_Type);

      Loop_Inc : constant Irep := Inc_Index (Loop_Var);

      Loop_Post : constant Irep :=
        Make_Side_Effect_Expr_Assign
          (Lhs => Loop_Var,
           Rhs => Loop_Inc,
           Source_Location => Source_Location,
           I_Type => Get_Type (Loop_Var));

   begin
      return Make_Code_For
        (Loop_Body       => Loop_Body,
         Cond            => Loop_Cond,
         Init            => Loop_Init,
         Iter            => Loop_Post,
         Source_Location => Source_Location);
   end Make_Simple_For_Loop;

   ---------------------
   -- Make_Zero_Index --
   ---------------------

   function Make_Zero_Index (Index, First, Location : Irep) return Irep is
      Index_IT : constant Irep :=
        (if Get_Type (First) = Index_T then
              Index
         else
            Make_Op_Typecast
           (Op0             => Index,
            Source_Location => Location,
            I_Type          => Index_T));
      First_IT : constant Irep :=
        (if Get_Type (First) = Index_T then
              First
         else
            Make_Op_Typecast
           (Op0             => First,
            Source_Location => Location,
            I_Type          => Index_T));
   begin
      return
        Make_Op_Sub
          (Rhs             => First_IT,
           Lhs             => Index_IT,
           Source_Location => Location,
           I_Type          => Index_T);
   end Make_Zero_Index;

   function Make_Zero_Index (Index : Irep; First : Int; Location : Irep)
                             return Irep is
     (Make_Zero_Index
        (Index    => Index,
         First    => Integer_Constant_To_Expr
           (Value           => UI_From_Int (First),
            Expr_Type       => Index_T,
            Source_Location => Location),
         Location => Location));

   function Multi_Dimension_Flat_Bounds (Array_Node : Node_Id)
                                         return Static_And_Dynamic_Bounds
   is
      Source_Location : constant Irep := Get_Source_Location (Array_Node);
      --  The front-end ensures that the array has at least one dimension.
      Array_Node_Kind : constant Node_Kind := Nkind (Array_Node);
      Array_Is_Object : constant Boolean :=
        (Array_Node_Kind in N_Entity and then Is_Object (Array_Node))
        or else
          (Array_Node_Kind in N_Has_Entity and then
             (Nkind (Entity (Array_Node)) in N_Entity and then
              Is_Object (Entity (Array_Node))));
      Array_Type   : constant Entity_Id :=
        (case Array_Node_Kind is
            when N_Full_Type_Declaration | N_Subtype_Declaration =>
               Underlying_Type (Defining_Identifier (Array_Node)),
            when N_Object_Declaration | N_Object_Renaming_Declaration =>
               Underlying_Type (Etype (Defining_Identifier (Array_Node))),
            when N_Identifier | N_Expanded_Name =>
               Underlying_Type (Etype (Entity (Array_Node))),
            when N_Attribute_Reference =>
           (if Get_Attribute_Id (Attribute_Name (Array_Node)) = Attribute_Image
            then
               Standard_String
            else
               Types.Empty),
            when others =>
           (if Array_Node_Kind in N_Entity and then
            Is_Type (Array_Node)
            then
               Underlying_Type (Array_Node)
            elsif Array_Node_Kind in N_Has_Etype then
               Underlying_Type (Etype (Array_Node))
            else
                 Types.Empty));
   begin
      if not Present (Array_Type) then
         Report_Unhandled_Node_Empty
           (N        => Array_Node,
            Fun_Name => "Multi_Dimension_Flat_Bounds",
            Message  => "Unsupported array node kind");
         return Static_And_Dynamic_Bounds'
           (Is_Unconstrained  => True,
            Has_Static_Bounds => False,
            Low_Static        => 0,
            High_Static       => 0,
            Low_Dynamic       => Index_T_Zero,
            High_Dynamic      => Index_T_Zero);
      end if;

      --  First chek if the string is due to an application of the attribute
      --  'Image.  Currently this is not supported.
      if Array_Node_Kind = N_Attribute_Reference then
         --  The case when it the attribute reference is not an
         --  Image attribute by the previous report of an unhandled node.
         Report_Unhandled_Node_Empty
           (N        => Array_Node,
            Fun_Name => "Multi_Dimension_Flat_Bounds",
            Message  => "The attribute Image is unsupported");
         --  The unsupported 'Image attribute returns the string literal
         --  "Unsupported'Image", so the length of this string is used to
         --  return an error recovery value.
         declare
            Error_String : constant String := "Unsupported'Image";
            Err_Str_Last : constant Int := Int (Error_String'Length - 1);
         begin
            return Static_And_Dynamic_Bounds'
              (Is_Unconstrained  => False,
               Has_Static_Bounds => True,
               Low_Static        => 0,
               High_Static       => Err_Str_Last,
               Low_Dynamic       => Index_T_Zero,
               High_Dynamic      => Integer_Constant_To_Expr
                 (Value           => UI_From_Int (Err_Str_Last),
                  Expr_Type       => Index_T,
                  Source_Location => Source_Location));
         end;
      end if;

      --  Check to see if the array is  string literal
      --  Process and return if it is.
      if Ekind (Array_Type) = E_String_Literal_Subtype then
         declare
            Str_Lit_Length          : constant Uint :=
              String_Literal_Length (Array_Type);

            --  The goto array representing the string literal must
            --  index from 0.
            Char_Array_Low_Static    : constant Uint := Uint_0;

            --  As string literals are always stored by the front-end
            --  starting at index 1, the string length is the number of
            --  charaters inthe string.  Since goto arrays are indexed from
            --  0 the high bound of the char array representing the string
            --  literal is the string literal lenght - 1.
            Char_Array_High_Static   : constant Uint :=
              Str_Lit_Length - Uint_1;

            --  All goto arrays are indexed from 0
            Char_Array_Low_Irep      : constant Irep := Index_T_Zero;

            Char_Array_High_Irep   : constant Irep :=
              Integer_Constant_To_Expr
                (Value           => Char_Array_High_Static,
                 Expr_Type       => Index_T,
                 Source_Location => Source_Location);
         begin
            return Static_And_Dynamic_Bounds'
              (Is_Unconstrained  => False,
               Has_Static_Bounds => True,
               Low_Static        => UI_To_Int (Char_Array_Low_Static),
               High_Static       => UI_To_Int (Char_Array_High_Static),
               Low_Dynamic       => Char_Array_Low_Irep,
               High_Dynamic      => Char_Array_High_Irep);
         end;
      end if;

      --   Not a string literal.

      --  This test is placed first because an object may not be in the
      --  symbol table if this function is called a in an initialisation of
      --  the object's declaration but an unconstrained array result
      --  variable should not be treated in the same way as an
      --  ordinary object.
      if not Is_Constrained (Array_Type) and
        Nkind (Array_Node) = N_Function_Call
      then
         --  It is an unconstrained array result from a function call
         --  or it is the result variable of unconstrained array function
         return Flat_Bounds_From_Array_Struc
           (Array_Struc  => Do_Expression (Array_Node),
            N_Dimensions => Number_Dimensions (Array_Type));

      elsif Is_Constrained (Array_Type) or else Array_Is_Object then
         declare
            Dimension_Number  : Pos := 1;
            Dimension_Iter    : Node_Id := First_Index (Array_Type);
            Dimension_Range   : Node_Id := Get_Range (Dimension_Iter);
            Var_Dim_Bounds    : Irep := Ireps.Empty;
            Static_Array_Size : Uint := Uint_0;
         begin
            if Is_Constrained (Array_Type) and then
              Is_OK_Static_Range (Dimension_Range)
            then
               Static_Array_Size :=
                 Calculate_Static_Dimension_Length (Dimension_Range);
            else
               --  Bounds are variable or it is an array object of an
               --  unconstrained subtype.
               Var_Dim_Bounds := Calculate_Dimension_Length
                 (Get_Dimension_Bounds
                    (Array_Node, Dimension_Number, Dimension_Iter));
            end if;
            --  Multidimensional arrays are converted into a a single
            --  dimension of an appropriate length.
            --  This needs to be considered when indexing into, or
            --  assigning aggregates to a multidimensional array.
            Dimension_Iter := Next (Dimension_Iter);
            while Present (Dimension_Iter) loop
               Dimension_Number := Dimension_Number + 1;
               Dimension_Range := Get_Range (Dimension_Iter);
               if Is_Constrained (Array_Type) and then
                 Is_OK_Static_Range (Dimension_Range)
               then
                  Static_Array_Size := Static_Array_Size *
                    Calculate_Static_Dimension_Length (Dimension_Range);
               else
                  if Var_Dim_Bounds = Ireps.Empty then
                     Var_Dim_Bounds := Calculate_Dimension_Length
                       (Get_Dimension_Bounds
                          (Array_Node, Dimension_Number, Dimension_Iter));
                  else
                     Var_Dim_Bounds := Make_Op_Mul
                       (Rhs             => Calculate_Dimension_Length
                          (Get_Dimension_Bounds
                               (Array_Node,
                                Dimension_Number, Dimension_Iter)),
                        Lhs             => Var_Dim_Bounds,
                        Source_Location => Internal_Source_Location,
                        Overflow_Check  => False,
                        I_Type          => Index_T,
                        Range_Check     => False);
                  end if;
               end if;
               Dimension_Iter := Next (Dimension_Iter);
            end loop;

            declare
               Has_Static_Bounds : constant Boolean :=
                 Var_Dim_Bounds = Ireps.Empty;

               Static_Size : constant Irep :=
                 (if Static_Array_Size /= Uint_0 then
                     Integer_Constant_To_Expr
                    (Value           => Static_Array_Size,
                     Expr_Type       => Index_T,
                     Source_Location => Internal_Source_Location)
                  else
                     Ireps.Empty);

               Array_Size : constant Irep :=
                 (if Var_Dim_Bounds = Ireps.Empty then
                     Static_Size
                  elsif Static_Array_Size /= Uint_0 then
                     Make_Op_Mul
                    (Rhs             => Static_Size,
                     Lhs             => Var_Dim_Bounds,
                     Source_Location => Internal_Source_Location,
                     Overflow_Check  => False,
                     I_Type          => Index_T,
                     Range_Check     => False)
                  else
                     Var_Dim_Bounds);
            begin
               --  Goto arrays are indexed from 0.
               return Static_And_Dynamic_Bounds'
                 (Is_Unconstrained  => False,
                  Has_Static_Bounds => Has_Static_Bounds,
                  Low_Static        => 0,
                  High_Static       =>
                    (if Has_Static_Bounds then
                          UI_To_Int (Static_Array_Size - 1)
                     else
                        0),
                  Low_Dynamic       => Index_T_Zero,
                  High_Dynamic      =>
                    Make_Op_Sub
                      (Rhs             => Index_T_One,
                       Lhs             => Array_Size,
                       Source_Location => Internal_Source_Location,
                       Overflow_Check  => False,
                       I_Type          => Index_T,
                       Range_Check     => False));
            end;
         end;

      else
         declare
            Nondet_Index : constant Irep := Index_T_Zero;
            --              Make_Nondet_Expr
            --                (Source_Location => Internal_Source_Location,
            --                 I_Type          => Index_T,
            --                 Range_Check     => False);
         begin
            return Static_And_Dynamic_Bounds'
              (Is_Unconstrained  => True,
               Has_Static_Bounds => False,
               Low_Static        => 0,
               High_Static       => 0,
               Low_Dynamic       => Nondet_Index,
               High_Dynamic      => Nondet_Index);
         end;
      end if;
   end Multi_Dimension_Flat_Bounds;

   function Flat_Bounds_From_Array_Struc (Array_Struc  : Irep;
                                          N_Dimensions : Pos)
                                          return Static_And_Dynamic_Bounds
   is
     (Static_And_Dynamic_Bounds'
        (Is_Unconstrained  => False,
         Has_Static_Bounds => False,
         Low_Static        => 0,
         High_Static       => 0,
         Low_Dynamic       => Index_T_Zero,
         High_Dynamic      => Make_Op_Sub
           (Rhs             => Index_T_One,
            Lhs             => Get_Size_From_Array_Struc
              (Array_Struc  => Array_Struc,
               N_Dimensions => N_Dimensions),
            Source_Location => Get_Source_Location (Array_Struc),
            I_Type          => Index_T)));

   function Get_Size_From_Array_Struc (Array_Struc  : Irep;
                                       N_Dimensions : Pos) return Irep
   is
      Source_Location : constant Irep :=
        Get_Source_Location (Array_Struc);
      Unconstr_I_Type : constant Irep :=
        Get_Base_I_Type (Array_Struc, Global_Symbol_Table);
      Comp_List  : constant Irep_List :=
        Get_Component (Get_Components (Unconstr_I_Type));
      List_Cur   : constant List_Cursor := List_First (Comp_List);
      Bounds     : constant Irep := List_Element (Comp_List, List_Cur);
      Bounds_Arr : constant Irep := Make_Member_Expr
        (Compound         => Array_Struc,
         Source_Location  => Source_Location,
         I_Type           => Get_Type (Bounds),
         Component_Name   => Array_Struc_Bounds);

      Accum_Size : Irep := Index_T_Zero;
   begin
      for I in 1 .. N_Dimensions loop
         declare
            --  Goto arrays are indexd from 0.
            First_Index : constant Irep := Integer_Constant_To_Expr
              (Value           => UI_From_Int (I - 1),
               Expr_Type       => Index_T,
               Source_Location => Source_Location);
            Last_Index  : constant Irep := Integer_Constant_To_Expr
              (Value           => UI_From_Int (I),
               Expr_Type       => Index_T,
               Source_Location => Source_Location);

            Dim_Bounds : constant Dimension_Bounds :=
              Dimension_Bounds'
                (Low  => Make_Index_Expr
                   (I_Array         => Bounds_Arr,
                    Index           => First_Index,
                    Source_Location => Source_Location,
                    I_Type          => Int32_T),
                 High =>  Make_Index_Expr
                   (I_Array         => Bounds_Arr,
                    Index           => Last_Index,
                    Source_Location => Source_Location,
                    I_Type          => Int32_T));

            Dim_Length : constant Irep :=
              Calculate_Dimension_Length (Dim_Bounds);

         begin
            if I = 1 then
               Accum_Size := Dim_Length;
            else
               Accum_Size := Make_Op_Mul
                 (Rhs             => Dim_Length,
                  Lhs             => Accum_Size,
                  Source_Location => Source_Location,
                  I_Type          => Index_T);
            end if;
         end;
      end loop;
      return Accum_Size;
   end Get_Size_From_Array_Struc;

   function Zero_Based_Bounds (The_Array : Node_Id)
                               return Static_And_Dynamic_Bounds
   is
      Array_Is_Slice    : constant Boolean := Nkind (The_Array) = N_Slice;
      Array_Type        : constant Entity_Id :=
        (if Array_Is_Slice then
              Underlying_Type (Etype (Prefix (The_Array)))
         else
            Underlying_Type (Etype (The_Array)));
   begin
      if not Array_Is_Slice then
         --  The array may be multidimensional
         return Multi_Dimension_Flat_Bounds (Array_Type);
      else
         --  It's a slice. A slice can only be one-dimensional.
         return Zero_Based_Slice_Bounds
           (The_Slice        => The_Array,
            Underlying_Array => Array_Type);
      end if;
   end Zero_Based_Bounds;

   function Zero_Based_Slice_Bounds (The_Slice        : Node_Id;
                                     Underlying_Array : Entity_Id)
                                     return Static_And_Dynamic_Bounds
   is
      Source_Location   : constant Irep := Get_Source_Location (The_Slice);
      Slice_Range       : constant Entity_Id := Discrete_Range (The_Slice);
      Has_Static_Bounds : constant Boolean :=
        All_Dimensions_Static (Underlying_Array) and
        Is_OK_Static_Range (Slice_Range);
      First_Ix           : constant Node_Id := First_Index (Underlying_Array);
      Underlying_Range   : constant Node_Id :=
        (if Nkind (First_Ix) = N_Range then
              First_Ix
         else
            Scalar_Range (Etype (First_Ix)));
   begin
      if Has_Static_Bounds then
         declare
            Slice_Low             : constant Uint :=
              Expr_Value (Low_Bound (Slice_Range));
            Slice_High            : constant Uint :=
              Expr_Value (High_Bound (Slice_Range));
            Underlying_Array_Low  : constant Uint :=
              Expr_Value (Low_Bound (Underlying_Range));
            Low_Static            : constant Int :=
              UI_To_Int (Slice_Low - Underlying_Array_Low);
            High_Static           : constant Int :=
              UI_To_Int (Slice_High - Underlying_Array_Low);
         begin
            return Static_And_Dynamic_Bounds'
              (Is_Unconstrained  => False,
               Has_Static_Bounds => True,
               Low_Static        => Low_Static,
               High_Static       => High_Static,
               Low_Dynamic       =>
                 Integer_Constant_To_Expr
                   (Value           => UI_From_Int (Low_Static),
                    Expr_Type       => Index_T,
                    Source_Location => Source_Location),
               High_Dynamic      =>
                 Integer_Constant_To_Expr
                   (Value           => UI_From_Int (High_Static),
                    Expr_Type       => Index_T,
                    Source_Location => Source_Location));
         end;
      else
         declare
            Slice_Low             : constant Irep :=
              Typecast_If_Necessary
                (Expr           =>
                   Do_Expression (Low_Bound (Slice_Range)),
                 New_Type       => Index_T,
                 A_Symbol_Table => Global_Symbol_Table);
            Slice_High            : constant Irep :=
              Typecast_If_Necessary
                (Expr           =>
                   Do_Expression (High_Bound (Slice_Range)),
                 New_Type       => Index_T,
                 A_Symbol_Table => Global_Symbol_Table);
            Underlying_Array_Low  : constant Irep :=
              Typecast_If_Necessary
                (Expr           =>
                   Do_Expression (Low_Bound (Underlying_Range)),
                 New_Type       => Index_T,
                 A_Symbol_Table => Global_Symbol_Table);
            Low_Dynamic            : constant Irep :=
              Make_Op_Sub
                (Rhs             => Underlying_Array_Low,
                 Lhs             => Slice_Low,
                 Source_Location => Source_Location,
                 Overflow_Check  => False,
                 I_Type          => Index_T,
                 Range_Check     => False);
            High_Dynamic           : constant Irep :=
              Make_Op_Sub
                (Rhs             => Underlying_Array_Low,
                 Lhs             => Slice_High,
                 Source_Location => Source_Location,
                 Overflow_Check  => False,
                 I_Type          => Index_T,
                 Range_Check     => False);
         begin
            return Static_And_Dynamic_Bounds'
              (Is_Unconstrained  => False,
               Has_Static_Bounds => False,
               Low_Static        => 0,
               High_Static       => 0,
               Low_Dynamic       => Low_Dynamic,
               High_Dynamic      => High_Dynamic);
         end;
      end if;
   end Zero_Based_Slice_Bounds;

end Arrays.Low_Level;
