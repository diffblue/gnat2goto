with Nlists;                use Nlists;
with Uintp;                 use Uintp;

with Tree_Walk;             use Tree_Walk;
with Follow;                use Follow;

package body Arrays is

   --------------------------------
   -- Do_Aggregate_Literal_Array --
   --------------------------------

   function Do_Aggregate_Literal_Array (N : Node_Id) return Irep is
      Result_Type : constant Irep := Do_Type_Reference (Etype (N));

      function Build_Array_Lit_Func_Body (N : Node_Id) return Irep
        with Pre => Ekind (Etype (N)) in E_Array_Type | E_Array_Subtype,
        Post => Kind (Build_Array_Lit_Func_Body'Result) = I_Code_Block;

      function Build_Array_Lit_Func (N : Node_Id) return Symbol
        with Pre => Ekind (Etype (N)) in E_Array_Type | E_Array_Subtype,
        Post => not (Build_Array_Lit_Func'Result.Value = Ireps.Empty);

      function Make_No_Args_Func_Call_Expr (Fun_Symbol : Irep;
                                            Return_Type : Irep;
                                            Source_Loc : Source_Ptr)
                                            return Irep
        with Pre => (Kind (Fun_Symbol) = I_Symbol_Expr
                     and then Kind (Return_Type) in Class_Type),
        Post => Kind (Make_No_Args_Func_Call_Expr'Result) =
        I_Side_Effect_Expr_Function_Call;

      -------------------------------
      -- Build_Array_Lit_Func_Body --
      -------------------------------

      --  build the following function:
      --  struct array_struct {int first1; int last1; array_type* data; }
      --  array_lit() {
      --    array_type temp_literal[high_bound - low_bound + 1];
      --    temp_literal = { literal_1, .. literal_n };
      --    struct arrays_struct { int first1; int last1; array_type *data; }
      --      temp_array;
      --    temp_array.first1 = low_bound;
      --    temp_array.last1 = high_bound;
      --    temp_array.data = (array_type *)malloc((high_bound-low_bound+1)*
      --                                           sizeof(array_type));
      --    temp_lhs = memcpy(temp_array.data,
      --                      &temp_literal,
      --                      (high_bound-low_bound+1)*sizeof(array_type)));
      --    return temp_array;
      --  }
      function Build_Array_Lit_Func_Body (N : Node_Id) return Irep is

         Pos_Iter : Node_Id := First (Expressions (N));
         Source_Loc : constant Source_Ptr := Sloc (N);
         Bounds : constant Node_Id := Aggregate_Bounds (N);
         Low_Expr : constant Irep := Do_Expression (Low_Bound (Bounds));
         High_Expr : constant Irep := Do_Expression (High_Bound (Bounds));
         Index_Type_Node : constant Entity_Id := Etype (Etype (Bounds));
         Index_Type : constant Irep := Follow_Symbol_Type
           (Do_Type_Reference (Index_Type_Node),
            Global_Symbol_Table);
         Len_Expr : constant Irep :=
           Build_Array_Size (First      => Low_Expr,
                             Last       => High_Expr,
                             Idx_Type => Index_Type);
         Element_Type_Ent : constant Entity_Id := Get_Array_Component_Type (N);
         Element_Type : constant Irep := Do_Type_Reference (Element_Type_Ent);
         Bare_Array_Type : constant Irep :=
           Make_Array_Type (I_Subtype => Element_Type,
                            Size => Len_Expr);
         Literal_Temp : constant Irep :=
           Fresh_Var_Symbol_Expr (Bare_Array_Type, "array_literal");
         Array_Temp : constant Irep :=
           Fresh_Var_Symbol_Expr (Result_Type, "temp_array");
         Lhs_Temp : constant Irep :=
           Fresh_Var_Symbol_Expr (Make_Pointer_Type (Element_Type),
                                  "temp_lhs");
         Array_Expr : Irep;
         Result_Block : constant Irep := New_Irep (I_Code_Block);
         With_Mode : Boolean;
         Pos_Number : Natural := 0;

         --  NB: Component number seem to be ignored by CBMC
         --  We represent arrays as a structure of 3 members:
         --  0: first index
         --  1: last index
         --  2: data pointer
         --  Using the component numbers may be dropped in the future or it
         --  may be enforced.
         Data_Mem_Expr : constant Irep :=
           Make_Member_Expr (Compound         => Array_Temp,
                             Source_Location  => Source_Loc,
                             Component_Number => 2,
                             I_Type           =>
                               Make_Pointer_Type (Element_Type),
                             Component_Name   => "data");
         Array_Temp_Struct : constant Irep :=
           Make_Struct_Expr (Source_Location => Source_Loc,
                             I_Type          => Result_Type);
         Raw_Malloc_Call : constant Irep :=
           Make_Malloc_Function_Call_Expr (Num_Elem          => Len_Expr,
                                           Element_Type_Size =>
                                             Esize (Element_Type_Ent),
                                           Source_Loc        => Source_Loc);
         Malloc_Call_Expr : constant Irep :=
           Typecast_If_Necessary (Expr     => Raw_Malloc_Call,
                                  New_Type =>
                                    Make_Pointer_Type (Element_Type));
         Literal_Address : constant Irep :=
           Typecast_If_Necessary (Expr     => Make_Address_Of (Literal_Temp),
                                  New_Type =>
                                    Make_Pointer_Type (Element_Type));
         Memcpy_Call_Expr : constant Irep :=
           Make_Memcpy_Function_Call_Expr (Destination       => Data_Mem_Expr,
                                         Source            => Literal_Address,
                                         Num_Elem          => Len_Expr,
                                         Element_Type_Size =>
                                           Esize (Element_Type_Ent),
                                         Source_Loc        => Source_Loc);
      begin
         --  Handle an "others" splat expression if present:
         if Present (Component_Associations (N)) then
            --  Produce something like array_of(others_expr)
            --                         with 1 => 100, 2 => 200, ...
            --  We expect only one named operand (others => ...):
            if List_Length (Component_Associations (N)) /= 1 then
               return Report_Unhandled_Node_Irep (N,
                                               "Do_Aggregate_Literal_Array",
                                               "More than one named operand");
            end if;

            declare
               Others_Node : constant Node_Id :=
                 First (Component_Associations (N));
               Others_Choices : constant List_Id := Choices (Others_Node);
               Expr : constant Irep :=
                 Do_Expression (Expression (Others_Node));
            begin
               if List_Length (Others_Choices) /= 1 then
                  return Report_Unhandled_Node_Irep (N,
                                                 "Do_Aggregate_Literal_Array",
                                                 "More than one other choice");
               end if;
               if Nkind (First (Others_Choices)) /= N_Others_Choice then
                  return Report_Unhandled_Node_Irep (N,
                                                 "Do_Aggregate_Literal_Array",
                                                 "Wrong kind of other choice");
               end if;
               Array_Expr :=
                 Make_Op_Array_Of (I_Type => Bare_Array_Type,
                                   Op0 => Expr,
                                   Source_Location => Sloc (N));
            end;
            With_Mode := True;
         else
            Array_Expr := Make_Array_Expr (I_Type => Bare_Array_Type,
                                           Source_Location => Sloc (N));
            With_Mode := False;
         end if;

         Set_Type (Array_Expr, Bare_Array_Type);

         while Present (Pos_Iter) loop
            declare
               Expr : constant Irep := Do_Expression (Pos_Iter);
            begin
               if With_Mode then
                  declare
                     Pos_Constant : constant Irep := Integer_Constant_To_Expr
                       (Value => UI_From_Int (Int (Pos_Number)),
                        Expr_Type => Index_Type,
                        Source_Location => No_Location);
                     New_With : constant Irep :=
                       Make_With_Expr (Old => Array_Expr,
                                       Where => Pos_Constant,
                                       New_Value => Expr,
                                       I_Type => Bare_Array_Type,
                                       Source_Location => Sloc (N));
                  begin
                     Array_Expr := New_With;
                  end;
               else
                  Append_Operand (Array_Expr, Expr);
               end if;
            end;
            Next (Pos_Iter);
            Pos_Number := Pos_Number + 1;
         end loop;

         Append_Struct_Member (Array_Temp_Struct, Low_Expr);
         Append_Struct_Member (Array_Temp_Struct, High_Expr);
         Append_Struct_Member (Array_Temp_Struct, Malloc_Call_Expr);

         Append_Declare_And_Init (Symbol     => Literal_Temp,
                                  Value      => Array_Expr,
                                  Block      => Result_Block,
                                  Source_Loc => Source_Loc);
         --  As long as symex is field-insensitive we need to initialise the
         --  array structure with the information about allocated size.
         --  I.e. Create a temporary struct and assign it in one swoop to
         --  Array_Temp - so that Symex does not see the struct as having been
         --  changed after its creation and can therefore see it as constant -
         --  which means that the struct member that refers to "allocated size"
         --  remains visible/accessible.
         Append_Declare_And_Init (Symbol     => Array_Temp,
                                  Value      => Array_Temp_Struct,
                                  Block      => Result_Block,
                                  Source_Loc => Source_Loc);
         Append_Op (Result_Block,
                    Make_Code_Assign (Rhs             => Memcpy_Call_Expr,
                                      Lhs             => Lhs_Temp,
                                      Source_Location => Source_Loc));
         Append_Op (Result_Block,
                    Make_Code_Return (Return_Value    => Array_Temp,
                                      Source_Location => Source_Loc));

         return Result_Block;
      end Build_Array_Lit_Func_Body;

      --------------------------
      -- Build_Array_Lit_Func --
      --------------------------

      function Build_Array_Lit_Func (N : Node_Id) return Symbol is
         Func_Name : constant String := Fresh_Var_Name ("array_lit");
         Func_Params : constant Irep := New_Irep (I_Parameter_List);
         Func_Type : constant Irep :=
           Make_Code_Type (Parameters  => Func_Params,
                           Ellipsis    => False,
                           Return_Type => Do_Type_Reference (Etype (N)),
                           Inlined     => False,
                           Knr         => False);
      begin
         return New_Function_Symbol_Entry (Name        => Func_Name,
                                           Symbol_Type => Func_Type,
                                           Value       =>
                                             Build_Array_Lit_Func_Body (N),
                                           A_Symbol_Table =>
                                             Global_Symbol_Table);
      end Build_Array_Lit_Func;

      -----------------------------------
      -- Make_Array_Lit_Func_Call_Expr --
      -----------------------------------

      function Make_No_Args_Func_Call_Expr (Fun_Symbol : Irep;
                                            Return_Type : Irep;
                                            Source_Loc : Source_Ptr)
                                            return Irep is
         Call_Args  : constant Irep := New_Irep (I_Argument_List);
         Fun_Call : constant Irep :=
           Make_Side_Effect_Expr_Function_Call (
                                              Arguments       => Call_Args,
                                              I_Function      => Fun_Symbol,
                                              Source_Location => Source_Loc,
                                              I_Type          => Return_Type);
      begin
         return Fun_Call;
      end Make_No_Args_Func_Call_Expr;

      Func_Symbol : constant Symbol := Build_Array_Lit_Func (N);
   begin
      return Make_No_Args_Func_Call_Expr (Fun_Symbol  =>
                                            Symbol_Expr (Func_Symbol),
                                          Return_Type => Result_Type,
                                          Source_Loc  => Sloc (N));
   end Do_Aggregate_Literal_Array;

   ------------------------------------
   -- Make_Array_Default_Initialiser --
   ------------------------------------

   --  temp_comment: this was a nested function of 'do_object_declaration) but
   --  is pure

   function Make_Array_Default_Initialiser (E : Entity_Id) return Irep is
      Idx : constant Node_Id := First_Index (E);
      Lbound : constant Irep := Do_Expression (Low_Bound (Idx));
      Hbound : constant Irep := Do_Expression (High_Bound (Idx));
      Idx_Type : constant Entity_Id := Get_Array_Index_Type (E);
      Source_Loc : constant Source_Ptr := Sloc (E);
      Len : constant Irep :=
        Build_Array_Size (First      => Lbound,
                          Last       => Hbound,
                          Idx_Type => Do_Type_Reference (Idx_Type));
      Component_Type : constant Irep :=
        Do_Type_Reference (Get_Array_Component_Type (E));
      Alloc : constant Irep :=
        Make_Malloc_Function_Call_Expr (Num_Elem          => Len,
                                        Element_Type_Size =>
                                          Esize (Get_Array_Component_Type (E)),
                                        Source_Loc        => Source_Loc);
      Ret : constant Irep :=
        Make_Struct_Expr (Source_Location => Source_Loc,
                          I_Type          => Do_Type_Reference (E));
      Comp_P_Type : constant Irep :=
        Make_Pointer_Type (I_Subtype => Component_Type,
                           Width     => Pointer_Type_Width);
   begin
      Append_Struct_Member (Ret, Lbound);
      Append_Struct_Member (Ret, Hbound);
      Append_Struct_Member (Ret,
                            Typecast_If_Necessary (Expr     => Alloc,
                                                   New_Type => Comp_P_Type));
      return Ret;
   end Make_Array_Default_Initialiser;

   -------------------------------------
   -- Do_Constrained_Array_Definition --
   -------------------------------------

   --  No difference between representations at the moment:
   function Do_Constrained_Array_Definition (N : Node_Id) return Irep
   is (Do_Unconstrained_Array_Definition (N));

   ---------------------------------------
   -- Do_Unconstrained_Array_Definition --
   ---------------------------------------

   function Do_Unconstrained_Array_Definition (N : Node_Id) return Irep is
      Ret_Components : constant Irep := New_Irep (I_Struct_Union_Components);
      Ret : constant Irep :=
        Make_Struct_Type (Tag        => "unconstr_array",
                          Components => Ret_Components);
      Sub_Identifier : constant Node_Id :=
        Subtype_Indication (Component_Definition (N));
      Sub : constant Irep :=
        Do_Type_Reference (Etype (Sub_Identifier));
      Data_Type : constant Irep :=
        Make_Pointer_Type (I_Subtype => Sub,
                           Width     => Pointer_Type_Width);
      Data_Member : constant Irep :=
        Make_Struct_Component ("data", Data_Type);

      Dimension_Iter : Node_Id :=
        First ((if Nkind (N) = N_Unconstrained_Array_Definition then
                   Subtype_Marks (N) else
                   Discrete_Subtype_Definitions (N)));
      Dimension_Number : Positive := 1;
   begin

      --  Define a structure with explicit first, last and data-pointer members

      while Present (Dimension_Iter) loop
         declare
            Number_Str_Raw : constant String :=
              Integer'Image (Dimension_Number);
            Number_Str : constant String :=
              Number_Str_Raw (2 .. Number_Str_Raw'Last);
            First_Name : constant String := "first" & Number_Str;
            Last_Name : constant String := "last" & Number_Str;
            Dimension_Type : constant Irep :=
              Do_Type_Reference (Etype (Dimension_Iter));
            First_Comp : constant Irep :=
              Make_Struct_Component (First_Name, Dimension_Type);
            Last_Comp : constant Irep :=
              Make_Struct_Component (Last_Name, Dimension_Type);
         begin

            --  Declare the dimension index type if required:
            case Nkind (Dimension_Iter) is
               when N_Subtype_Indication =>
                  Do_Type_Declaration (Do_Subtype_Indication (Dimension_Iter),
                                       Etype (Dimension_Iter));
               when N_Range =>
                  Do_Type_Declaration (Do_Array_Range (Dimension_Iter),
                                       Etype (Dimension_Iter));
               when others =>
                  null;
            end case;

            Append_Component (Ret_Components, First_Comp);
            Append_Component (Ret_Components, Last_Comp);

         end;
         Dimension_Number := Dimension_Number + 1;
         Next (Dimension_Iter);
      end loop;

      Append_Component (Ret_Components, Data_Member);
      return Ret;
   end Do_Unconstrained_Array_Definition;

   -------------------------
   -- Do_Array_Assignment --
   -------------------------

   --  The following function builds a generalised array assignment
   --  dest := src_1 & src_2 & .. & src_n         for $n$ greater or equal to 1
   --  where each src_i may overlap with dest
   --  and sum_size is the sum of the slice sizes
   --  (which is why we copy each src_i to a temporary before copying to dest)
   --  Let ArrT := struct { int first; int last; T* data; }
   ----------------------------------------------------------------------------
   --  void concat_assign(ArrT dest, ArrT src_1, ArrT src_2, .., ArrT src_n) {
   --    dest_temp = (T*)malloc(sum_size * sizeof(T));
   --    offset_step = 0;
   --    slice_size = src_1.last - src_1.first + 1;
   --    memcpy(dest_temp + offset_step, src_1.data, slice_size * sizeof(T));
   --    offset_step += slice_size;
   --
   --    slice_size = src_2.last - src_2.first + 1;
   --    memcpy(dest_temp + offset_step, src_2.data, slice_size * sizeof(T));
   --    offset_step += slice_size;
   --    ...
   --    slice_size = src_n.last - src_n.first + 1;
   --    memcpy(dest_temp + offset_step, src_n.data, slice_size * sizeof(T));
   --    offset_step += slice_size;
   --
   --    memcpy(dest.data, dest_temp, sum_size * sizeof(T));
   --  }
   ----------------------------------------------------------------------------
   --  Once the function is constructed it returns a function call (expression)
   --  concat_assign(dest, src_1, src_2, .., src_n);
   function Do_Array_Assignment (N : Node_Id) return Irep
   is
      --  We assume the lhs is allocated
      LHS_Node : constant Node_Id := Name (N);
      RHS_Node : constant Node_Id := Expression (N);

      Source_Loc : constant Source_Ptr := Sloc (N);
      Ret_Type : constant Irep := Make_Void_Type;
      RHS_Arrays : constant Irep_Array := Do_RHS_Array_Assign (RHS_Node);
      Result_Type : constant Irep := Do_Type_Reference (Etype (LHS_Node));
      Concat_Params : constant Irep := New_Irep (I_Parameter_List);
      Concat_Arguments : constant Irep := New_Irep (I_Argument_List);
      Elem_Type_Ent : constant Entity_Id :=
        Get_Array_Component_Type (LHS_Node);
      Element_Type : constant Irep := Do_Type_Reference (Elem_Type_Ent);
      Index_Type : constant Irep :=
        Do_Type_Reference (Get_Array_Index_Type (LHS_Node));
      Function_Name : constant String := "concat_assign";

      Destination : constant Irep :=
        Create_Fun_Parameter (Fun_Name        => Function_Name,
                              Param_Name      => "dest_array",
                              Param_Type      => Result_Type,
                              Param_List      => Concat_Params,
                              A_Symbol_Table  => Global_Symbol_Table,
                              Source_Location => Source_Loc);

      function Build_Array_Params return Irep_Array;
      function Build_Concat_Assign_Body return Irep;

      function Build_Array_Params return Irep_Array
      is
         Result_Array : Irep_Array (RHS_Arrays'Range);
      begin
         for I in RHS_Arrays'Range loop
            Result_Array (I) :=
              Create_Fun_Parameter (Fun_Name        => Function_Name,
                                    Param_Name      => "array_rhs",
                                    Param_Type      => Result_Type,
                                    Param_List      => Concat_Params,
                                    A_Symbol_Table  => Global_Symbol_Table,
                                    Source_Location => Source_Loc);
         end loop;
         return Result_Array;
      end Build_Array_Params;

      function Build_Concat_Assign_Body return Irep
      is
         Slices : constant Irep_Array := Build_Array_Params;
         Result_Block : constant Irep := New_Irep (I_Code_Block);
         Dest_Symbol : constant Irep := Param_Symbol (Destination);
         PElement_Type : constant Irep :=
           Make_Pointer_Type (Element_Type, Pointer_Type_Width);

         Dest_Data : constant Irep :=
           Make_Member_Expr (Compound         => Dest_Symbol,
                             Source_Location  => Source_Loc,
                             Component_Number => 2,
                             I_Type           => PElement_Type,
                             Component_Name   => "data");
         Current_Offset : constant Irep :=
           Fresh_Var_Symbol_Expr (Index_Type, "offset_step");

         Void_Ptr_Type : constant Irep :=
           Make_Pointer_Type (I_Subtype => Make_Void_Type,
                              Width     => Pointer_Type_Width);
         Memcpy_Lhs : constant Irep :=
           Fresh_Var_Symbol_Expr (Void_Ptr_Type, "memcpy_lhs");
         Zero : constant Irep :=
           Build_Index_Constant (Value      => 0,
                                 Index_Type => Index_Type,
                                 Source_Loc => Source_Loc);
         EType_Size : constant Uint := Esize (Elem_Type_Ent);

         Sum_Size_Var : constant Irep :=
           Fresh_Var_Symbol_Expr (CProver_Size_T, "sum_size");
         Dest_Temp_Pre_Alloc : constant Irep :=
           Make_Malloc_Function_Call_Expr (
                                           Num_Elem          => Sum_Size_Var,
                                           Element_Type_Size => EType_Size,
                                           Source_Loc        => Source_Loc);
         Dest_Temp_Alloc : constant Irep :=
           Typecast_If_Necessary (Expr     => Dest_Temp_Pre_Alloc,
                                  New_Type => PElement_Type);
         Dest_Temp : constant Irep :=
           Fresh_Var_Symbol_Expr (PElement_Type, "dest_temp");

         procedure Build_Sum_Size (Ith_Slice : Irep);

         procedure Build_Sum_Size (Ith_Slice : Irep) is
            Source_I_Symbol : constant Irep := Param_Symbol (Ith_Slice);
            Slice_Size : constant Irep :=
              Build_Array_Size (Source_I_Symbol, Index_Type);
            Size_Increment : constant Irep :=
              Make_Op_Add (Rhs             =>
                           Typecast_If_Necessary (Slice_Size, CProver_Size_T),
                           Lhs             => Sum_Size_Var,
                           Source_Location => Source_Loc,
                           Overflow_Check  => False,
                           I_Type          => CProver_Size_T);
         begin
            Append_Op (Result_Block,
                       Make_Code_Assign (Rhs             => Size_Increment,
                                         Lhs             => Sum_Size_Var,
                                         Source_Location => Source_Loc));
         end Build_Sum_Size;

         procedure Process_Slice (Ith_Slice : Irep);

         --  Allocate a temporary, memcpy into the temporary, compute offset
         --  for destination, memcpy into the destination
         procedure Process_Slice (Ith_Slice : Irep)
         is
            Source_I_Symbol : constant Irep := Param_Symbol (Ith_Slice);
            Slice_Size : constant Irep :=
              Build_Array_Size (Source_I_Symbol, Index_Type);
            Slice_Size_Var : constant Irep :=
              Fresh_Var_Symbol_Expr (Index_Type, "slice_size");
            Offset_Dest : constant Irep :=
              Make_Op_Add (Rhs             => Current_Offset,
                           Lhs             => Dest_Temp,
                           Source_Location => Source_Loc,
                           Overflow_Check  => False,
                           I_Type          => PElement_Type);
            Left_Data : constant Irep :=
              Make_Member_Expr (Compound         => Source_I_Symbol,
                                Source_Location  => Source_Loc,
                                Component_Number => 2,
                                I_Type           => PElement_Type,
                                Component_Name   => "data");

            Memcpy_Fin : constant Irep :=
              Make_Memcpy_Function_Call_Expr (
                                          Destination       => Offset_Dest,
                                          Source            => Left_Data,
                                          Num_Elem          => Slice_Size_Var,
                                          Element_Type_Size => EType_Size,
                                          Source_Loc        => Source_Loc);
            Size_Increment : constant Irep :=
              Make_Op_Add (Rhs             => Slice_Size_Var,
                           Lhs             => Current_Offset,
                           Source_Location => Source_Loc,
                           I_Type          => Index_Type);
         begin
            Append_Op (Result_Block,
                       Make_Code_Assign (Rhs             => Slice_Size,
                                         Lhs             => Slice_Size_Var,
                                         Source_Location => Source_Loc));
            Append_Op (Result_Block,
                       Make_Code_Assign (Rhs             => Memcpy_Fin,
                                         Lhs             => Memcpy_Lhs,
                                         Source_Location => Source_Loc));
            Append_Op (Result_Block,
                       Make_Code_Assign (Rhs             => Size_Increment,
                                         Lhs             => Current_Offset,
                                         Source_Location => Source_Loc));
         end Process_Slice;

         Memcpy_Dest : constant Irep :=
           Make_Memcpy_Function_Call_Expr (
                                           Destination       => Dest_Data,
                                           Source            => Dest_Temp,
                                           Num_Elem          => Sum_Size_Var,
                                           Element_Type_Size => EType_Size,
                                           Source_Loc        => Source_Loc);
      begin
         Append_Op (Result_Block,
                    Make_Code_Assign (Rhs             => Zero,
                                      Lhs             => Current_Offset,
                                      Source_Location => Source_Loc));
         Append_Op (Result_Block,
                    Make_Code_Assign (Rhs             =>
                                  Typecast_If_Necessary (Zero, CProver_Size_T),
                                  Lhs             => Sum_Size_Var,
                                  Source_Location => Source_Loc));
         for I in Slices'Range loop
            Build_Sum_Size (Slices (I));
         end loop;
         Append_Op (Result_Block,
                    Make_Code_Assign (Rhs             => Dest_Temp_Alloc,
                                      Lhs             => Dest_Temp,
                                      Source_Location => Source_Loc));
         for I in Slices'Range loop
            Process_Slice (Slices (I));
         end loop;
         Append_Op (Result_Block,
                    Make_Code_Assign (Rhs             => Memcpy_Dest,
                                      Lhs             => Memcpy_Lhs,
                                      Source_Location => Source_Loc));
         return Result_Block;
      end Build_Concat_Assign_Body;

      Func_Symbol : constant Symbol :=
        Build_Function (Name           => Function_Name,
                        RType          => Ret_Type,
                        Func_Params    => Concat_Params,
                        FBody          => Build_Concat_Assign_Body,
                        A_Symbol_Table => Global_Symbol_Table);

      Func_Call : constant Irep :=
        Make_Side_Effect_Expr_Function_Call (
                                  Arguments       => Concat_Arguments,
                                  I_Function      => Symbol_Expr (Func_Symbol),
                                  Source_Location => Source_Loc,
                                  I_Type          => Ret_Type);
      Concat_Lhs : constant Irep :=
        Fresh_Var_Symbol_Expr (Ret_Type, "concat_Lhs");
   begin
      Append_Argument (Concat_Arguments,
                       Do_Expression (LHS_Node));
      for I in RHS_Arrays'Range loop
         Append_Argument (Concat_Arguments,
                          RHS_Arrays (I));
      end loop;

      return Make_Code_Assign (Rhs             => Func_Call,
                               Lhs             => Concat_Lhs,
                               Source_Location => Source_Loc);
   end Do_Array_Assignment;

   function Do_RHS_Array_Assign (N : Node_Id) return Irep_Array
   is
   begin
      if not (Nkind (N) = N_Op_Concat) then
         return (1 => Do_Expression (N));
      end if;
      if Nkind (Right_Opnd (N)) = N_Op_Concat then
         if Nkind (Left_Opnd (N)) = N_Op_Concat then
            return Do_RHS_Array_Assign (Left_Opnd (N))
              & Do_RHS_Array_Assign (Right_Opnd (N));
         else
            return (1 => Do_Expression (Left_Opnd (N)))
              & Do_RHS_Array_Assign (Right_Opnd (N));
         end if;
      else
         if Nkind (Left_Opnd (N)) = N_Op_Concat then
            return Do_RHS_Array_Assign (Left_Opnd (N))
              & (1 => Do_Expression (Right_Opnd (N)));
         else
            return (Do_Expression (Left_Opnd (N)),
                    Do_Expression (Right_Opnd (N)));
         end if;
      end if;
   end Do_RHS_Array_Assign;

   function Do_Array_Length (N : Node_Id) return Irep
   is
      Array_Struct : constant Irep := Do_Expression (Prefix (N));
      Index_Type : constant Entity_Id := Get_Array_Index_Type (Prefix (N));
   begin
      return Build_Array_Size (Array_Comp => Array_Struct,
                               Idx_Type => Do_Type_Reference (Index_Type));
   end Do_Array_Length;

   function Do_Array_First (N : Node_Id) return Irep
   is
   begin
      return Get_First_Index (Array_Struct   => Do_Expression (Prefix (N)),
                              Source_Loc     => Sloc (N),
                              A_Symbol_Table => Global_Symbol_Table);
   end Do_Array_First;

   function Do_Array_Last (N : Node_Id) return Irep
   is
   begin
      return Get_Last_Index (Array_Struct   => Do_Expression (Prefix (N)),
                              Source_Loc     => Sloc (N),
                              A_Symbol_Table => Global_Symbol_Table);
   end Do_Array_Last;

   --  This handled the oddball anonymous range nodes that can occur
   --  in array type declarations; they're effectively subtype indication
   --  nodes with an implied base type and a range constraint.
   function Do_Array_Range (N : Node_Id) return Irep
   is
      Underlying : constant Irep :=
        Do_Type_Reference (Etype (Etype (N)));
   begin
      return Do_Bare_Range_Constraint (N, Underlying);
   end Do_Array_Range;

   ------------------------------
   -- Get_Array_Component_Type --
   ------------------------------

   function Get_Array_Component_Type (N : Node_Id) return Entity_Id is
      Ty : Entity_Id := Etype (N);
   begin
      while Ekind (Ty) = E_Array_Subtype loop
         Ty := Etype (Ty);
      end loop;
      return Component_Type (Ty);
   end Get_Array_Component_Type;

   --------------------------
   -- Get_Array_Index_Type --
   --------------------------

   function Get_Array_Index_Type (N : Node_Id) return Entity_Id is
      Ret : Entity_Id := Etype (First_Index (Etype (N)));
   begin
      --  Many array index types are itypes with ranges private to
      --  this particular context. Use the underlying, unconstrained
      --  numeric type instead.
      while Ekind (Ret) = E_Signed_Integer_Subtype loop
         Ret := Etype (Ret);
      end loop;
      return Ret;
   end Get_Array_Index_Type;

   ---------------------------
   -- Make_Array_First_Expr --
   ---------------------------

   function Make_Array_First_Expr
     (Base_Type : Node_Id; Base_Irep : Irep) return Irep
   is
      Idx_Type : Node_Id;
      First : constant Irep := New_Irep (I_Member_Expr);
   begin
      -- Dummy initialisation --
      Set_Component_Name (First, "first1");

      if not Is_Array_Type (Base_Type) then
         Report_Unhandled_Node_Empty (Base_Type, "Make_Array_First_Expr",
                                      "Base type not array type");
         return First;
      end if;
      Idx_Type := Etype (First_Index (Base_Type));
      Set_Component_Name (First, "first1");
      Set_Compound (First, Base_Irep);
      Set_Type (First, Do_Type_Reference (Idx_Type));
      return First;
   end Make_Array_First_Expr;

   -------------------------
   -- Make_Array_Index_Op --
   -------------------------

   function Make_Array_Index_Op
     (Base_Irep : Irep; Base_Type : Node_Id; Idx_Irep : Irep) return Irep
   is
      Source_Loc : constant Source_Ptr := Sloc (Base_Type);
      First_Irep : constant Irep :=
        Make_Array_First_Expr (Base_Type, Base_Irep);
      Zero_Based_Index : constant Irep :=
        Make_Op_Sub (Rhs             => First_Irep,
                     Lhs             => Idx_Irep,
                     Source_Location => Source_Loc,
                     Overflow_Check  => False,
                     I_Type          => Get_Type (Idx_Irep),
                     Range_Check     => False);
      Result_Type : Irep;
      Pointer_Type : constant Irep := New_Irep (I_Pointer_Type);
      Indexed_Data : constant Irep :=
        Offset_Array_Data (Base         => Base_Irep,
                           Offset       => Zero_Based_Index,
                           Pointer_Type => Pointer_Type,
                           Source_Loc   => Source_Loc);
      Deref : Irep := New_Irep (I_Dereference_Expr);
   begin
      if not Is_Array_Type (Base_Type) then
         Report_Unhandled_Node_Empty (Base_Type, "Make_Array_Index_Op",
                                      "Base type not array type");
         return Deref;
      end if;
      Result_Type := Do_Type_Reference (Component_Type (Base_Type));
      if not (Kind (Zero_Based_Index) in Class_Expr) or else
        not (Kind (Get_Type (Idx_Irep)) in Class_Type)
      then
         Report_Unhandled_Node_Empty (Base_Type, "Make_Array_Index_Op",
                                      "Kinds not in classes");
         return Deref;
      end if;
      Set_Subtype (I     => Pointer_Type,
                   Value => Result_Type);
      Set_Width (I     => Pointer_Type,
                 Value => Pointer_Type_Width);
      Deref := Make_Dereference_Expr (Object          => Indexed_Data,
                                      Source_Location => Source_Loc,
                                      I_Type          => Result_Type);
      return Deref;
   end Make_Array_Index_Op;

   --------------
   -- Do_Slice --
   --------------

   --  The following build an expression representing slice
   --  orig(start .. end)
   --  Let ArrT := struct { int first; int last; T* data; }
   ----------------------------------------------------------------------------
   --  ArrT slice_expr(ArrT orig) {
   --    T* new_data = data + (start - orig.first);
   --    ArrT temp_array = {.first=start, .last=end, .data=new_data};
   --    return temp_array;
   --  }
   ----------------------------------------------------------------------------
   function Do_Slice (N : Node_Id) return Irep is
      Source_Loc : constant Source_Ptr := Sloc (N);
      Result_Type : constant Irep := Do_Type_Reference (Etype (N));
      Slice_Params : constant Irep := New_Irep (I_Parameter_List);
      Slice_Args : constant Irep := New_Irep (I_Argument_List);
      Function_Name : constant String := "slice_expr";
      Array_Param : constant Irep :=
        Create_Fun_Parameter (Fun_Name        => Function_Name,
                              Param_Name      => "orig_array",
                              Param_Type      => Result_Type,
                              Param_List      => Slice_Params,
                              A_Symbol_Table  => Global_Symbol_Table,
                              Source_Location => Source_Loc);

      function Build_Slice_Func_Body return Irep;

      function Build_Slice_Func_Body return Irep is
         Base : constant Irep := Param_Symbol (Array_Param);
         Idx_Type : constant Entity_Id :=
           Etype (First_Index (Etype (N)));
         New_First_Expr : constant Irep :=
           Do_Expression (Low_Bound (Scalar_Range (Idx_Type)));
         First_Type : constant Irep := Get_Type (New_First_Expr);
         Old_First_Expr : constant Irep :=
           Make_Member_Expr (Compound         => Base,
                             Source_Location  => Source_Loc,
                             Component_Number => 0,
                             I_Type           => First_Type,
                             Component_Name   => "first1");

         New_Last_Expr : constant Irep :=
           Do_Expression (High_Bound (Scalar_Range (Idx_Type)));
         Element_Type : constant Entity_Id := Get_Array_Component_Type (N);

         Result_Block : constant Irep := New_Irep (I_Code_Block);
         Array_Temp : constant Irep :=
           Fresh_Var_Symbol_Expr (Result_Type, "temp_array");

         Offset : constant Irep :=
           Make_Op_Sub (Rhs             => Old_First_Expr,
                        Lhs             => New_First_Expr,
                        Source_Location => Source_Loc,
                        Overflow_Check  => False,
                        I_Type          => First_Type);
         Pointer_Type : constant Irep :=
           Make_Pointer_Type (I_Subtype => Do_Type_Reference (Element_Type),
                              Width     => Pointer_Type_Width);
         New_Data : constant Irep :=
           Offset_Array_Data (Base         => Base,
                              Offset       => Offset,
                              Pointer_Type => Pointer_Type,
                              Source_Loc   => Source_Loc);
         Result : constant Irep :=
           Make_Struct_Expr (Source_Location => Source_Loc,
                             I_Type          => Result_Type);

         Data_Temp : constant Irep :=
           Fresh_Var_Symbol_Expr (Get_Type (New_Data), "temp_array_data");
      begin
         Append_Struct_Member (Result, New_First_Expr);
         Append_Struct_Member (Result, New_Last_Expr);
         Append_Struct_Member (Result, Data_Temp);

         Append_Op (Result_Block,
                    Make_Code_Assign (Rhs             => New_Data,
                                      Lhs             => Data_Temp,
                                      Source_Location => Source_Loc));
         Append_Op (Result_Block,
                    Make_Code_Assign (Rhs             => Result,
                                      Lhs             => Array_Temp,
                                      Source_Location => Source_Loc));

         Append_Op (Result_Block,
                    Make_Code_Return (Return_Value    => Array_Temp,
                                      Source_Location => Source_Loc));
         return Result_Block;
      end Build_Slice_Func_Body;

      Func_Symbol : constant Symbol :=
        Build_Function (Name           => Function_Name,
                        RType          => Result_Type,
                        Func_Params    => Slice_Params,
                        FBody          => Build_Slice_Func_Body,
                        A_Symbol_Table => Global_Symbol_Table);
      Slice_Id : constant Irep := Do_Expression (Prefix (N));
   begin
      Append_Argument (Slice_Args,
                       Slice_Id);
      return Make_Side_Effect_Expr_Function_Call (
                                 Arguments       => Slice_Args,
                                 I_Function      => Symbol_Expr (Func_Symbol),
                                 Source_Location => Source_Loc,
                                 I_Type          => Result_Type);
   end Do_Slice;

   --------------------------
   -- Do_Indexed_Component --
   --------------------------

   --  TODO: multi-dimensional arrays.
   function Do_Indexed_Component (N : Node_Id) return Irep is
      (Make_Array_Index_Op
         (Do_Expression (Prefix (N)),
          Etype (Prefix (N)),
          Do_Expression (First (Expressions (N)))));

   function Get_First_Index_Component (Array_Struct : Irep;
                                       A_Symbol_Table : Symbol_Table)
                                       return Irep
   is
      Array_Struct_Type : constant Irep :=
        Follow_Symbol_Type (Get_Type (Array_Struct), A_Symbol_Table);
      Struct_Component : constant Irep_List :=
        Get_Component (Get_Components (Array_Struct_Type));
   begin
      return List_Element (Struct_Component, List_First (Struct_Component));
   end Get_First_Index_Component;

   function Get_Last_Index_Component (Array_Struct : Irep;
                                      A_Symbol_Table : Symbol_Table)
                                      return Irep
   is
      Array_Struct_Type : constant Irep :=
        Follow_Symbol_Type (Get_Type (Array_Struct), A_Symbol_Table);
      Struct_Component : constant Irep_List :=
        Get_Component (Get_Components (Array_Struct_Type));
      Last_Cursor :  constant List_Cursor :=
        List_Next (Struct_Component, List_First (Struct_Component));
   begin
      return List_Element (Struct_Component, Last_Cursor);
   end Get_Last_Index_Component;

   function Get_Data_Component (Array_Struct : Irep;
                                A_Symbol_Table : Symbol_Table)
                                return Irep
   is
      Array_Struct_Type : constant Irep :=
        Follow_Symbol_Type (Get_Type (Array_Struct), A_Symbol_Table);
      Struct_Component : constant Irep_List :=
        Get_Component (Get_Components (Array_Struct_Type));
      Last_Cursor :  constant List_Cursor :=
        List_Next (Struct_Component,
                   List_Next (Struct_Component,
                     List_First (Struct_Component)));
   begin
      return List_Element (Struct_Component, Last_Cursor);
   end Get_Data_Component;

   function Get_First_Index (Array_Struct : Irep; Source_Loc : Source_Ptr;
                             A_Symbol_Table : Symbol_Table)
                             return Irep
   is
      First_Index_Component : constant Irep :=
        Get_First_Index_Component (Array_Struct   => Array_Struct,
                                   A_Symbol_Table => A_Symbol_Table);
   begin
      return Make_Member_Expr (Compound         => Array_Struct,
                               Source_Location  => Source_Loc,
                               Component_Number => 0,
                               I_Type           =>
                                 Get_Type (First_Index_Component),
                               Component_Name   =>
                                 Get_Name (First_Index_Component));
   end Get_First_Index;

   function Get_Last_Index (Array_Struct : Irep; Source_Loc : Source_Ptr;
                             A_Symbol_Table : Symbol_Table)
                             return Irep
   is
      Last_Index_Component : constant Irep :=
        Get_Last_Index_Component (Array_Struct   => Array_Struct,
                                   A_Symbol_Table => A_Symbol_Table);
   begin
      return Make_Member_Expr (Compound         => Array_Struct,
                               Source_Location  => Source_Loc,
                               Component_Number => 1,
                               I_Type           =>
                                 Get_Type (Last_Index_Component),
                               Component_Name   =>
                                 Get_Name (Last_Index_Component));
   end Get_Last_Index;

end Arrays;
