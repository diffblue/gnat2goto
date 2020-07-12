with Nlists;                use Nlists;
with Uintp;                 use Uintp;
with Namet;                 use Namet;
with Tree_Walk;             use Tree_Walk;
with Follow;                use Follow;
with Range_Check;           use Range_Check;
with Sem_Util;              use Sem_Util;
--  with ASVAT.Size_Model;
--  with Treepr;                use Treepr;
--  with Text_IO;               use Text_IO;
package body Arrays is

   --  A type for storing the bounds of an array dimension;
   type Dimension_Bounds is record
      Low, High, Length : Irep;
   end record;

   procedure Declare_First_Last_Length (Prefix : String;
                                        Dimension : Positive;
                                        Is_Constrained : Boolean;
                                        Index : Node_Id);
   --  Each dimension of an array has three read-only variable friends
   --  added to the symbol table and their values are the lower and upper
   --  bounds and the number of elements in the dimension.as determined from
   --  the constraint of the given index.
   --  If the index is not constrained the lower and upper bound
   --  values are nondet and the number of elements is zero.
   --  The variables have the same scope as the array.
   --  The variables representing the lower and upper bounds of the dimension
   --  are of the base type of the index type and the number of elements is
   --  an Int32_T which limits the size of a dimension to 2**31-1.
   --  Their names of the variables are <Prefix>___first_<Dimension>,
   --  <Prefix>___last_<imension> and <Prefix>___length_<Dimension>.

   function Get_Bounds (Index : Node_Id; Is_Constrained : Boolean)
                        return Dimension_Bounds;
   --  If the array is constrained, returns the lower and upper bounds of
   --  an index constraint and the length of the array dimension.
   --  If the array is unconstrained, returns nondet lower and upper bounds
   --  and a length of zero.
   --  The lower (first) and upper (last) bounds are the base type of
   --  index and the length is of type Int32_T;
   --  Note this places a maximum length of an array to be 2**31-1.

   ----------------------------
   -- Declare_First_And_Last --
   ----------------------------
   procedure Declare_First_Last_Length (Prefix : String;
                                        Dimension : Positive;
                                        Is_Constrained : Boolean;
                                        Index : Node_Id)
   is
      Number_Str_Raw : constant String :=
        Integer'Image (Dimension);
      Number_Str     : constant String :=
        Number_Str_Raw (2 .. Number_Str_Raw'Last);
      First_Name     : constant String :=
        Prefix & "___first_" & Number_Str;
      First_Name_Id  : constant Symbol_Id := Intern (First_Name);
      Last_Name      : constant String :=
        Prefix & "___last_" & Number_Str;
      Last_Name_Id   : constant Symbol_Id := Intern (Last_Name);
      Length_Name    : constant String :=
        Prefix & "___length_" & Number_Str;
      Length_Name_Id : constant Symbol_Id := Intern (Length_Name);

      Index_Type : constant Entity_Id :=
        Base_Type (Etype (Index));
      Index_Id : constant Symbol_Id :=
        Intern (Unique_Name (Index_Type));
      pragma Assert (Global_Symbol_Table.Contains (Index_Id));

      Type_Irep : constant Irep :=
        Do_Type_Reference (Index_Type);

      Bounds : constant Dimension_Bounds :=
        Get_Bounds (Index, Is_Constrained);

   begin
      New_Object_Symbol_Entry
        (Object_Name       => First_Name_Id,
         Object_Type       => Type_Irep,
         Object_Init_Value => Bounds.Low,
         A_Symbol_Table    => Global_Symbol_Table);

      New_Object_Symbol_Entry
        (Object_Name       => Last_Name_Id,
         Object_Type       => Type_Irep,
         Object_Init_Value => Bounds.High,
         A_Symbol_Table    => Global_Symbol_Table);

      New_Object_Symbol_Entry
        (Object_Name       => Length_Name_Id,
         Object_Type       => Int32_T,
         Object_Init_Value => Bounds.Length,
         A_Symbol_Table    => Global_Symbol_Table);

   end Declare_First_Last_Length;

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
                                            Source_Loc : Irep)
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
         Source_Loc : constant Irep := Get_Source_Location (N);
         Bounds : constant Node_Id := Aggregate_Bounds (N);
         Low_Expr : constant Irep :=
           Typecast_If_Necessary (Do_Expression (Low_Bound (Bounds)),
                                  CProver_Size_T, Global_Symbol_Table);
         High_Expr : constant Irep :=
           Typecast_If_Necessary (Do_Expression (High_Bound (Bounds)),
                                  CProver_Size_T, Global_Symbol_Table);
         Len_Expr : constant Irep :=
           Build_Array_Size (First      => Low_Expr,
                             Last       => High_Expr);
         Element_Type_Ent : constant Entity_Id := Get_Array_Component_Type (N);
         Element_Type_Pre : constant Irep :=
           Do_Type_Reference (Element_Type_Ent);

         Element_Type : constant Irep :=
           (if Kind (Follow_Symbol_Type (Element_Type_Pre,
            Global_Symbol_Table)) = I_C_Enum_Type
            then
               Make_Signedbv_Type (32)
            else
               Element_Type_Pre);
         Element_Size : constant Uint :=
           (if Kind (Element_Type) in Class_Bitvector_Type
            then
               UI_From_Int (Int (Get_Width (Element_Type)))
            else
               Esize (Element_Type_Ent));
         Literal_Temp : constant Irep :=
           Fresh_Var_Symbol_Expr (Make_Pointer_Type (Element_Type),
                                  "array_literal");
         Array_Temp : constant Irep :=
           Fresh_Var_Symbol_Expr (Result_Type, "temp_array");
         Lhs_Temp : constant Irep :=
           Fresh_Var_Symbol_Expr (Make_Pointer_Type (Element_Type),
                                  "temp_lhs");
         Result_Block : constant Irep :=
           Make_Code_Block (Source_Loc, CProver_Nil_T);
         Pos_Number : Natural := 0;

         --  NB: Component number seem to be ignored by CBMC
         --  We represent arrays as a structure of 3 members:
         --  0: first index
         --  1: last index
         --  2: data pointer
         Data_Mem_Expr : constant Irep := Get_Data_Member (Array_Temp,
                                                          Global_Symbol_Table);
         Array_Temp_Struct : constant Irep :=
           Make_Struct_Expr (Source_Location => Source_Loc,
                             I_Type          => Result_Type);
         Raw_Malloc_Call : constant Irep :=
           Make_Malloc_Function_Call_Expr (Num_Elem          => Len_Expr,
                                           Element_Type_Size => Element_Size,
                                           Source_Loc        => Source_Loc);
         Malloc_Call_Expr : constant Irep :=
           Typecast_If_Necessary (Raw_Malloc_Call,
                                  Make_Pointer_Type (Element_Type),
                                  Global_Symbol_Table);
         Literal_Address : constant Irep :=
           Typecast_If_Necessary (Literal_Temp,
                                  Make_Pointer_Type (Element_Type),
                                  Global_Symbol_Table);
         Memcpy_Call_Expr : constant Irep :=
           Make_Memcpy_Function_Call_Expr (Destination       => Data_Mem_Expr,
                                         Source            => Literal_Address,
                                         Num_Elem          => Len_Expr,
                                         Element_Type_Size => Element_Size,
                                           Source_Loc        => Source_Loc);

         PElement_Type : constant Irep := Make_Pointer_Type (Element_Type);

         procedure Initialize_Array;
         procedure Initialize_Array is
            Raw_Malloc_Call : constant Irep :=
              Make_Malloc_Function_Call_Expr (Num_Elem          => Len_Expr,
                                              Element_Type_Size =>
                                                Element_Size,
                                              Source_Loc        => Source_Loc);
            Malloc_Call_Expr : constant Irep :=
              Typecast_If_Necessary (Raw_Malloc_Call,
                                     Make_Pointer_Type (Element_Type),
                                     Global_Symbol_Table);
            Others_Expression : Irep;

            Loop_Iter_Var : constant Irep :=
              Fresh_Var_Symbol_Expr (CProver_Size_T, "i");
            Loop_Cond : constant Irep :=
              Make_Op_Gt (Rhs             => Loop_Iter_Var,
                          Lhs             => Len_Expr,
                          Source_Location => Source_Loc,
                          Overflow_Check  => False,
                          I_Type          => Make_Bool_Type);
            Size_T_Zero : constant Irep :=
              Build_Index_Constant (Value      => 0,
                                    Source_Loc => Source_Loc);
            Size_T_One : constant Irep :=
              Build_Index_Constant (Value      => 1,
                                    Source_Loc => Source_Loc);
            Increment_I : constant Irep :=
              Make_Op_Add (Rhs             => Size_T_One,
                           Lhs             => Loop_Iter_Var,
                           Source_Location => Source_Loc,
                           Overflow_Check  => False,
                           I_Type          => CProver_Size_T);
            Loop_Iter : constant Irep :=
              Make_Code_Assign (Rhs             => Increment_I,
                                Lhs             => Loop_Iter_Var,
                                Source_Location => Source_Loc,
                                I_Type          => Make_Nil_Type);
            Loop_Body : constant Irep :=
              Make_Code_Block (Source_Location => Source_Loc,
                               I_Type          => Make_Nil_Type);

            Array_As_Pointer : constant Irep :=
              Typecast_If_Necessary (Literal_Temp, PElement_Type,
                                     Global_Symbol_Table);
            Lhs_Ptr : constant Irep :=
              Make_Op_Add (Rhs             => Loop_Iter_Var,
                           Lhs             => Array_As_Pointer,
                           Source_Location => Source_Loc,
                           Overflow_Check  => False,
                           I_Type          => PElement_Type);
            Lhs_Irep : constant Irep :=
              Make_Dereference_Expr (Object          => Lhs_Ptr,
                                     Source_Location => Source_Loc,
                                     I_Type          => Element_Type);
         begin
            Append_Declare_And_Init (Symbol     => Literal_Temp,
                                     Value      => Malloc_Call_Expr,
                                     Block      => Result_Block,
                                     Source_Loc => Source_Loc);

            --  Handle an "others" splat expression if present:
            if Present (Component_Associations (N)) then
               declare
                  Maybe_Others_Node : constant Node_Id :=
                    Last (Component_Associations (N));
                  Maybe_Others_Choices : constant List_Id :=
                    Choices (Maybe_Others_Node);
               begin
                  pragma Assert (List_Length (Maybe_Others_Choices) = 1);

                  --  this association does not end with others -> bail
                  if Nkind (First (Maybe_Others_Choices)) /= N_Others_Choice
                  then
                     return;
                  end if;

                  Others_Expression :=
                    Do_Expression (Expression (Maybe_Others_Node));
               end;
            else
               return;
            end if;

            --  iterate over elements and assing others-value to them
            Append_Op (Loop_Body,
                       Make_Code_Assign (Rhs             => Others_Expression,
                                         Lhs             => Lhs_Irep,
                                         Source_Location => Source_Loc,
                                         I_Type          => Make_Nil_Type));
            Append_Op (Loop_Body, Loop_Iter);

            Append_Op (Result_Block,
                       Make_Code_Assign (Rhs             => Size_T_Zero,
                                         Lhs             => Loop_Iter_Var,
                                         Source_Location => Source_Loc,
                                         I_Type          => Make_Nil_Type));
            Append_Op (Result_Block,
                       Make_Code_While (Loop_Body       => Loop_Body,
                                        Cond            => Loop_Cond,
                                        Source_Location => Source_Loc,
                                        I_Type          => Make_Nil_Type));
         end Initialize_Array;

      begin
         Initialize_Array;

         while Present (Pos_Iter) loop
            declare
               Expr : constant Irep := Do_Expression (Pos_Iter);
               Pos_Constant : constant Irep :=
                 Build_Index_Constant (Value      => Int (Pos_Number),
                                       Source_Loc => Source_Loc);
               Array_As_Pointer : constant Irep :=
                 Typecast_If_Necessary (Literal_Temp, PElement_Type,
                                        Global_Symbol_Table);
               Lhs_Ptr : constant Irep :=
                 Make_Op_Add (Rhs             => Pos_Constant,
                              Lhs             => Array_As_Pointer,
                              Source_Location => Source_Loc,
                              Overflow_Check  => False,
                              I_Type          => PElement_Type);
               Lhs_Irep : constant Irep :=
                 Make_Dereference_Expr (Object          => Lhs_Ptr,
                                        Source_Location => Source_Loc,
                                        I_Type          => Element_Type);
            begin
               Append_Op (Result_Block,
                          Make_Code_Assign (Rhs             =>
               Typecast_If_Necessary (Expr, Element_Type, Global_Symbol_Table),
                                            Lhs             => Lhs_Irep,
                                            Source_Location => Source_Loc,
                                            I_Type          => Element_Type));
            end;
            Next (Pos_Iter);
            Pos_Number := Pos_Number + 1;
         end loop;

         Append_Struct_Member (Array_Temp_Struct, Low_Expr);
         Append_Struct_Member (Array_Temp_Struct, High_Expr);
         Append_Struct_Member (Array_Temp_Struct, Malloc_Call_Expr);

         if Present (Component_Associations (N)) and then
           List_Length (Component_Associations (N)) /= 1
         then
            declare
               Components : constant List_Id := Component_Associations (N);
               Component_Node : Node_Id := First (Components);
            begin
               if List_Length (Choices (Component_Node)) /= 1 then
                  return Report_Unhandled_Node_Irep (N,
                                     "Do_Aggregate_Literal_Array",
                                     "More than one choice in component node");
               end if;
               while Present (Component_Node) loop
                  declare
                     Expr : constant Irep :=
                       Do_Expression (Expression (Component_Node));
                     Choice_Id : constant Irep :=
                       Do_Expression (First (Choices (Component_Node)));
                     Component_Index : constant Irep :=
                       Typecast_If_Necessary (Choice_Id, CProver_Size_T,
                                              Global_Symbol_Table);
                     Zero_Based_Index : constant Irep :=
                       Make_Op_Sub (Rhs             => Low_Expr,
                                    Lhs             => Component_Index,
                                    Source_Location => Source_Loc,
                                    Overflow_Check  => False,
                                    I_Type          => CProver_Size_T);
                     Array_As_Pointer : constant Irep :=
                       Typecast_If_Necessary (Literal_Temp, PElement_Type,
                                              Global_Symbol_Table);
                     Lhs_Ptr : constant Irep :=
                       Make_Op_Add (Rhs             => Zero_Based_Index,
                                    Lhs             => Array_As_Pointer,
                                    Source_Location => Source_Loc,
                                    Overflow_Check  => False,
                                    I_Type          => PElement_Type);
                     Lhs_Irep : constant Irep :=
                       Make_Dereference_Expr (Object          => Lhs_Ptr,
                                              Source_Location => Source_Loc,
                                              I_Type          => Element_Type);
                  begin
                     Append_Op (Result_Block,
                                Make_Code_Assign (Rhs             =>
               Typecast_If_Necessary (Expr, Element_Type, Global_Symbol_Table),
                                            Lhs             => Lhs_Irep,
                                            Source_Location => Source_Loc,
                                            I_Type          => Element_Type));
                  end;
                  Component_Node := Next (Component_Node);
               end loop;
            end;
         end if;

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
         Func_Params : constant Irep := Make_Parameter_List;
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
                                            Source_Loc : Irep)
                                            return Irep is
         Call_Args  : constant Irep := Make_Argument_List;
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
      return Make_No_Args_Func_Call_Expr
        (Fun_Symbol  =>
           Symbol_Expr (Func_Symbol),
         Return_Type => Result_Type,
         Source_Loc  => Get_Source_Location (N));
   end Do_Aggregate_Literal_Array;

   ------------------------------------
   -- Make_Array_Default_Initialiser --
   ------------------------------------

   --  temp_comment: this was a nested function of 'do_object_declaration) but
   --  is pure

   function Make_Array_Default_Initialiser (E : Entity_Id) return Irep is
      --  Note this function only works for one dimensional arrays at present.
      Idx : constant Node_Id := First_Index (E);
      --  The Entity is an array object
      --  The first index is a discrete_subtype_definition which
      --  may be a subtype_indication or a range.
      --  For determining the upper bounds and lower bounds a range is required
      --  and if the first index is a subtype_indication, the constraints
      --  of the subtype have to be obtained - which should be a range.
      Bound_Range : constant Node_Id :=
        (if Nkind (Idx) = N_Range
         then
            --  It is a range
            Idx
         elsif Nkind (Idx) = N_Subtype_Indication then
            --  It is an anonymous subtype
            Scalar_Range (Etype (Idx))
         else
            --  It is an explicitly declared subtype
            Scalar_Range (Entity (Idx)));

      Lbound : constant Irep :=
        Typecast_If_Necessary (Do_Expression (Low_Bound (Bound_Range)),
                               CProver_Size_T, Global_Symbol_Table);
      Hbound : constant Irep :=
        Typecast_If_Necessary (Do_Expression (High_Bound (Bound_Range)),
                               CProver_Size_T, Global_Symbol_Table);
      Source_Loc : constant Irep := Get_Source_Location (E);
      Len : constant Irep :=
        Build_Array_Size (First      => Lbound,
                          Last       => Hbound);
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
                            Typecast_If_Necessary (Alloc, Comp_P_Type,
                              Global_Symbol_Table));
      return Ret;
   end Make_Array_Default_Initialiser;

   -------------------------------------
   -- Do_Constrained_Array_Definition --
   -------------------------------------

   --  No difference between representations at the moment:
   function Do_Constrained_Array_Definition (N : Node_Id) return Irep
   renames Do_Unconstrained_Array_Definition;

   ---------------------------------------
   -- Do_Unconstrained_Array_Definition --
   ---------------------------------------

   function Do_Unconstrained_Array_Definition (N : Node_Id) return Irep is
      Sub_Identifier : constant Node_Id :=
        Subtype_Indication (Component_Definition (N));
      Sub_Pre : constant Irep :=
        Do_Type_Reference (Etype (Sub_Identifier));
      Sub : constant Irep :=
        (if Kind (Follow_Symbol_Type (Sub_Pre, Global_Symbol_Table))
         = I_C_Enum_Type
         then
         --  TODO: use ASVAT.Size_Model.Size when Package standard
         --  is handled
            Make_Signedbv_Type (32)
         else
            Sub_Pre);

      Array_Type_Name : constant String :=
        Unique_Name (Defining_Identifier (Parent (N)));

      Dimension_Iter : Node_Id :=
        First ((if Nkind (N) = N_Unconstrained_Array_Definition then
                   Subtype_Marks (N) else
                   Discrete_Subtype_Definitions (N)));

      --  The front-end ensures that the array has at least one dimension.
      Array_Type_Irep  : Irep;
      Dimension_Number : Positive := 1;
   begin
      --  Do the first dimension.
      declare
         Raw_Str : constant String := Integer'Image (Dimension_Number);
         Dim_Str : constant String := Raw_Str (2 .. Raw_Str'Last);
         Length_Id : constant Symbol_Id :=
           Intern (Array_Type_Name & "___length_" & Dim_Str);
      begin
         Declare_First_Last_Length
           (Prefix         => Array_Type_Name,
            Dimension      => Dimension_Number,
            Is_Constrained => Nkind (N) = N_Constrained_Array_Definition,
            Index          => Dimension_Iter);
         --  Not the most efficient way of obtaining the length of the
         --  dimension, but it checks that it has been inserted correctly.
         pragma Assert (Global_Symbol_Table.Contains (Length_Id));
         Array_Type_Irep := Make_Array_Type
           (Sub, Global_Symbol_Table (Length_Id).Value);
      end;

      --  Now the remaining dimensions.
      Dimension_Iter := Next (Dimension_Iter);
      while Present (Dimension_Iter) loop
         declare
            Raw_Str : constant String := Integer'Image (Dimension_Number);
            Dim_Str : constant String := Raw_Str (2 .. Raw_Str'Last);
            Length_Id : constant Symbol_Id :=
              Intern (Array_Type_Name & "___length_" & Dim_Str);
         begin
            Dimension_Number := Dimension_Number + 1;
            Declare_First_Last_Length
              (Prefix         => Array_Type_Name,
               Dimension      => Dimension_Number,
               Is_Constrained => Nkind (N) = N_Constrained_Array_Definition,
               Index          => Dimension_Iter);

            pragma Assert (Global_Symbol_Table.Contains (Length_Id));
            Array_Type_Irep := Make_Array_Type
              (Array_Type_Irep, Global_Symbol_Table (Length_Id).Value);
            Dimension_Iter := Next (Dimension_Iter);
         end;
      end loop;
      return Array_Type_Irep;
   end Do_Unconstrained_Array_Definition;

--        Ret_Components : constant Irep := Make_Struct_Union_Components;
--        Ret : constant Irep :=
--          Make_Struct_Type (Tag        => "unconstr_array",
--                            Components => Ret_Components);
--        Sub_Identifier : constant Node_Id :=
--          Subtype_Indication (Component_Definition (N));
--        Sub_Pre : constant Irep :=
--          Do_Type_Reference (Etype (Sub_Identifier));
--        Sub : constant Irep :=
--          (if Kind (Follow_Symbol_Type (Sub_Pre, Global_Symbol_Table))
--           = I_C_Enum_Type
--           then
--              Make_Signedbv_Type (32)
--           else
--              Sub_Pre);
--        Data_Type : constant Irep :=
--          Make_Pointer_Type (I_Subtype => Sub,
--                             Width     => Pointer_Type_Width);
--        Data_Member : constant Irep :=
--          Make_Struct_Component ("data", Data_Type);
--
--        Dimension_Iter : Node_Id :=
--          First ((if Nkind (N) = N_Unconstrained_Array_Definition then
--                     Subtype_Marks (N) else
--                     Discrete_Subtype_Definitions (N)));
--        Dimension_Number : Positive := 1;
--     begin
--
--   --  Define a structure with explicit first, last and data-pointer members
--
--        while Present (Dimension_Iter) loop
--           declare
--              Number_Str_Raw : constant String :=
--                Integer'Image (Dimension_Number);
--              Number_Str : constant String :=
--                Number_Str_Raw (2 .. Number_Str_Raw'Last);
--              First_Name : constant String := "first" & Number_Str;
--              Last_Name : constant String := "last" & Number_Str;
--              First_Comp : constant Irep :=
--                Make_Struct_Component (First_Name, CProver_Size_T);
--              Last_Comp : constant Irep :=
--                Make_Struct_Component (Last_Name, CProver_Size_T);
--           begin
--
--              Append_Component (Ret_Components, First_Comp);
--              Append_Component (Ret_Components, Last_Comp);
--
--           end;
--           Dimension_Number := Dimension_Number + 1;
--           Next (Dimension_Iter);
--        end loop;
--
--        Append_Component (Ret_Components, Data_Member);
--        return Ret;
--     end Do_Unconstrained_Array_Definition;

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

      Source_Loc : constant Irep := Get_Source_Location (N);
      Ret_Type : constant Irep := Make_Void_Type;
      RHS_Arrays : constant Irep_Array := Do_RHS_Array_Assign (RHS_Node);
      Result_Type : constant Irep := Do_Type_Reference (Etype (LHS_Node));
      Concat_Params : constant Irep := Make_Parameter_List;
      Concat_Arguments : constant Irep := Make_Argument_List;
      Elem_Type_Ent : constant Entity_Id :=
        Get_Array_Component_Type (LHS_Node);
      Element_Type : constant Irep := Do_Type_Reference (Elem_Type_Ent);
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
         Result_Block : constant Irep :=
           Make_Code_Block (Source_Loc, CProver_Nil_T);
         Dest_Symbol : constant Irep := Param_Symbol (Destination);
         PElement_Type : constant Irep :=
           Make_Pointer_Type (Element_Type, Pointer_Type_Width);

         Dest_Data : constant Irep := Get_Data_Member (Dest_Symbol,
                                                       Global_Symbol_Table);
         Current_Offset : constant Irep :=
           Fresh_Var_Symbol_Expr (CProver_Size_T, "offset_step");

         Void_Ptr_Type : constant Irep :=
           Make_Pointer_Type (I_Subtype => Make_Void_Type,
                              Width     => Pointer_Type_Width);
         Memcpy_Lhs : constant Irep :=
           Fresh_Var_Symbol_Expr (Void_Ptr_Type, "memcpy_lhs");
         Zero : constant Irep :=
           Build_Index_Constant (Value      => 0,
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
           Typecast_If_Necessary (Dest_Temp_Pre_Alloc, PElement_Type,
                                  Global_Symbol_Table);
         Dest_Temp : constant Irep :=
           Fresh_Var_Symbol_Expr (PElement_Type, "dest_temp");

         procedure Build_Sum_Size (Ith_Slice : Irep);

         procedure Build_Sum_Size (Ith_Slice : Irep) is
            Source_I_Symbol : constant Irep := Param_Symbol (Ith_Slice);
            Slice_Size : constant Irep :=
              Build_Array_Size (Source_I_Symbol);
            Size_Increment : constant Irep :=
              Make_Op_Add (Rhs             =>
                             Typecast_If_Necessary (Slice_Size, CProver_Size_T,
                               Global_Symbol_Table),
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
              Build_Array_Size (Source_I_Symbol);
            Slice_Size_Var : constant Irep :=
              Fresh_Var_Symbol_Expr (CProver_Size_T, "slice_size");
            Offset_Dest : constant Irep :=
              Make_Op_Add (Rhs             => Current_Offset,
                           Lhs             => Dest_Temp,
                           Source_Location => Source_Loc,
                           Overflow_Check  => False,
                           I_Type          => PElement_Type);
            Left_Data : constant Irep := Get_Data_Member (Source_I_Symbol,
                                                          Global_Symbol_Table);

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
                           I_Type          => CProver_Size_T);
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
                                        Typecast_If_Necessary (Zero,
                                          CProver_Size_T, Global_Symbol_Table),
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
      --  It seems as though an N_Explicit_Drereference is placed in the tree
      --  even when the prefix of the Length attribute is an implicit
      --  dereference.
      --  Hence, implicit dereferences do not have to be seperately handled,
      --  they are handled as explicit dereferences.
      Array_Struct      : constant Irep := Do_Expression (Prefix (N));
   begin
      return Build_Array_Size (Array_Struct);
   end Do_Array_Length;

   function Do_Array_First (N : Node_Id) return Irep
   is
      --  It seems as though an N_Explicit_Drereference is placed in the tree
      --  even when the prefix of the Length attribute is an implicit
      --  dereference.
      --  Hence, implicit dereferences do not have to be seperately handled,
      --  they are handled as explicit dereferences.
   begin
      return Get_First_Index (Do_Expression (Prefix (N)));
   end Do_Array_First;

   function Do_Array_Last (N : Node_Id) return Irep
   is
      --  It seems as though an N_Explicit_Drereference is placed in the tree
      --  even when the prefix of the Length attribute is an implicit
      --  dereference.
      --  Hence, implicit dereferences do not have to be seperately handled,
      --  they are handled as explicit dereferences.
   begin
      return Get_Last_Index (Do_Expression (Prefix (N)));
   end Do_Array_Last;

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

   ---------------------------
   -- Make_Array_First_Expr --
   ---------------------------

   function Make_Array_First_Expr
     (Base_Type : Node_Id; Base_Irep : Irep) return Irep
   is
      First : constant Irep := Make_Member_Expr
         (Compound => Base_Irep,
          Source_Location => Get_Source_Location (Base_Type),
          Component_Number => 0,
          I_Type => CProver_Size_T,
          Component_Name => "first1");
   begin
      if not Is_Array_Type (Base_Type) then
         Report_Unhandled_Node_Empty (Base_Type, "Make_Array_First_Expr",
                                      "Base type not array type");
      end if;
      return First;
   end Make_Array_First_Expr;

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
      --  The prefix to the slice may be an access to an array object
      --  which must be implicitly dereferenced.
      The_Prefix        : constant Node_Id := Prefix (N);
      Prefix_Etype      : constant Node_Id := Etype (The_Prefix);
      Is_Implicit_Deref : constant Boolean := Is_Access_Type (Prefix_Etype);
      Prefix_Irep       : constant Irep := Do_Expression (The_Prefix);
      Result_Type        : constant Irep :=
        (if Is_Implicit_Deref then
            Do_Type_Reference (Designated_Type (Prefix_Etype))
         else
            Do_Type_Reference (Prefix_Etype));
      Base_Irep         : constant Irep :=
        (if Is_Implicit_Deref then
            Make_Dereference_Expr
           (I_Type => Result_Type,
            Object => Prefix_Irep,
            Source_Location => Get_Source_Location (N))
         else
            Prefix_Irep);

      --  Where required the prefix has been implicitly dereferenced.
      Source_Loc : constant Irep := Get_Source_Location (N);
      Slice_Params : constant Irep := Make_Parameter_List;
      Slice_Args : constant Irep := Make_Argument_List;
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
           Typecast_If_Necessary (Do_Expression (Low_Bound (Scalar_Range
                                  (Idx_Type))), CProver_Size_T,
                                  Global_Symbol_Table);
         Old_First_Expr : constant Irep :=
           Make_Member_Expr (Compound         => Base,
                             Source_Location  => Source_Loc,
                             Component_Number => 0,
                             I_Type           => CProver_Size_T,
                             Component_Name   => "first1");

         New_Last_Expr : constant Irep :=
           Typecast_If_Necessary (Do_Expression (High_Bound (Scalar_Range
                                  (Idx_Type))), CProver_Size_T,
                                  Global_Symbol_Table);
         Result_Block : constant Irep :=
           Make_Code_Block (Source_Loc, CProver_Nil_T);
         Array_Temp : constant Irep :=
           Fresh_Var_Symbol_Expr (Result_Type, "temp_array");

         Offset : constant Irep :=
           Make_Op_Sub (Rhs             => Old_First_Expr,
                        Lhs             => New_First_Expr,
                        Source_Location => Source_Loc,
                        Overflow_Check  => False,
                        I_Type          => CProver_Size_T);
         New_Data : constant Irep :=
           Offset_Array_Data (Base         => Base,
                              Offset       => Offset);
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
      Slice_Id : constant Irep := Base_Irep;
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
      --  The prefix to an indexed component may be an access to an
      --  array object which must be implicitly dereferenced.
      The_Prefix        : constant Node_Id := Prefix (N);
      Prefix_Etype      : constant Node_Id := Etype (The_Prefix);
      Is_Implicit_Deref : constant Boolean := Is_Access_Type (Prefix_Etype);
   begin
      if (if Nkind (Prefix_Etype) = N_Defining_Identifier then
             Get_Name_String (Chars (Etype (Etype (Prefix (N)))))
          elsif Is_Implicit_Deref then
             Get_Name_String (Chars (Designated_Type (Prefix_Etype)))
          else
             "")
        = "string"
      then
         return Report_Unhandled_Node_Irep (N, "Do_Expression",
                                            "Index of string unsupported");
      end if;

      --  Where required the prefix has been implicitly dereferenced.
      declare
         Prefix_Irep       : constant Irep := Do_Expression (The_Prefix);
         Resolved_Type     : constant Irep :=
           (if Is_Implicit_Deref then
               Do_Type_Reference (Designated_Type (Prefix_Etype))
            else
               Do_Type_Reference (Prefix_Etype));
         Base_Irep         : constant Irep :=
           (if Is_Implicit_Deref then
               Make_Dereference_Expr
              (I_Type => Resolved_Type,
               Object => Prefix_Irep,
               Source_Location => Get_Source_Location (N))
            else
               Prefix_Irep);

         Idx_Irep : constant Irep :=
           Typecast_If_Necessary (Do_Expression (First (Expressions (N))),
                                  CProver_Size_T, Global_Symbol_Table);

         Source_Loc : constant Irep := Get_Source_Location (Base_Irep);
         First_Irep : constant Irep := Get_First_Index (Base_Irep);
         Last_Irep : constant Irep := Get_Last_Index (Base_Irep);
         Checked_Index : constant Irep :=
           Make_Index_Assert_Expr (N           => N,
                                   Index       => Idx_Irep,
                                   First_Index => First_Irep,
                                   Last_Index  => Last_Irep);
         Zero_Based_Index : constant Irep :=
           Make_Op_Sub (Rhs             => First_Irep,
                        Lhs             => Checked_Index,
                        Source_Location => Source_Loc,
                        Overflow_Check  => False,
                        I_Type          => Get_Type (Idx_Irep),
                        Range_Check     => False);

         Data_Irep : constant Irep :=
           Get_Data_Member (Base_Irep, Global_Symbol_Table);
         Data_Type : constant Irep := Get_Type (Data_Irep);
         Indexed_Data : constant Irep :=
           Offset_Array_Data (Base         => Base_Irep,
                              Offset       => Zero_Based_Index);
         Element_Type : constant Irep := Get_Subtype (Data_Type);
      begin
         return
           Make_Dereference_Expr (Object          => Indexed_Data,
                                  Source_Location => Source_Loc,
                                  I_Type          => Element_Type);
      end;
   end Do_Indexed_Component;

   -----------------
   -- Get_Bounds --
   ---------------

   function Get_Bounds (Index : Node_Id; Is_Constrained : Boolean)
                        return Dimension_Bounds
   is
   begin
      if Is_Constrained then
         declare
            Bounds : constant Node_Id :=
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

            Low  : constant Irep := Do_Expression (Low_Bound (Bounds));
            High : constant Irep := Do_Expression (High_Bound (Bounds));

            First_Type : constant Irep := Get_Type (Low);
            Last_Type  : constant Irep := Get_Type (High);

            First_Val : constant Irep :=
              (if Kind (First_Type) /= I_Signedbv_Type or else
               Get_Width (First_Type) /= 32
               then
                  Make_Op_Typecast
                 (Op0             => Low,
                  Source_Location => Internal_Source_Location,
                  I_Type          => Int32_T,
                  Range_Check     => False)
               else
                  Low);
            Last_Val : constant Irep :=
              (if Kind (Last_Type) /= I_Signedbv_Type or else
               Get_Width (Last_Type) /= 32
               then
                  Make_Op_Typecast
                 (Op0             => High,
                  Source_Location => Internal_Source_Location,
                  I_Type          => Int32_T,
                  Range_Check     => False)
               else
                  High);

            One : constant Irep :=
              Make_Constant_Expr
                (Source_Location => Internal_Source_Location,
                 I_Type          => Int32_T,
                 Range_Check     => False,
                 Value           => "1");

            Diff : constant Irep :=
              Make_Op_Sub
                (Rhs             => First_Val,
                 Lhs             => Last_Val,
                 Source_Location => Internal_Source_Location,
                 Overflow_Check  => False,
                 I_Type          => Int32_T,
                 Range_Check     => False);

            Length_Val : constant Irep :=
              Make_Op_Add
                (Rhs             => One,
                 Lhs             => Diff,
                 Source_Location => Internal_Source_Location,
                 Overflow_Check  => False,
                 I_Type          => Int32_T,
                 Range_Check     => False);
         begin
            return (Low => Low, High => High, Length => Length_Val);
         end;
      else
         return (Low | High =>  Make_Side_Effect_Expr_Nondet
                 (I_Type => Do_Type_Reference (Etype (Index)),
                  Source_Location => Internal_Source_Location),
                Length => Ireps.Make_Constant_Expr
                   (Source_Location => Internal_Source_Location,
                    I_Type          => Int32_T,
                    Range_Check     => False,
                    Value           => "0"));
      end if;
   end Get_Bounds;

   function Get_First_Index_Component (Array_Struct : Irep)
                                       return Irep
   is
      Array_Struct_Type : constant Irep :=
        Follow_Symbol_Type (Get_Type (Array_Struct), Global_Symbol_Table);
      Struct_Component : constant Irep_List :=
        Get_Component (Get_Components (Array_Struct_Type));
   begin
      return List_Element (Struct_Component, List_First (Struct_Component));
   end Get_First_Index_Component;

   function Get_Last_Index_Component (Array_Struct : Irep) return Irep
   is
      Array_Struct_Type : constant Irep :=
        Follow_Symbol_Type (Get_Type (Array_Struct), Global_Symbol_Table);
      Struct_Component : constant Irep_List :=
        Get_Component (Get_Components (Array_Struct_Type));
      Last_Cursor :  constant List_Cursor :=
        List_Next (Struct_Component, List_First (Struct_Component));
   begin
      return List_Element (Struct_Component, Last_Cursor);
   end Get_Last_Index_Component;

   function Get_Data_Component_From_Type (Array_Struct_Type : Irep)
                                          return Irep
   is
      Struct_Component : constant Irep_List :=
        Get_Component (Get_Components (Array_Struct_Type));
      Last_Cursor :  constant List_Cursor :=
        List_Next (Struct_Component,
                   List_Next (Struct_Component,
                     List_First (Struct_Component)));
   begin
      return List_Element (Struct_Component, Last_Cursor);
   end Get_Data_Component_From_Type;

   function Get_Data_Component (Array_Struct : Irep;
                                A_Symbol_Table : Symbol_Table)
                                return Irep
   is
      Array_Struct_Type : constant Irep :=
        Follow_Symbol_Type (Get_Type (Array_Struct), A_Symbol_Table);
   begin
      return Get_Data_Component_From_Type (Array_Struct_Type);
   end Get_Data_Component;

   function Get_First_Index (Array_Struct : Irep) return Irep
   is
      First_Index_Component : constant Irep :=
        Get_First_Index_Component (Array_Struct);
   begin
      return Make_Member_Expr (Compound         => Array_Struct,
                               Source_Location  => Internal_Source_Location,
                               Component_Number => 0,
                               I_Type           => CProver_Size_T,
                               Component_Name   =>
                                 Get_Name (First_Index_Component));
   end Get_First_Index;

   function Get_Last_Index (Array_Struct : Irep) return Irep
   is
      Last_Index_Component : constant Irep :=
        Get_Last_Index_Component (Array_Struct);
   begin
      return Make_Member_Expr (Compound         => Array_Struct,
                               Source_Location  => Internal_Source_Location,
                               Component_Number => 1,
                               I_Type           => CProver_Size_T,
                               Component_Name   =>
                                 Get_Name (Last_Index_Component));
   end Get_Last_Index;

   function Get_Data_Member (Array_Struct : Irep;
                             A_Symbol_Table : Symbol_Table)
                             return Irep
   is
      Data_Member : constant Irep :=
        Get_Data_Component (Array_Struct, A_Symbol_Table);
   begin
      return Make_Member_Expr (Compound         => Array_Struct,
                               Source_Location  => Internal_Source_Location,
                               Component_Number => 2,
                               I_Type           =>
                                 Get_Type (Data_Member),
                               Component_Name   =>
                                 Get_Name (Data_Member));
   end Get_Data_Member;

   function Build_Array_Size (First : Irep; Last : Irep; Idx_Type : Irep)
                              return Irep
   is
      Source_Loc : constant Irep := Get_Source_Location (First);
      Diff : constant Irep :=
        Make_Op_Sub (Rhs             => First,
                     Lhs             => Last,
                     Source_Location => Source_Loc,
                     Overflow_Check  => False,
                     I_Type          => Idx_Type);
      One : constant Irep :=
        Build_Index_Constant (Value      => 1,
                              Source_Loc => Source_Loc);
   begin
      return Make_Op_Add (Rhs             => One,
                          Lhs             => Diff,
                          Source_Location => Source_Loc,
                          Overflow_Check  => False,
                          I_Type          => Idx_Type);
   end Build_Array_Size;

   function Offset_Array_Data (Base : Irep; Offset : Irep) return Irep
   is
      Data_Member : constant Irep :=
        Get_Data_Member (Base, Global_Symbol_Table);
   begin
      return Make_Op_Add (Rhs             => Offset,
                          Lhs             => Data_Member,
                          Source_Location => Get_Source_Location (Base),
                          Overflow_Check  => False,
                          I_Type          => Get_Type (Data_Member));
   end Offset_Array_Data;
end Arrays;
