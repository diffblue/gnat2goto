with Stand;                 use Stand;
with Nlists;                use Nlists;
with Uintp;                 use Uintp;
--  with Namet;                 use Namet;
with Stringt;               use Stringt;
with Tree_Walk;             use Tree_Walk;
with Aggregates;            use Aggregates;
--  with Follow;                use Follow;
with Range_Check;           use Range_Check;
with ASVAT.Size_Model;
with Sem_Util;              use Sem_Util;
with Sem_Eval;              use Sem_Eval;
with Gnat2goto_Itypes;      use Gnat2goto_Itypes;
with Arrays.Low_Level;      use Arrays.Low_Level;
with Symbol_Table_Info;     use Symbol_Table_Info;
package body Arrays is

   function Is_Bounded_Array (Expr : Irep) return Boolean
     renames Arrays.Low_Level.Is_Unconstrained_Array_Result;

   procedure Array_Object_And_Friends
     (Array_Name   : String;
      Array_Node   : Node_Id;
      Array_Bounds : Static_And_Dynamic_Bounds;
      The_Array    : out Irep;
      Source_Loc   : Irep;
      Block        : Irep)
   with Pre => Is_Array_Type (Underlying_Type (Etype (Array_Node)));

   procedure Array_Assignment_Op (Source_Expr  : Node_Id;
                                  N_Dimensions : Pos;
                                  Dest_Bounds  : Static_And_Dynamic_Bounds;
                                  Target_Array : Irep;
                                  Block        : Irep)
   with Pre => Is_Array_Type (Underlying_Type (Etype (Source_Expr)));

   procedure Declare_Array_Friends (Array_Name  : String;
                                    Src_Array   : Node_Id;
                                    Flat_Bounds : Static_And_Dynamic_Bounds;
                                    Block       : Irep)
     with Pre => Is_Array_Type (Etype (Src_Array)) and
          Kind (Block) = I_Code_Block;
   --  An unconstrained array object declaration has to be suplemented
   --  by the declaration of the array friend variables
   --  Array_Name___first_<Dimension> and Array_Name___last_<Dimension>
   --  for each dimension of the array.

   procedure Declare_First_Last_From_Bounds (Prefix     : String;
                                             Dimension  : String;
                                             Index_Type : Irep;
                                             Bounds     : Dimension_Bounds;
                                             Block      : Irep);
   --  This is similar to Declare_First_Last_Vars but is called at a slightly
   --  lower-level with the index node replaced by the Index_Type Irep and
   --  the dimension Bounds.

   procedure Declare_First_Last_From_Object (Target_Name : String;
                                             Object_Name : String;
                                             Dimension   : Positive;
                                             Block       : Irep);

   procedure Declare_First_Last_Params (Prefix     : String;
                                        Dimension  : Positive;
                                        Index      : Node_Id;
                                        Param_List : Irep);
   --  Each dimension of an unconstrained array parameter
   --  introduces two extra friend parameters of mode in to a subprogram.
   --  The values passed in these extra parameters are the lower and upper
   --  bounds of each dimension of the unconstrained array parameter.
   --  The parameters representing the lower and upper bounds of the
   --  dimension are of the base type of the index type.
   --  Their names of the variables are <Prefix>___first_<Dimension>,
   --  and <Prefix>___last_<Dimension>.

   procedure Declare_First_Last_Vars (Prefix     : String;
                                      Dimension  : Positive;
                                      Index      : Node_Id;
                                      Block      : Irep);
   --  A declatation of an unconstrained array object has to be supplemented
   --  by declarations of friend variables to represent the upper and lower
   --  bounds of each dimension of the array object.
   --  The variables representing the lower and upper bounds of the
   --  dimension are of the base type of the index type.
   --  Their names of the variables are <Prefix>___first_<Dimension>,
   --  and <Prefix>___last_<Dimension>.

   function Do_Array_First_Last (N         : Node_Id;
                                 Dimension : Pos)
                                 return Dimension_Bounds;
--       with Pre => Is_Array_Type (Etype (N));

   function Get_Dimension_Index (The_Array : Entity_Id; Dim : Pos)
                                 return Node_Id;

   function Get_Underlying_Array_From_Slice (N : Node_Id) return Node_Id
     with Pre => Nkind (N) = N_Slice;

--     procedure Make_Array_Object_From_Bounds (Block          : Irep;
--                                              Array_Name     : String;
--                                              Target_Type    : Entity_Id;
--                                              Array_Length   : Irep;
--                                         Array_Bounds   : Dimension_Bounds;
--                                              Needs_Size_Var : Boolean;
--                                              Source_Loc     : Irep;
--                                              The_Array      : out Irep)
--       with Pre => (Is_Array_Type (Target_Type) and
--                   not Is_Constrained (Target_Type)) and then
--                   Number_Dimensions (Target_Type) = 1;
   --  Decalre a one-dimensional array from the given target type,
   --  its length and its bounds.
   --  Also declare the First and Last companion variables for the
   --  unconstrained array.

   function Determine_Array_Bounds_From_Initialization
     (Init_Expr    : Node_Id) return Static_And_Dynamic_Bounds
   with Pre => Is_Array_Type (Underlying_Type (Etype (Init_Expr)));

   function Make_Constrained_Array_Subtype (Declaration : Node_Id) return Irep;

   function Make_Unconstrained_Array_Subtype (Declaration    : Node_Id;
                                              Component_Type : Entity_Id)
                                              return Irep;
   procedure Update_Array_From_Concatenation
           (Block        : Irep;
            Concat      : Node_Id;
            Dest_Bounds : Static_And_Dynamic_Bounds;
            Dest_Array  : Irep)
     with Pre => Nkind (Concat) = N_Op_Concat and
                 Kind (Get_Type (Dest_Array)) = I_Array_Type;

   procedure Update_Array_From_Slice
           (Block       : Irep;
            Slice       : Node_Id;
            Dest_Array  : Irep;
            Dest_Bounds : Static_And_Dynamic_Bounds)
     with Pre => Nkind (Slice) = N_Slice and
     Kind (Get_Type (Dest_Array)) = I_Array_Type;

   procedure Update_Array_From_String_Literal
     (Block        : Irep;
      Str_Lit      : Node_Id;
      Dest_Array   : Irep)
     with Pre => Nkind (Str_Lit) = N_String_Literal;

   procedure Array_Assignment_Op (Source_Expr  : Node_Id;
                                  N_Dimensions : Pos;
                                  Dest_Bounds  : Static_And_Dynamic_Bounds;
                                  Target_Array : Irep;
                                  Block        : Irep)
   is
      RHS_Node_Kind      : constant Node_Kind := Nkind (Source_Expr);
      RHS_Entity         : constant Node_Id :=
        (if RHS_Node_Kind in N_Entity then
            Source_Expr
         elsif RHS_Node_Kind in N_Has_Entity then
            Entity (Source_Expr)
         else
            Types.Empty);
      RHS_Is_Object      : constant Boolean :=
        Present (RHS_Entity) and then Is_Object (RHS_Entity);

      Source_Type        : constant Entity_Id :=
        Underlying_Type (Etype (Source_Expr));
      Source_I_Type      : constant Irep :=
        (if RHS_Is_Object then
            Global_Symbol_Table (Intern (Unique_Name (RHS_Entity))).SymType
         else
            Do_Type_Reference (Source_Type));
   begin
      if RHS_Node_Kind = N_Aggregate then
         Update_Array_From_Aggregate
           (Block        => Block,
            Agg          => Source_Expr,
            N_Dimensions => N_Dimensions,
            Dest_Bounds  => Dest_Bounds,
            Dest_Array   => Target_Array);
      elsif RHS_Node_Kind = N_String_Literal then
         Update_Array_From_String_Literal
            (Block        => Block,
             Str_Lit      => Source_Expr,
             Dest_Array   => Target_Array);
      elsif RHS_Node_Kind = N_Slice then
         Update_Array_From_Slice
           (Block       => Block,
            Slice       => Source_Expr,
            Dest_Array  => Target_Array,
            Dest_Bounds => Dest_Bounds);
      elsif RHS_Node_Kind = N_Op_Concat then
         Update_Array_From_Concatenation
           (Block       => Block,
            Concat      => Source_Expr,
            Dest_Array  => Target_Array,
            Dest_Bounds => Dest_Bounds);
      else
         --  ***********************************************************
         --  TODO: Variable Arrays.
         --  This check and reporting should be removed
         --  when cbmc properly handles arrays with bounds specified by
         --  a variable results are supported.
         if RHS_Node_Kind = N_Function_Call and then
           not All_Dimensions_Static (Source_Type)
         then
            Report_Unhandled_Node_Empty
              (N        => Source_Expr,
               Fun_Name => "Array_Assignment_Op",
               Message  =>
                 "Calling a function returning an array with non-static bounds"
               & " is currently unsupported");
            --  *******************************************************
         end if;

         declare
            Resolved_Source_Expr : constant Irep :=
              Typecast_If_Necessary
                (Expr           => Do_Expression (Source_Expr),
                 New_Type       => Source_I_Type,
                 A_Symbol_Table => Global_Symbol_Table);
            Source_Bounds : constant Static_And_Dynamic_Bounds :=
              Multi_Dimension_Flat_Bounds (Source_Expr);
         begin
            Assign_Array
              (Block         => Block,
               Destination   => Target_Array,
               Dest_Bounds   => Dest_Bounds,
               Source        => Resolved_Source_Expr,
               Source_Bounds => Source_Bounds);
         end;
      end if;
   end Array_Assignment_Op;

   procedure Array_Object_And_Friends
     (Array_Name   : String;
      Array_Node   : Node_Id;
      Array_Bounds : Static_And_Dynamic_Bounds;
      The_Array    : out Irep;
      Source_Loc   : Irep;
      Block        : Irep)
   is
      Src_Array_Kind   : constant Node_Kind := Nkind (Array_Node);
      Id               : constant Symbol_Id := Intern (Array_Name);
      Src_Array_Type   : constant Entity_Id :=
        Underlying_Type (Etype (Array_Node));
      Src_Entity       : constant Entity_Id :=
        (if Src_Array_Kind in N_Entity then
            Array_Node
         elsif Src_Array_Kind in N_Has_Entity then
            Entity (Array_Node)
         else
            Types.Empty);
      Src_Is_Object     : constant Boolean :=
       (if Present (Src_Entity) then
            Is_Object (Src_Entity)
         else
            False);

      Comp_Type        : constant Entity_Id :=
        Component_Type (Src_Array_Type);
      Comp_Irep        : constant Irep :=
        Make_Resolved_I_Type (Comp_Type);

      Src_Array_I_Type : constant Irep := Do_Type_Reference (Src_Array_Type);

      Array_Size : constant Static_And_Dynamic_Index :=
        Get_Array_Size_From_Bounds (Array_Bounds);

      Needs_Size_Var : constant Boolean :=
        (not Is_Constrained (Src_Array_Type) or else
             (not Array_Bounds.Has_Static_Bounds and then
              Is_Itype (Src_Array_Type)));
      --  A constrained array with non-static bounds will have had
      --  the size variable declared when it was declared unless it
      --  is an Itype declaration.

      Array_Size_Var  : constant Irep :=
        (if Needs_Size_Var then
            Make_Symbol_Expr
           (Source_Location => Source_Loc,
            I_Type          => Index_T,
            Range_Check     => False,
            Identifier      => Array_Name & "_$array_size")
         else
            Ireps.Empty);
      Array_Type_Irep : constant Irep :=
        (if Array_Size_Var = Ireps.Empty then
         --  Does not need a size var, which means the array subtype has
         --  static bounds or the size variable has been declared and
         --  intialised in the goto code when the array was declared.
         --  In either case the Irep array type from the Do_Type_Reference
         --  can be used.
            Src_Array_I_Type
         else
         --  A new variable has to be declared to represent the size of
         --  the goto array object.
            Make_Array_Type
           (I_Subtype => Comp_Irep,
            Size      => Array_Size_Var));

      Array_Irep : constant Irep :=
        Make_Symbol_Expr
          (Source_Location => Source_Loc,
           I_Type          => Array_Type_Irep,
           Range_Check     => False,
           Identifier      => Array_Name);
      Decl      : constant Irep := Make_Code_Decl
        (Symbol => Array_Irep,
         Source_Location => Source_Loc);
   begin
      if not Global_Symbol_Table.Contains (Id) then
         --  If a size variable is needed to define the size of the
         --  goto array object, declare it before the array.
         if Needs_Size_Var then
            Append_Declare_And_Init
              (Symbol     => Array_Size_Var,
               Value      => Array_Size.Dynamic_Index,
               Block      => Block,
               Source_Loc => Source_Loc);
         end if;

         Append_Op (Block, Decl);
         New_Object_Symbol_Entry
           (Object_Name       => Id,
            Object_Type       => Array_Type_Irep,
            Object_Init_Value => Ireps.Empty,
            A_Symbol_Table    => Global_Symbol_Table);

         --  The model size of the object has to be recorded.
         if Is_Constrained (Src_Array_Type) then
            if ASVAT.Size_Model.Has_Static_Size (Src_Array_Type) then
               ASVAT.Size_Model.Set_Static_Size
                 (Id         => Id,
                  Model_Size =>
                    ASVAT.Size_Model.Static_Size (Src_Array_Type));
            else
               ASVAT.Size_Model.Set_Computed_Size
                 (Id        => Id,
                  Size_Expr =>
                    ASVAT.Size_Model.Computed_Size (Src_Array_Type));
            end if;
         elsif Src_Is_Object then
            if ASVAT.Size_Model.Has_Static_Size (Src_Entity) then
               ASVAT.Size_Model.Set_Static_Size
                 (Id         => Id,
                  Model_Size =>
                    ASVAT.Size_Model.Static_Size (Src_Entity));
            else
               ASVAT.Size_Model.Set_Computed_Size
                 (Id        => Id,
                  Size_Expr =>
                    ASVAT.Size_Model.Computed_Size (Src_Entity));
            end if;

         elsif not Array_Bounds.Is_Unconstrained then
            ASVAT.Size_Model.Set_Computed_Size
              (Id        => Id,
               Size_Expr => Make_Op_Mul
                 (Rhs             => Array_Size.Dynamic_Index,
                  Lhs             => Typecast_If_Necessary
                    (Expr           =>
                         ASVAT.Size_Model.Computed_Size (Comp_Type),
                     New_Type       => Int32_T,
                     A_Symbol_Table => Global_Symbol_Table),
                  Source_Location => Source_Loc,
                  I_Type          => Int32_T));
         else
            Report_Unhandled_Node_Empty
              (N        => Array_Node,
               Fun_Name => "Array_Object_And_Friends",
               Message  => "Unexpected unconstrained array result");
         end if;

         --  The first and last variables for each dimension have to
         --  added to the symbol table and initialised.

         Declare_Array_Friends
           (Array_Name  => Array_Name,
            Src_Array   => Array_Node,
            Flat_Bounds => Array_Bounds,
            Block      => Block);
      end if;
      --  Ensure the out variables are set.
      The_Array := Array_Irep;
   end Array_Object_And_Friends;

   -----------------------------
   -- Do_Array_Assignment_Op  --
   -----------------------------

   procedure Do_Array_Assignment_Op (Block       : Irep;
                                     Destination : Irep;
                                     Dest_Type   : Entity_Id;
                                     Source_Expr : Node_Id)
   is
      Underlying : constant Entity_Id := Underlying_Type (Dest_Type);
      Array_Bounds : constant Static_And_Dynamic_Bounds :=
            Multi_Dimension_Flat_Bounds (Underlying);
   begin
      if Array_Bounds.Is_Unconstrained then
         Report_Unhandled_Node_Empty
           (N        => Source_Expr,
            Fun_Name => "Do_Array_Assignment_Op",
            Message  => "Assignment expression cannot be unconstrained");
      else
         Array_Assignment_Op
           (Source_Expr  => Source_Expr,
            N_Dimensions => Number_Dimensions (Underlying),
            Dest_Bounds  => Array_Bounds,
            Target_Array => Destination,
            Block        => Block);
      end if;
   end Do_Array_Assignment_Op;

   ----------------------------------
   -- Do_Array_Object_Declaration  --
   ----------------------------------

   procedure Do_Array_Object_Declaration (Block       : Irep;
                                          Dec_Node    : Node_Id;
                                          Target_Type : Entity_Id;
                                          Array_Name  : String;
                                          Init_Expr   : Node_Id)
   is
      Source_Loc     : constant Irep := Get_Source_Location (Dec_Node);
      Target_Def     : constant Entity_Id := Defining_Identifier (Dec_Node);
      Array_Id       : constant Symbol_Id := Intern (Array_Name);

      Array_Bounds : Static_And_Dynamic_Bounds :=
            Multi_Dimension_Flat_Bounds (Dec_Node);
      Comp_Type        : constant Entity_Id :=
        Component_Type (Target_Type);
      Comp_Irep      : constant Irep :=
        Make_Resolved_I_Type (Comp_Type);

      The_Array    : Irep;
   begin
      if not Array_Bounds.Is_Unconstrained then
         --  The destination array object is constrained.
         --  Create the array symbol with the target type
         --  but do not perform initialization.
         --  Array initialization is performed below after the if statement.
         declare
            Array_Length     : constant Irep :=
              (if Array_Bounds.Has_Static_Bounds then
                  Integer_Constant_To_Expr
                 (Value           => UI_From_Int
                      (Array_Bounds.High_Static + 1),
                  Expr_Type       => Index_T,
                  Source_Location => Source_Loc)
               else
                  Make_Op_Add
                 (Rhs             => Index_T_One,
                  Lhs             => Array_Bounds.High_Dynamic,
                  Source_Location => Source_Loc,
                  Overflow_Check  => False,
                  I_Type          => Index_T,
                  Range_Check     => False));

            --  If the bounds of the array are static then Array_Length is a
            --  constant and can be used directly to define the size of the
            --  array.  However, if the bounds of the array are not static,
            --  goto requires that a variable, not an expresion,
            --  is used to define the size of the array.
            Arr_Len_Irep : constant Irep :=
              (if Array_Bounds.Has_Static_Bounds then
                  Typecast_If_Necessary
                 (Expr           => Array_Length,
                  New_Type       => Index_T,
                  A_Symbol_Table => Global_Symbol_Table)
               else
                  Make_Symbol_Expr
                 (Source_Location => Source_Loc,
                  I_Type          => Index_T,
                  Range_Check     => False,
                  Identifier      => Array_Name & "_$array_size"));

            Array_Itype      : constant Irep :=
              Make_Array_Type
                (I_Subtype => Comp_Irep,
                 Size      => Arr_Len_Irep);

            Array_Model_Size : constant Irep :=
              Make_Op_Mul
                (Rhs             => Typecast_If_Necessary
                   (Expr           =>
                          ASVAT.Size_Model.Computed_Size (Comp_Type),
                    New_Type       => Index_T,
                    A_Symbol_Table => Global_Symbol_Table),
                 Lhs             => Array_Length,
                 Source_Location => Source_Loc,
                 Overflow_Check  => False,
                 I_Type          => Index_T,
                 Range_Check     => False);

            Decl : constant Irep := Make_Code_Decl
              (Symbol => Arr_Len_Irep,
               Source_Location => Source_Loc);

         begin
            --  Set the ASVAT.Size_Model size for the array.
            ASVAT.Size_Model.Set_Computed_Size
              (Target_Def, Array_Model_Size);

            if not Array_Bounds.Has_Static_Bounds then
               --  The auxilliary variable used to define the array size
               --  has to be declared and initialised.
               --  Declare the variable in the goto code
               Append_Op (Block, Decl);
               --  and assign the array length expression.
               Append_Op (Block,
                          Make_Code_Assign
                            (Rhs             => Typecast_If_Necessary
                               (Expr           => Array_Length,
                                New_Type       => Index_T,
                                A_Symbol_Table => Global_Symbol_Table),
                             Lhs             => Arr_Len_Irep,
                             Source_Location => Source_Loc,
                             I_Type          => Index_T,
                             Range_Check     => False));
            end if;

            The_Array :=
              Make_Symbol_Expr
                (Source_Location => Source_Loc,
                 I_Type          => Array_Itype,
                 Identifier      => Array_Name);

            --  Do not inintalize here, so Init_Expr_Irep = Ireps.Empty.
            Do_Plain_Object_Declaration
              (Block          => Block,
               Object_Sym     => The_Array,
               Object_Name    => Array_Name,
               Object_Def     => Target_Def,
               Init_Expr_Irep => Ireps.Empty);
         end;
      else
         --  The array length, i.e. its goto I_Array_Type,
         --  for an unconstrained array object has to be determined from its
         --  initialization, which must be present.
         Array_Bounds :=
           Determine_Array_Bounds_From_Initialization (Init_Expr);
         if Array_Bounds.Is_Unconstrained then
            Report_Unhandled_Node_Empty
              (Init_Expr,
               "Make_Constrained_Array_From_Initialization",
               "Unsupported unconstrained array initialization by " &
                 Node_Kind'Image (Nkind (Init_Expr)));
         end if;

         Array_Object_And_Friends
           (Array_Name   => Array_Name,
            The_Array    => The_Array,
            Array_Bounds => Array_Bounds,
            Array_Node   => Init_Expr,
            Source_Loc   => Source_Loc,
            Block        => Block);
      end if;

      --  The array object should now be in the symbol table.
      pragma Assert (Global_Symbol_Table.Contains (Array_Id));

      --  Now do its initialization, if any.
      if Present (Init_Expr) then
         Array_Assignment_Op
           (Source_Expr  => Init_Expr,
            N_Dimensions => Number_Dimensions (Target_Type),
            Dest_Bounds  => Array_Bounds,
            Target_Array => The_Array,
            Block        => Block);
      end if;

   end Do_Array_Object_Declaration;

   ---------------------------
   -- All_Dimensions_Static --
   ---------------------------

   function All_Dimensions_Static (The_Array : Entity_Id) return Boolean
     renames Arrays.Low_Level.All_Dimensions_Static;

   ------------------------
   -- Add_Array_Friends --
   ------------------------

   procedure Add_Array_Friends (Array_Name : String;
                                Array_Type : Entity_Id;
                                Param_List : Irep)
   is
      Index_Iter : Node_Id := First_Index (Array_Type);
   begin
      for Dimension in 1 .. Integer (Number_Dimensions (Array_Type)) loop
         pragma Assert (Present (Index_Iter));
         Declare_First_Last_Params
           (Prefix     => Array_Name,
            Dimension  => Dimension,
            Index      => Index_Iter,
            Param_List => Param_List);
         Index_Iter := Next_Index (Index_Iter);
      end loop;
   end Add_Array_Friends;

   ---------------------------
   -- Declare_Array_Friends --
   ---------------------------

   procedure Declare_Array_Friends (Array_Name  : String;
                                    Src_Array   : Node_Id;
                                    Flat_Bounds : Static_And_Dynamic_Bounds;
                                    Block       : Irep)
   is
      Source_Location : constant Irep := Get_Source_Location (Block);
      Array_Type_Pre  : constant Entity_Id := Etype (Src_Array);
      Array_Type      : constant Entity_Id :=
        (if Is_Access_Type (Array_Type_Pre) then
              Designated_Type (Array_Type_Pre) else
         Array_Type_Pre);
      Src_Node_Kind   : constant Node_Kind := Nkind (Src_Array);
      Src_Is_Object   : constant Boolean :=
        (if Src_Node_Kind in N_Entity then
            Is_Object (Src_Array)
         elsif Src_Node_Kind in N_Has_Entity and then
         Nkind (Entity (Src_Array)) in N_Entity
         then
            Is_Object (Entity (Src_Array))
         else
            False);
   begin
      pragma Assert (Nkind (Array_Type) in N_Entity);
      if Ekind (Array_Type) = E_String_Literal_Subtype then
         --  A string literal can only have 1 dimension but
         --  gnat does not give a first index in the atree for string literals.
         declare
            --  The index subtype of a string is Positive
            Str_Index_Type     : constant Irep :=
              Do_Type_Reference (Standard_Positive);
            Str_Lit_Low        : constant Node_Id :=
              String_Literal_Low_Bound (Array_Type);
            Str_Lit_Is_Static  : constant Boolean :=
              Is_OK_Static_Expression (Str_Lit_Low);
            Str_Lit_Low_Static : constant Uint :=
              (if Str_Lit_Is_Static then
                    Expr_Value (Str_Lit_Low)
               else
                  Uint_1);
            Str_Lit_Low_Irep   : constant Irep := Do_Expression (Str_Lit_Low);

            Str_Lit_Length     : constant Uint :=
              String_Literal_Length (Array_Type);

            Str_Lit_High_Irep  : constant Irep :=
              (if Str_Lit_Is_Static then
                  Integer_Constant_To_Expr
                 (Value           =>
                      Str_Lit_Low_Static + Str_Lit_Length - Uint_1,
                  Expr_Type       => Str_Index_Type,
                  Source_Location => Source_Location)
               else
                  Make_Op_Sub
                 (Rhs             =>
                      Integer_Constant_To_Expr
                    (Value           => Uint_1,
                     Expr_Type       => Str_Index_Type,
                     Source_Location => Source_Location),
                  Lhs             => Make_Op_Add
                    (Rhs             =>
                         Integer_Constant_To_Expr
                       (Value           => Str_Lit_Length,
                        Expr_Type       => Str_Index_Type,
                        Source_Location => Source_Location),
                     Lhs             => Str_Lit_Low_Irep,
                     Source_Location => Source_Location,
                     I_Type          => Str_Index_Type),
                  Source_Location => Source_Location,
                  I_Type          => Str_Index_Type));

            Bounds : constant Dimension_Bounds := Dimension_Bounds'
              (Low  => Str_Lit_Low_Irep,
               High => Str_Lit_High_Irep);
         begin
            Declare_First_Last_From_Bounds
              (Prefix     => Array_Name,
               Dimension  => "1",
               Index_Type => Str_Index_Type,
               Bounds     => Bounds,
               Block      => Block);
         end;
      elsif Is_Constrained (Array_Type) then
         declare
            Index_Iter : Node_Id := First_Index (Array_Type);
         begin
            for Dimension in 1 .. Integer (Number_Dimensions (Array_Type)) loop
               pragma Assert (Present (Index_Iter));
               Declare_First_Last_Vars
                 (Prefix     => Array_Name,
                  Dimension  => Dimension,
                  Index      => Index_Iter,
                  Block      => Block);
               Index_Iter := Next_Index (Index_Iter);
            end loop;
         end;
      elsif Src_Is_Object then
         declare
            Src_Entity : constant Entity_Id :=
              (if Src_Node_Kind in N_Entity then
                  Src_Array
               else
                  Entity (Src_Array));
            Src_Name   : constant String := Unique_Name (Src_Entity);
         begin
            for Dimension in 1 .. Integer (Number_Dimensions (Array_Type)) loop
               Declare_First_Last_From_Object
                 (Target_Name => Array_Name,
                  Object_Name => Src_Name,
                  Dimension   => Dimension,
                  Block       => Block);
            end loop;
         end;
      elsif Src_Node_Kind = N_Function_Call or else
        Is_Access_Type (Array_Type_Pre)
      then
         --  It is an unconstrained array result.
         declare
            Src_Irep_Pre : constant Irep := Do_Expression (Src_Array);
            Src_Irep     : constant Irep :=
              (if Is_Access_Type (Array_Type_Pre) then
                    Make_Dereference_Expr
                 (Object          => Src_Irep_Pre,
                  Source_Location => Source_Location,
                  I_Type          => Do_Type_Reference (Array_Type))
               else
                  Src_Irep_Pre);
            --  Determine the I_Type of the bounds array
            Src_I_Type : constant Irep := Get_Type (Src_Irep);
         begin
            if Is_Unconstrained_Array_Result (Src_Irep) then
               declare
                  Comp_List  : constant Irep_List :=
                    Get_Component (Get_Components (Src_I_Type));
                  List_Cur   : constant List_Cursor := List_First (Comp_List);
                  Bounds     : constant Irep :=
                    List_Element (Comp_List, List_Cur);
                  Bounds_Array_Type : constant Irep := Get_Type (Bounds);
                  Bounds_Arr : constant Irep := Make_Member_Expr
                    (Compound         => Do_Expression (Src_Array),
                     Source_Location  => Source_Location,
                     I_Type           => Bounds_Array_Type,
                     Component_Name   => Array_Struc_Bounds);

                  --  Get the bounds field from the Fun_Result.
                  Bounds_Array        : constant Irep :=
                    Fresh_Var_Symbol_Expr (Bounds_Array_Type, "bounds_array");

                  Index_Iter : Node_Id := First_Index (Array_Type);
               begin
                  Append_Declare_And_Init
                    (Symbol     => Bounds_Array,
                     Value      => Bounds_Arr,
                     Block      => Block,
                     Source_Loc => Source_Location);

                  for Dimension in 1 .. Number_Dimensions (Array_Type) loop
                     declare
                        Dim_First : constant Irep :=
                          Make_Index_Expr
                            (I_Array         => Bounds_Array,
                             Index           => Integer_Constant_To_Expr
                               (Value           => Bounds_First (Dimension),
                                Expr_Type       => Index_T,
                                Source_Location => Source_Location),
                             Source_Location => Source_Location,
                             I_Type          => Bounds_Component);
                        Dim_Last : constant Irep :=
                          Make_Index_Expr
                            (I_Array         => Bounds_Array,
                             Index           => Integer_Constant_To_Expr
                               (Value           => Bounds_Last (Dimension),
                                Expr_Type       => Index_T,
                                Source_Location => Source_Location),
                             Source_Location => Source_Location,
                             I_Type          => Bounds_Component);
                        Bounds         : constant Dimension_Bounds :=
                          Dimension_Bounds'
                            (Low  => Dim_First,
                             High => Dim_Last);
                        Dim_String_Pre   : constant String :=
                          Int'Image (Dimension);
                        Dim_String       : constant String :=
                          Dim_String_Pre (2 .. Dim_String_Pre'Last);

                        Index_I_Type     : constant Irep :=
                          Make_Resolved_I_Type (Etype (Index_Iter));

                     begin
                        Declare_First_Last_From_Bounds
                          (Prefix     => Array_Name,
                           Dimension  => Dim_String,
                           Index_Type => Index_I_Type,
                     Bounds     => Bounds,
                     Block      => Block);
                     end;
                     Index_Iter := Next_Index (Index_Iter);
                  end loop;
               end;
            else
               Report_Unhandled_Node_Empty
                 (N        => Src_Array,
                  Fun_Name => "Declare_Array_Friends",
                  Message  => "Expected a bounded array");
            end if;
         end;
      elsif Src_Node_Kind = N_Op_Concat then
         --  The source array expression is a concatination.
         --  Concatinations are one dimensional arrays
         --  An object of an unconstrained type
         --  (if the object is declared without a constraint)
         --  will have a lower bound of Index_Type'First and an upper bound
         --  of Index_Type'First + Flat_Bounds.High
         declare
            Index_Type      : constant Irep :=
              Do_Type_Reference (Etype (First_Index (Array_Type)));
            Unconstr_Bounds : constant Dimension_Bounds :=
              Get_Bounds_From_Index (First_Index (Array_Type));
            Bounds : constant Dimension_Bounds :=
              Dimension_Bounds'
                (Low  => Typecast_If_Necessary
                   (Expr           => Unconstr_Bounds.Low,
                    New_Type       => Index_T,
                    A_Symbol_Table => Global_Symbol_Table),
                 High => Typecast_If_Necessary
                   (Expr           => Make_Op_Add
                      (Rhs             => Typecast_If_Necessary
                           (Expr           => Flat_Bounds.High_Dynamic,
                            New_Type       => Index_T,
                            A_Symbol_Table => Global_Symbol_Table),
                       Lhs             => Typecast_If_Necessary
                         (Expr           => Unconstr_Bounds.Low,
                          New_Type       => Index_T,
                          A_Symbol_Table => Global_Symbol_Table),
                       Source_Location => Source_Location,
                       I_Type          => Index_T),
                    New_Type       => Index_T,
                    A_Symbol_Table => Global_Symbol_Table));
         begin
            Declare_First_Last_From_Bounds
              (Prefix     => Array_Name,
               Dimension  => "1",
               Index_Type => Index_Type,
               Bounds     => Bounds,
               Block      => Block);
         end;
      else
         Report_Unhandled_Node_Empty
           (N        => Array_Type,
            Fun_Name => "Declare_Array_Friends",
            Message  =>
              "Cannot create First and Last from unconstrained array");
      end if;
   end Declare_Array_Friends;

   ----------------------------------------
   -- Declare_First_And_Last_From_Bounds --
   ----------------------------------------

   procedure Declare_First_Last_From_Bounds (Prefix     : String;
                                             Dimension  : String;
                                             Index_Type : Irep;
                                             Bounds     : Dimension_Bounds;
                                             Block      : Irep)
   is
      Source_Loc      : constant Irep := Internal_Source_Location;
      First_Name      : constant String :=
        Prefix & First_Var_Str & Dimension;
      First_Name_Id   : constant Symbol_Id := Intern (First_Name);
      Last_Name       : constant String :=
        Prefix & Last_Var_Str & Dimension;
      Last_Name_Id    : constant Symbol_Id := Intern (Last_Name);

      First_Sym : constant Irep :=
        Make_Symbol_Expr
          (Source_Location => Source_Loc,
           I_Type          => Index_Type,
           Range_Check     => False,
           Identifier      => First_Name);
      Last_Sym : constant Irep :=
        Make_Symbol_Expr
          (Source_Location => Source_Loc,
           I_Type          => Index_Type,
           Range_Check     => False,
           Identifier      => Last_Name);

      First_Val : constant Irep :=
        Typecast_If_Necessary
          (Expr           => Bounds.Low,
           New_Type       => Index_Type,
           A_Symbol_Table => Global_Symbol_Table);

      Last_Val : constant Irep :=
        Typecast_If_Necessary
          (Expr           => Bounds.High,
           New_Type       => Index_Type,
           A_Symbol_Table => Global_Symbol_Table);
   begin
      --  Add the first and last variables to the symbol table.
      New_Object_Symbol_Entry
        (Object_Name       => First_Name_Id,
         Object_Type       => Index_Type,
         Object_Init_Value => Bounds.Low,
         A_Symbol_Table    => Global_Symbol_Table);
      New_Object_Symbol_Entry
        (Object_Name       => Last_Name_Id,
         Object_Type       => Index_Type,
         Object_Init_Value => Bounds.High,
         A_Symbol_Table    => Global_Symbol_Table);

      --  Declare and assign values in goto code.
      Append_Declare_And_Init
        (Symbol     => First_Sym,
         Value      => First_Val,
         Block      => Block,
         Source_Loc => Source_Loc);
      Append_Declare_And_Init
        (Symbol     => Last_Sym,
         Value      => Last_Val,
         Block      => Block,
         Source_Loc => Source_Loc);
   end Declare_First_Last_From_Bounds;

   -----------------------------------------
   -- Declare_First_And_Last_From_Object --
   -----------------------------------------

   procedure Declare_First_Last_From_Object (Target_Name : String;
                                             Object_Name : String;
                                             Dimension   : Positive;
                                             Block       : Irep)
   is
      Dim_String_Pre  : constant String := Integer'Image (Dimension);
      Dim_String      : constant String :=
        Dim_String_Pre (2 .. Dim_String_Pre'Last);
      Source_Loc      : constant Irep := Internal_Source_Location;
      Target_First    : constant String :=
        Target_Name & First_Var_Str & Dim_String;
      Target_First_Id : constant Symbol_Id := Intern (Target_First);
      Target_Last     : constant String :=
        Target_Name & Last_Var_Str & Dim_String;
      Target_Last_Id  : constant Symbol_Id := Intern (Target_Last);

      Object_First    : constant String :=
        Object_Name & First_Var_Str & Dim_String;
      Object_First_Id : constant Symbol_Id := Intern (Object_First);
      Object_Last     : constant String :=
        Object_Name & Last_Var_Str & Dim_String;
      Object_Last_Id  : constant Symbol_Id := Intern (Object_Last);

      pragma Assert (Global_Symbol_Table.Contains (Object_First_Id) and
                     Global_Symbol_Table.Contains (Object_Last_Id));

      Index_Type : constant Irep :=
        Global_Symbol_Table (Object_First_Id).SymType;

      Target_First_Sym : constant Irep :=
        Make_Symbol_Expr
          (Source_Location => Source_Loc,
           I_Type          => Index_Type,
           Range_Check     => False,
           Identifier      => Target_First);
      Target_Last_Sym : constant Irep :=
        Make_Symbol_Expr
          (Source_Location => Source_Loc,
           I_Type          => Index_Type,
           Range_Check     => False,
           Identifier      => Target_Last);

      Object_First_Sym : constant Irep :=
        Make_Symbol_Expr
          (Source_Location => Source_Loc,
           I_Type          => Index_Type,
           Range_Check     => False,
           Identifier      => Object_First);
      Object_Last_Sym : constant Irep :=
        Make_Symbol_Expr
          (Source_Location => Source_Loc,
           I_Type          => Index_Type,
           Range_Check     => False,
           Identifier      => Object_Last);

   begin
      --  Add the first and last variables to the symbol table.
      New_Object_Symbol_Entry
        (Object_Name       => Target_First_Id,
         Object_Type       => Index_Type,
         Object_Init_Value => Object_First_Sym,
         A_Symbol_Table    => Global_Symbol_Table);
      New_Object_Symbol_Entry
        (Object_Name       => Target_Last_Id,
         Object_Type       => Index_Type,
         Object_Init_Value => Object_Last_Sym,
         A_Symbol_Table    => Global_Symbol_Table);

      --  Declare and assign values in goto code.
      Append_Declare_And_Init
        (Symbol     => Target_First_Sym,
         Value      => Object_First_Sym,
         Block      => Block,
         Source_Loc => Source_Loc);
      Append_Declare_And_Init
        (Symbol     => Target_Last_Sym,
         Value      => Object_Last_Sym,
         Block      => Block,
         Source_Loc => Source_Loc);
   end Declare_First_Last_From_Object;

   -----------------------------------
   -- Declare_First_And_Last_Params --
   -----------------------------------

   procedure Declare_First_Last_Params (Prefix     : String;
                                        Dimension  : Positive;
                                        Index      : Node_Id;
                                        Param_List : Irep)
   is
      Source_Loc      : constant Irep := Get_Source_Location (Index);
      Number_Str_Raw  : constant String :=
        Integer'Image (Dimension);
      Number_Str      : constant String :=
        Number_Str_Raw (2 .. Number_Str_Raw'Last);
      First_Name      : constant String :=
        Prefix & First_Var_Str & Number_Str;
      First_Name_Id   : constant Symbol_Id := Intern (First_Name);
      Last_Name       : constant String :=
        Prefix & Last_Var_Str & Number_Str;
      Last_Name_Id    : constant Symbol_Id := Intern (Last_Name);

      Index_Type : constant Entity_Id :=
        Base_Type (Etype (Index));
      Index_Id : constant Symbol_Id :=
        Intern (Unique_Name (Index_Type));
      pragma Assert (Global_Symbol_Table.Contains (Index_Id));

      Type_Irep : constant Irep :=
        Do_Type_Reference (Index_Type);

      --  Formal parameters.
      First_Irep : constant Irep := Make_Code_Parameter
        (Source_Location => Source_Loc,
         I_Type => Type_Irep,
         Identifier => First_Name,
         Base_Name => First_Name,
         This => False,
         Default_Value => Ireps.Empty);

      Last_Irep : constant Irep := Make_Code_Parameter
        (Source_Location => Source_Loc,
         I_Type => Type_Irep,
         Identifier => Last_Name,
         Base_Name => Last_Name,
         This => False,
         Default_Value => Ireps.Empty);
   begin
      --  Add the parameters to the symbol table.
      New_Parameter_Symbol_Entry
        (Name_Id        => First_Name_Id,
         BaseName       => First_Name,
         Symbol_Type    => Type_Irep,
         A_Symbol_Table => Global_Symbol_Table);

      New_Parameter_Symbol_Entry
        (Name_Id        => Last_Name_Id,
         BaseName       => Last_Name,
         Symbol_Type    => Type_Irep,
         A_Symbol_Table => Global_Symbol_Table);

      --  Append the parameters to the parameter list.
      Append_Parameter (Param_List, First_Irep);
      Append_Parameter (Param_List, Last_Irep);

   end Declare_First_Last_Params;

   ---------------------------------
   -- Declare_First_And_Last_Vars --
   ---------------------------------

   procedure Declare_First_Last_Vars (Prefix    : String;
                                      Dimension : Positive;
                                      Index     : Node_Id;
                                      Block      : Irep)
   is
      Number_Str_Raw  : constant String :=
        Integer'Image (Dimension);
      Number_Str      : constant String :=
        Number_Str_Raw (2 .. Number_Str_Raw'Last);

      Index_Type : constant Entity_Id :=
        Base_Type (Etype (Index));

      Bounds : constant Dimension_Bounds := Get_Bounds_From_Index (Index);
   begin
      Declare_First_Last_From_Bounds
        (Prefix     => Prefix,
         Dimension  => Number_Str,
         Index_Type => Do_Type_Reference (Index_Type),
         Bounds     => Bounds,
         Block      => Block);
   end Declare_First_Last_Vars;

   --------------------------------
   -- Do_Aggregate_Literal_Array --
   --------------------------------

   function Do_Aggregate_Literal_Array (N : Node_Id) return Irep is
      Source_Location   : constant Irep := Get_Source_Location (N);
      Positional_Assoc  : constant Boolean := Present (Expressions (N));
      Has_Static_Bounds : constant Boolean :=
        Is_OK_Static_Range (Aggregate_Bounds (N));
      Aggregate_Subtype : constant Entity_Id := Etype (N);
      New_Name          : constant String := Fresh_Var_Name ("aggregate_");
      Aggregate_Obj     : constant String := New_Name & "_obj";
      Aggregate_Func    : constant String := New_Name & "_fun";
--        Aggregate_Loop    : constant String := New_Name & "_loop";
      Subtype_Irep      : constant Irep :=
        Do_Type_Reference (Aggregate_Subtype);
      Component_Irep    : constant Irep :=
        Make_Resolved_I_Type (Component_Type (Aggregate_Subtype));
      Obj_Irep          : constant Irep := Make_Symbol_Expr
        (Source_Location => Source_Location,
         I_Type          => Subtype_Irep,
         Range_Check     => False,
         Identifier      => Aggregate_Obj);
      Func_Irep         : constant Irep :=
        Make_Code_Type (Parameters  => Make_Parameter_List,  -- No parameters.
                        Ellipsis    => False,
                        Return_Type => Subtype_Irep,
                        Inlined     => False,
                        Knr         => False);
      Result_Block      : constant Irep := Make_Code_Block (Source_Location);
      Obj_Dec           : constant Irep := Make_Code_Decl
        (Symbol          => Obj_Irep,
         Source_Location => Source_Location,
         I_Type          => Subtype_Irep,
         Range_Check     => False);

   begin
      --  First add the aggregate array object to the symbol table.
      New_Object_Symbol_Entry
        (Object_Name       => Intern (Aggregate_Obj),
               Object_Type       => Subtype_Irep,
               Object_Init_Value => Ireps.Empty,
               A_Symbol_Table    => Global_Symbol_Table);

      --  Make the body of the function that builds the aggregate
      --  First the declaration of the aggregate array;
      Append_Op (Result_Block, Obj_Dec);

      if Has_Static_Bounds then
         declare
            Low_Expr  : constant Uint :=
              Expr_Value (Low_Bound  (Aggregate_Bounds (N)));
            High_Expr : constant Uint :=
              Expr_Value (High_Bound (Aggregate_Bounds (N)));
            Low  : constant Int := UI_To_Int (Low_Expr);
            High : constant Int := UI_To_Int (High_Expr);
         begin
            if Positional_Assoc then
               Array_Static_Positional
                 (Block      => Result_Block,
                  Low_Bound  => 0,
                  High_Bound => High - Low,
                  N          => N,
                  Target     => Obj_Irep,
                  Comp_Type  => Component_Irep);
            elsif Present (Component_Associations (N)) then
               --  Named associations.
               Array_Static_Named_Assoc
                 (Block      => Result_Block,
                  Low_Bound  => 0,
                  High_Bound => High - Low,
                  N          => N,
                  Target     => Obj_Irep,
                  Comp_Type  => Component_Irep);
            else
               Report_Unhandled_Node_Empty
                 (N        => N,
                  Fun_Name => "Do_Aggregate_Array_Literal",
                  Message  =>
                 "Aggregate has neither Positional or Named arguments");
            end if;
         end;
      else
         declare
            Bounds : constant Dimension_Bounds :=
              Get_Dimension_Bounds (N, 1, Aggregate_Bounds (N));
         begin
            if Positional_Assoc then
               Array_Dynamic_Positional
                 (Block      => Result_Block,
                  Low_Bound  => Index_T_Zero,
                  High_Bound => Make_Zero_Index
                    (Index    => Bounds.High,
                     First    => Bounds.Low,
                     Location => Source_Location),
                  N          => N,
                  Target     => Obj_Irep,
                  Comp_Type  => Component_Irep);
            else
               Array_Dynamic_Named_Assoc
                 (Block      => Result_Block,
                  Low_Bound  => Index_T_Zero,
                  High_Bound => Make_Zero_Index
                    (Index    => Bounds.High,
                     First    => Bounds.Low,
                     Location => Source_Location),
                  N          => N,
                  Target     => Obj_Irep,
                  Comp_Type  => Component_Irep);
            end if;
         end;
      end if;

      --  Now add the return statement.
      Append_Op (Result_Block,
                 Make_Code_Return (Return_Value    => Obj_Irep,
                                   Source_Location => Source_Location));
      --  Create the aggregate function from the body
      --  and return a call to the function.
      declare
         Aggregate_Func_Symbol : constant Symbol :=
           New_Function_Symbol_Entry
             (Name           => Aggregate_Func,
              Symbol_Type    => Func_Irep,
              Value          => Result_Block,
              A_Symbol_Table => Global_Symbol_Table);
         Func_Call : constant Irep :=
           Make_Side_Effect_Expr_Function_Call
             (Arguments       => Make_Argument_List,  -- Null arg list.
              I_Function      => Symbol_Expr (Aggregate_Func_Symbol),
              Source_Location => Source_Location,
              I_Type          => Subtype_Irep,
              Range_Check     => False);
      begin
         return Func_Call;
      end;
   end Do_Aggregate_Literal_Array;

   -----------------------
   -- Do_String_Literal --
   -----------------------

   function Do_String_Literal (N : Node_Id) return Irep is
      Source_Location   : constant Irep := Get_Source_Location (N);
      --  String literals are stored in string constants table described
      --  Stringst.
      --  Their lower bound is always 1 and therefore the string length
      --  is also the string litera['s high bound.
      Str_Id            : constant String_Id := Strval (N);
      Str_Lit_High      : constant Nat := String_Length (Str_Id);
      Str_Lit_Size_Irep : constant Irep :=
        Integer_Constant_To_Expr
          (Value           => UI_From_Int (Str_Lit_High - 1),
           Expr_Type       => Index_T,
           Source_Location => Source_Location);
      --  To Do: This needs to changed to Make_Char_Type ...
      Component_Irep    : constant Irep := Make_Unsignedbv_Type (8);
      Str_Subtype       : constant Irep :=
        Make_Array_Type
          (I_Subtype => Component_Irep,
           Size      => Str_Lit_Size_Irep);

      New_Name          : constant String := Fresh_Var_Name ("string_");
      String_Obj        : constant String := New_Name & "_obj";
      String_Func       : constant String := New_Name & "_fun";
      Obj_Irep          : constant Irep := Make_Symbol_Expr
        (Source_Location => Source_Location,
         I_Type          => Str_Subtype,
         Range_Check     => False,
         Identifier      => String_Obj);
      Func_Irep         : constant Irep :=
        Make_Code_Type (Parameters  => Make_Parameter_List,  -- No parameters.
                        Ellipsis    => False,
                        Return_Type => Str_Subtype,
                        Inlined     => False,
                        Knr         => False);
      Result_Block      : constant Irep := Make_Code_Block (Source_Location);
      Obj_Dec           : constant Irep := Make_Code_Decl
        (Symbol          => Obj_Irep,
         Source_Location => Source_Location,
         I_Type          => Str_Subtype,
         Range_Check     => False);
   begin
      --  First add the array object for the string to the symbol table.
      New_Object_Symbol_Entry
        (Object_Name       => Intern (String_Obj),
               Object_Type       => Str_Subtype,
               Object_Init_Value => Ireps.Empty,
               A_Symbol_Table    => Global_Symbol_Table);

      --  Make the body of the function that builds the aggregate
      --  First the declaration of the aggregate array;
      Append_Op (Result_Block, Obj_Dec);

      Update_Array_From_String_Literal
        (Block        => Result_Block,
         Str_Lit      => N,
         Dest_Array   => Obj_Irep);

      --  Now add the return statement.
      Append_Op (Result_Block,
                 Make_Code_Return (Return_Value    => Obj_Irep,
                                   Source_Location => Source_Location));
      --  Create the aggregate function from the body
      --  and return a call to the function.
      declare
         Str_Func_Symbol : constant Symbol :=
           New_Function_Symbol_Entry
             (Name           => String_Func,
              Symbol_Type    => Func_Irep,
              Value          => Result_Block,
              A_Symbol_Table => Global_Symbol_Table);
         Func_Call : constant Irep :=
           Make_Side_Effect_Expr_Function_Call
             (Arguments       => Make_Argument_List,  -- Null arg list.
              I_Function      => Symbol_Expr (Str_Func_Symbol),
              Source_Location => Source_Location,
              I_Type          => Str_Subtype,
              Range_Check     => False);
      begin
         return Func_Call;
      end;
   end Do_String_Literal;

   ----------------------
   -- Do_Array_Subtype --
   ----------------------

   function Do_Array_Subtype (Subtype_Node : Node_Id;
                              The_Entity   : Entity_Id) return Irep
   is
   begin
      return (if Is_Constrained (The_Entity) then
                 Make_Constrained_Array_Subtype
                (Declaration    => Subtype_Node)
              else
                 Make_Unconstrained_Array_Subtype
                (Declaration    => Subtype_Node,
                 Component_Type => Component_Type (Etype (The_Entity))));
   end Do_Array_Subtype;

   -------------------------------------
   -- Do_Constrained_Array_Definition --
   -------------------------------------

   function Do_Constrained_Array_Definition (N     : Node_Id) return Irep is
      --  The array type declaration node is the  parent of the
      --  array_definition node.
   begin
      return
        (Make_Constrained_Array_Subtype
           (Declaration    => Parent (N)));
   end Do_Constrained_Array_Definition;

   ---------------------------------------
   -- Do_Unconstrained_Array_Definition --
   ---------------------------------------

   function Do_Unconstrained_Array_Definition (N     : Node_Id) return Irep is
      --  The array type declaration node is the  parent of the
      --  array_definition node.
     (Make_Unconstrained_Array_Subtype
        (Declaration    => Parent (N),
         Component_Type =>
           (Component_Type (Defining_Identifier (Parent (N))))));

--     function Get_Data_Component_From_Type (Array_Struct_Type : Irep)
--                                            return Irep;
--     function Get_Data_Component_From_Type (Array_Struct_Type : Irep)
--                                            return Irep
--     is
--        Struct_Component : constant Irep_List :=
--          Get_Component (Get_Components (Array_Struct_Type));
--        Last_Cursor :  constant List_Cursor :=
--          List_Next (Struct_Component,
--                     List_Next (Struct_Component,
--                       List_First (Struct_Component)));
--     begin
--        return List_Element (Struct_Component, Last_Cursor);
--     end Get_Data_Component_From_Type;

--     function Get_Data_Component (Array_Struct : Irep;
--                                  A_Symbol_Table : Symbol_Table)
--                                  return Irep;
--     function Get_Data_Component (Array_Struct : Irep;
--                                  A_Symbol_Table : Symbol_Table)
--                                  return Irep
--     is
--        Array_Struct_Type : constant Irep :=
--          Follow_Symbol_Type (Get_Type (Array_Struct), A_Symbol_Table);
--     begin
--        return Get_Data_Component_From_Type (Array_Struct_Type);
--     end Get_Data_Component;

--     function Get_Data_Member (Array_Struct : Irep;
--                               A_Symbol_Table : Symbol_Table)
--                               return Irep;
--     function Get_Data_Member (Array_Struct : Irep;
--                               A_Symbol_Table : Symbol_Table)
--                               return Irep
--     is
--        Data_Member : constant Irep :=
--          Get_Data_Component (Array_Struct, A_Symbol_Table);
--     begin
--        return Make_Member_Expr (Compound         => Array_Struct,
--                                 Source_Location  =>
--                                    Internal_Source_Location,
--                                 Component_Number => 2,
--                                 I_Type           =>
--                                   Get_Type (Data_Member),
--                                 Component_Name   =>
--                                   Get_Name (Data_Member));
--     end Get_Data_Member;

   -------------------------
   -- Do_Array_Assignment --
   -------------------------

   procedure Do_Array_Assignment (Block : Irep; N : Node_Id)
   is
--        Source_Loc : constant Irep := Get_Source_Location (N);
      --  The LHS must be a constrained array.
      LHS_Node   : constant Node_Id := Name (N);
      RHS_Node   : constant Node_Id := Expression (N);
      LHS_Kind   : constant Node_Kind := Nkind (LHS_Node);
      RHS_Kind   : constant Node_Kind := Nkind (RHS_Node);
      LHS_Type   : constant Node_Id := Underlying_Type (Etype (LHS_Node));
   begin
      if LHS_Kind /= N_Slice and
        RHS_Kind not in N_Slice | N_Aggregate | N_Op_Concat
      then
         Do_Array_Assignment_Op
           (Block       => Block,
            Destination => Do_Expression (LHS_Node),
            Dest_Type   => LHS_Type,
            Source_Expr => RHS_Node);
      else
         --  LHS should be constrained
         --  Declare a temporary array to construct the result
         declare
            LHS_I_Type         : constant Irep := Do_Type_Reference (LHS_Type);
            Temp_Array         : constant Irep :=
              Fresh_Var_Symbol_Expr (LHS_I_Type, "temp_arr_ass");
         begin
            declare
               Temp_Array_Bounds  : constant Static_And_Dynamic_Bounds :=
                 Multi_Dimension_Flat_Bounds (LHS_Node);
               Dest_Bounds        : constant Static_And_Dynamic_Bounds :=
                 Zero_Based_Bounds (LHS_Node);
            begin
               Append_Op (Block,
                          Make_Code_Decl
                            (Symbol          => Temp_Array,
                             Source_Location => Get_Source_Location (N)));
               --  Assign to the temporary array
               Do_Array_Assignment_Op
                 (Block       => Block,
                  Destination => Temp_Array,
                  Dest_Type   => LHS_Type,
                  Source_Expr => RHS_Node);

               --  Now assign the temporary array to the desination
               Assign_Array
                 (Block         => Block,
                  Destination   => Do_Expression (LHS_Node),
                  Dest_Bounds   => Dest_Bounds,
                  Source        => Temp_Array,
                  Source_Bounds => Temp_Array_Bounds);
            end;
         end;
      end if;
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

   function Do_Array_Concatination (N : Node_Id) return Irep is
      --  Eventually concatination should be handled by a goto function
      --  similar to that used for an aggregate.
      --  For now to provide error recovery just return a string literal?
      Dummy   : constant String := "unsupported";
      Recover : constant Irep :=
        Make_String_Constant_Expr
          (Source_Location => Get_Source_Location (N),
           I_Type => Make_String_Type,
           Value => Dummy);
   begin
      Report_Unhandled_Node_Empty
        (N        => N,
         Fun_Name => "Do_Array_Concatination",
         Message  => "Array concatitination operator currenly unsupported");
      return Recover;
   end Do_Array_Concatination;

   function Do_Array_First_Last_Length (N : Node_Id; Attr : Attribute_Id)
                                        return Irep
   is
      Raw_Prefix  : constant Node_Id := Prefix (N);
      The_Prefix  : constant Node_Id :=
        (if Nkind (Raw_Prefix) = N_Explicit_Dereference then
            Prefix (Raw_Prefix)
         else
            Raw_Prefix);
      Attr_Expr   : constant Node_Id := First (Expressions (N));
      Dimension   : constant Pos :=
        (if Present (Attr_Expr) then
         --  Ada rules require the dimension expression to be static.
            UI_To_Int (Intval (Attr_Expr))
         else
         --  No dimension expression defaults to dimension 1
            1);
      Bounds      : constant Dimension_Bounds :=
        Do_Array_First_Last (The_Prefix, Dimension);
   begin
      return
        (case Attr is
            when Attribute_First => Bounds.Low,
            when Attribute_Last => Bounds.High,
            when others =>
               Calculate_Dimension_Length (Bounds));
   end Do_Array_First_Last_Length;

   function Do_Array_First_Last (N : Node_Id;
                                 Dimension : Pos)
                                 return Dimension_Bounds
   is
      N_Etype    : constant Entity_Id := Etype (N);
      Array_Type : constant Entity_Id :=
        (if Is_Access_Type (N_Etype) then
              Designated_Type (N_Etype)
         else
            N_Etype);

      Dim_Index : Node_Id := First_Index (Array_Type);
   begin
      --  Get the right index for the dimension
      for I in 2 .. Dimension loop
         Dim_Index := Next_Index (Dim_Index);
      end loop;

      --  Now get the lower and upper bounds of the dimension
      declare
         Dim_Index_Type   : constant Entity_Id :=
           Etype (Get_Dimension_Index (Array_Type, Dimension));
         Index_I_Type     : constant Irep :=
           Make_Resolved_I_Type (Dim_Index_Type);
         Bounds : constant Dimension_Bounds :=
           Get_Dimension_Bounds (N, Dimension, Dim_Index);

         First  : constant Irep := Typecast_If_Necessary
           (Expr           => Bounds.Low,
            New_Type       => Index_I_Type,
            A_Symbol_Table => Global_Symbol_Table);
         Last   : constant Irep := Typecast_If_Necessary
           (Expr           => Bounds.High,
            New_Type       => Index_I_Type,
            A_Symbol_Table => Global_Symbol_Table);
      begin
         return (First, Last);
      end;
   end Do_Array_First_Last;

   -------------------------
   -- Get_Dimension_Index --
   -------------------------

   function Get_Dimension_Index (The_Array : Node_Id; Dim : Pos)
                                 return Node_Id
   is
      Dim_Iter : Node_Id := First_Index (Underlying_Type (Etype (The_Array)));
   begin
      for I in 2 .. Dim loop
         Dim_Iter := Next_Index (Dim_Iter);
      end loop;
      return Dim_Iter;
   end Get_Dimension_Index;

   ------------------------------------------------
   -- Determine_Array_Bounds_From_Initialization --
   ------------------------------------------------
   function Determine_Array_Bounds_From_Initialization
     (Init_Expr    : Node_Id) return Static_And_Dynamic_Bounds
   is
      Expr_Kind    : constant Node_Kind := Nkind (Init_Expr);
      Source_Loc   : constant Irep := Get_Source_Location (Init_Expr);

      function Obtain_Bounds return Static_And_Dynamic_Bounds;
      function Obtain_Bounds return Static_And_Dynamic_Bounds is
      begin
         if Expr_Kind = N_Op_Concat then
            --  The array is initialized by a concatination.
            --  Determine the length of the concatination
            --  The resultant array from a concatination is 1 dimensional
            declare
               Cat_Array_Length : constant Irep :=
                 Calculate_Concat_Length (Init_Expr);
            begin
               --  Goto arrays start from zero.
               return Static_And_Dynamic_Bounds'
                 (Is_Unconstrained  => False,
                  Has_Static_Bounds => False,
                  Low_Static        => 0,
                  High_Static       => 0,
                  Low_Dynamic       => Index_T_Zero,
                  High_Dynamic      => Make_Op_Sub
                    (Rhs             => Index_T_One,
                     Lhs             => Cat_Array_Length,
                     Source_Location => Source_Loc,
                     I_Type          => Index_T));
            end;
         else
            return Multi_Dimension_Flat_Bounds (Init_Expr);
         end if;
      end Obtain_Bounds;

      Bounds       : constant Static_And_Dynamic_Bounds := Obtain_Bounds;
   begin
      if Bounds.Is_Unconstrained then
         Report_Unhandled_Node_Empty
           (Init_Expr,
            "Make_Constrained_Array_From_Initialization",
            "Unsupported unconstrained array initialization by " &
              Node_Kind'Image (Nkind (Init_Expr)));
      end if;
      return Bounds;
   end Determine_Array_Bounds_From_Initialization;

   --------------
   -- Do_Slice --
   --------------

   --  A slice overlays an existing array.
   --  Do_Slice just returns the Irep of the existing, overlaid array and
   --  the restricted bounds of the slice are handled, where necessary in
   --  the subprograms which handle the use of slices.
   function Do_Slice (N : Node_Id) return Irep is
      Source_Loc : constant Irep := Get_Source_Location (N);

      Raw_Prefix  : constant Node_Id := Prefix (N);
      The_Prefix  : constant Node_Id :=
        (if Nkind (Raw_Prefix) = N_Explicit_Dereference then
            Prefix (Raw_Prefix)
         else
            Raw_Prefix);
      Prefix_Etype      : constant Node_Id := Etype (The_Prefix);
      Is_Implicit_Deref : constant Boolean := Is_Access_Type (Prefix_Etype);
      Prefix_Irep       : constant Irep := Do_Expression (Raw_Prefix);
      --  The prefix to the slice may be an access to an array object
      --  which must be implicitly dereferenced.
      --  The base array from which the slice is taken.
      Base_Array_Type   : constant Node_Id :=
        (if Is_Implicit_Deref then
            Designated_Type (Prefix_Etype)
         else
            Prefix_Etype);
      Base_Type_Irep    : constant Irep :=
        Do_Type_Reference (Base_Array_Type);
      Base_Irep         : constant Irep :=
        (if Is_Implicit_Deref then
            Make_Dereference_Expr
           (I_Type => Base_Type_Irep,
            Object => Prefix_Irep,
            Source_Location => Source_Loc)
         else
            Prefix_Irep);
   begin
      return Base_Irep;
   end Do_Slice;

   --------------------------
   -- Do_Indexed_Component --
   --------------------------

   function Do_Indexed_Component (N : Node_Id) return Irep is
      Source_Loc        : constant Irep := Get_Source_Location (N);
      Raw_Prefix  : constant Node_Id := Prefix (N);
      Pre_Prefix  : constant Node_Id :=
        (if Nkind (Raw_Prefix) = N_Explicit_Dereference then
              Prefix (Raw_Prefix)
         else
            Raw_Prefix);
      --  The prefix may be a slice.  The underlying array needs to be
      --  indexed and not the slice.
      The_Prefix        : constant Node_Id :=
        (if Nkind (Pre_Prefix) = N_Slice then
              Get_Underlying_Array_From_Slice (Pre_Prefix)
         else
            Pre_Prefix);
      Prefix_Etype      : constant Node_Id := Etype (The_Prefix);
      --  The prefix to an indexed component may be an access to an
      --  array object which must be implicitly dereferenced.
      Is_Implicit_Deref : constant Boolean := Is_Access_Type (Prefix_Etype);

      Array_Type : constant Entity_Id :=
        Underlying_Type ((if Is_Implicit_Deref then
                              Designated_Type (Prefix_Etype)
                         else
                            Prefix_Etype));

      Element_Type : constant Irep :=
        Do_Type_Reference (Component_Type (Array_Type));

      Prefix_Irep       : constant Irep := Do_Expression (The_Prefix);

      Resolved_Type     : constant Irep :=
        (if Nkind (The_Prefix) in N_Has_Entity and then
         Is_Formal (Entity (The_Prefix))
         then
         --  The array will be represented by a pointer.
            Make_Pointer_Type (Element_Type)
         else
            Do_Type_Reference (Array_Type));

      Base_Irep         : constant Irep :=
        (if Is_Implicit_Deref then
            Make_Dereference_Expr
           (I_Type => Resolved_Type,
            Object => Prefix_Irep,
            Source_Location => Get_Source_Location (N))
         else
            Prefix_Irep);

      --        Base_I_Type       : constant Irep := Get_Type (Base_Irep);
      --       pragma Assert
      --         (Kind (Base_I_Type) in I_Array_Type | I_Pointer_Type);
      --

      Index        : constant Irep := Typecast_If_Necessary
        (Expr           => Calculate_Index_Offset
           (Array_Node  => The_Prefix,
            Array_Type  => Array_Type,
            The_Indices => N),
         New_Type       => Index_T,
         A_Symbol_Table => Global_Symbol_Table);

      Indexed_Data : constant Irep :=
        Make_Resolved_Index_Expr (The_Array  => Base_Irep,
                                  Zero_Index => Index,
                                  I_Type     => Element_Type,
                                  Location   => Source_Loc);
   begin
      return Indexed_Data;
   end Do_Indexed_Component;

   -------------------------
   -- Get_Array_Reference --
   -------------------------

   function Get_Array_Reference (Array_Irep : Irep; Component_Irep : Irep)
                                 return Irep is
     (Get_Pointer_To_Array (Array_Irep, Component_Irep));

   ----------------------------------
   -- Get_Non_Array_Component_Type --
   ----------------------------------

   function Get_Non_Array_Component_Type (A : Entity_Id) return Entity_Id is
      This_Subtype : Entity_Id := Component_Type (A);
   begin
      while Is_Array_Type (This_Subtype) loop
         This_Subtype := Component_Type (This_Subtype);
      end loop;
      return This_Subtype;
   end Get_Non_Array_Component_Type;

   function Get_Underlying_Array_From_Slice (N : Node_Id) return Node_Id is
      Result : Node_Id := N;
   begin
      while Nkind (Result) = N_Slice loop
         Result := Prefix (Result);
      end loop;
      if Nkind (Result) = N_Explicit_Dereference then
         Result := Prefix (Result);
      end if;
      return Result;
   end Get_Underlying_Array_From_Slice;

   function Make_Constrained_Array_Subtype (Declaration : Node_Id)
       return Irep
   is
      Source_Location : constant Irep := Get_Source_Location (Declaration);
      Array_Entity    : constant Entity_Id :=
        (if Nkind (Declaration) = N_Defining_Identifier then
              Declaration
         else
            Defining_Identifier (Declaration));
      Comp_Type      : constant Entity_Id := Component_Type (Array_Entity);
      Comp_Irep      : constant Irep :=
        Make_Resolved_I_Type (Comp_Type);

         --  Get the array zero based bounds.
         --  If the array is multi-dimmensional, the bounds are calculated
         --  by flattening th array into a one-dimensional eaquivalent.
         --  ASVAT represents multimensional arrays as equivalent one
         --  dimensional arrays.
         --  All goto arrays are index from 0, so the length is
         --  upper bound + 1.
         Array_Bounds     : constant Static_And_Dynamic_Bounds :=
           Multi_Dimension_Flat_Bounds (Array_Entity);

         Array_Length     : constant Irep :=
           (if Array_Bounds.Has_Static_Bounds then
               Integer_Constant_To_Expr
              (Value           => UI_From_Int (Array_Bounds.High_Static + 1),
               Expr_Type       => Index_T,
               Source_Location => Source_Location)
            else
               Make_Op_Add
              (Rhs             => Index_T_One,
               Lhs             => Array_Bounds.High_Dynamic,
               Source_Location => Source_Location,
               Overflow_Check  => False,
               I_Type          => Index_T,
               Range_Check     => False));

         Array_Model_Size : constant Irep :=
           Make_Op_Mul
             (Rhs             => Typecast_If_Necessary
                (Expr           =>
                       ASVAT.Size_Model.Computed_Size (Comp_Type),
                 New_Type       => Index_T,
                 A_Symbol_Table => Global_Symbol_Table),
              Lhs             => Array_Length,
              Source_Location => Source_Location,
              Overflow_Check  => False,
              I_Type          => Index_T,
              Range_Check     => False);
   begin
      --  Set the ASVAT.Size_Model size for the array.
      ASVAT.Size_Model.Set_Computed_Size
        (Array_Entity, Array_Model_Size);
      if not Array_Bounds.Has_Static_Bounds then
         --  The array has at least one dimension which has an
         --  Ada variable specifying a bound.
         --  A variable rather than an expression must be used to define the
         --  length of the goto array.
         declare
            Array_Name   : constant String := Unique_Name (Array_Entity);
            Arr_Len      : constant String := Array_Name & "_$array_size";
            Arr_Len_Id   : constant Symbol_Id := Intern (Arr_Len);
            Arr_Len_Irep : constant Irep :=
              Make_Symbol_Expr
                (Source_Location => Source_Location,
                 I_Type          => Index_T,
                 Range_Check     => False,
                 Identifier      => Arr_Len);
         begin
            --  Add the new variable to the symbol table and set its value
            --  to the computed length.
            New_Object_Symbol_Entry
              (Object_Name       => Arr_Len_Id,
               Object_Type       => Index_T,
               Object_Init_Value => Array_Length,
               A_Symbol_Table    => Global_Symbol_Table);

            --  Return the dynamic array type
            --  using the declared array length variable.
            return Make_Array_Type
              (I_Subtype => Comp_Irep,
               Size      => Arr_Len_Irep);
         end;
      end if;
      --  Return the array type using the static
      --  length of the array.
      return Make_Array_Type
        (I_Subtype => Comp_Irep,
         Size      => Array_Length);
   end Make_Constrained_Array_Subtype;

   function Make_Unconstrained_Array_Subtype (Declaration    : Node_Id;
                                              Component_Type : Entity_Id)
                                              return Irep
   is
      Array_Type : constant Entity_Id :=
        Underlying_Type (Defining_Identifier (Declaration));
      Sub : constant Irep :=
        Make_Resolved_I_Type (Component_Type);
      Dimensions  : constant Pos := Number_Dimensions (Array_Type);

      --  An unconstrained array type is represented as an array_struc type
      Array_Struc : constant Irep := Make_Array_Struc_Type
        (Comp_Type  => Sub,
         Location   => Get_Source_Location (Declaration),
         Dimensions => Dimensions);
   begin
      --  Set the ASVAT.Size_Model size for the unconstrained array to
      --  the size of the array structure.
      ASVAT.Size_Model.Set_Static_Size
        (Array_Type, Integer (Get_Array_Struc_Type_Size (Dimensions)));
      return Array_Struc;
   end Make_Unconstrained_Array_Subtype;

   procedure Build_Unconstrained_Array_Result  (Block       : Irep;
                                                Result_Var  : Irep;
                                                Return_Expr : Node_Id)
   is
      Source_Loc   : constant Irep := Get_Source_Location (Return_Expr);
      Array_Type   : constant Entity_Id :=
        Underlying_Type (Etype (Return_Expr));
      N_Dimensions : constant Pos := Number_Dimensions (Array_Type);
      Comp_Type    : constant Entity_Id := Component_Type (Array_Type);
   begin
      Declare_Itype (Array_Type);
      --  Some of the following declarations must occur after the
      --  call to Declare_Itype as the array type may be an Itype and
      --  its details need to be recorded in the symbol table.
      --  For instance, Do_Type_Reference requires the Irep type information
      --  for the Array type to be in the global symbol table.
      declare
         Comp_I_Type  : constant Irep :=
           Make_Resolved_I_Type (Comp_Type);

         Source_I_Type : constant Irep :=
           Do_Type_Reference (Array_Type);
         Arr_Ptr_Type  : constant Irep := Make_Pointer_Type (Comp_I_Type);

         --  This array of the first and last of each dimension
         --  must have a lower bound of zero.
         Array_Dim_Bounds : Bounds_Array (0 .. (2 * N_Dimensions - 1));

         Array_Bounds     : constant Static_And_Dynamic_Bounds :=
           Multi_Dimension_Flat_Bounds (Array_Type);
         Array_Size       : constant Irep := Typecast_If_Necessary
           (Expr           => ASVAT.Size_Model.Make_Byte_Aligned_Size
              (ASVAT.Size_Model.Computed_Size (Array_Type)),
            New_Type       => Index_T,
            A_Symbol_Table => Global_Symbol_Table);

         --   Allocate an array to hold the funtion result.
         Malloc_Args  : constant Irep := Make_Argument_List;
         Malloc_Name  : constant String := "malloc";
         Malloc_Call  : constant Irep :=
           Make_Side_Effect_Expr_Function_Call
             (Arguments       => Malloc_Args,
              I_Function      => Symbol_Expr
                (Global_Symbol_Table (Intern (Malloc_Name))),
              Source_Location => Source_Loc,
              I_Type          => Make_Pointer_Type (Make_Void_Type));
         Array_Malloc : constant Irep :=
           Typecast_If_Necessary
             (Expr           => Malloc_Call,
              New_Type       => Arr_Ptr_Type,
              A_Symbol_Table => Global_Symbol_Table);

         --  A variable to point to the allocated array.
         Array_Ref    : constant Irep :=
           Fresh_Var_Symbol_Expr (Arr_Ptr_Type, "array_ref");

         --  A variable to hold the array result so that the array is
         --  only called once.
         Fun_Result   : constant Irep :=
           Fresh_Var_Symbol_Expr (Source_I_Type, "fun_result");

         Index : Node_Id := First_Index (Array_Type);
      begin
         --  Fill the bounds array.
         for I in 1 .. N_Dimensions loop
            declare
               Dim_Bounds  : constant Dimension_Bounds :=
                 Get_Dimension_Bounds (Array_Type, I, Index);
            begin
               --  Assign the first value for this dimension.
               Array_Dim_Bounds (I * 2 - 2) :=
                 Typecast_If_Necessary
                   (Expr           => Dim_Bounds.Low,
                    New_Type       => Bounds_Component,
                    A_Symbol_Table => Global_Symbol_Table);
               --  Now the last value for this dimension.
               Array_Dim_Bounds (I * 2 - 1) :=
                Typecast_If_Necessary
                   (Expr           => Dim_Bounds.High,
                    New_Type       => Bounds_Component,
                    A_Symbol_Table => Global_Symbol_Table);
            end;
            --  Do not Index beyond the number of dimensions
            if I < N_Dimensions then
               Index := Next_Index (Index);
            end if;
         end loop;

         --  Now create the malloced array to hold the result.
         Append_Argument (Malloc_Args, Array_Size);

         Append_Declare_And_Init
           (Symbol     => Array_Ref,
            Value      => Array_Malloc,
            Block      => Block,
            Source_Loc => Source_Loc);

         Append_Declare_And_Init
           (Symbol     => Fun_Result,
            Value      => Typecast_If_Necessary
              (Expr           => Do_Expression (Return_Expr),
               New_Type       => Get_Type (Fun_Result),
               A_Symbol_Table => Global_Symbol_Table),
            Block      => Block,
            Source_Loc => Source_Loc);

         --  Now copy the return expression to the allocated array.
         Copy_Array
           (Block         => Block,
            Dest_Bounds   => Array_Bounds,
            Source_Bounds => Array_Bounds,
            Dest_Irep     => Array_Ref,
            Source_Irep   => Get_Pointer_To_Array (Fun_Result, Comp_I_Type));

         --  Initialise the result Array_Struc
         Init_Array_Struc
           (Block       => Block,
            Array_Struc => Result_Var,
            Array_Ptr   => Array_Ref,
            Location    => Source_Loc,
            Bounds      => Array_Dim_Bounds);
      end;
   end Build_Unconstrained_Array_Result;

   function Make_Bounded_Array_Type (Dimensions : Pos; Comp_Type : Irep)
                                     return Irep is
     (Low_Level.Make_Array_Struc_Type
        (Comp_Type  => Comp_Type,
         Location   => Internal_Source_Location,
         Dimensions => Dimensions));

   function Make_Static_Array (Size : Pos; Array_Type : Node_Id) return Irep
   is
      Comp_I_Type : constant Irep :=
        Do_Type_Reference (Component_Type (Array_Type));
      Size_Irep   : constant Irep :=
        Integer_Constant_To_Expr
          (Value           => UI_From_Int (Size),
           Expr_Type       => Index_T,
           Source_Location => Get_Source_Location (Array_Type));
   begin
      return Make_Array_Type
        (I_Subtype => Comp_I_Type,
         Size      => Size_Irep);
   end Make_Static_Array;

   function Make_Unconstrained_Array_Result (Result_Expr : Node_Id)
                                             return Irep
   is
      Source_Loc   : constant Irep := Get_Source_Location (Result_Expr);
      Expr_Irep    : constant Irep := Do_Expression (Result_Expr);
      The_Array : constant Node_Id :=
        (if Nkind (Result_Expr) = N_Explicit_Dereference then
              Prefix (Result_Expr)
         else
            Result_Expr);
      Array_Type   : constant Entity_Id :=
        Underlying_Type (Etype (The_Array));
      N_Dimensions : constant Pos := Number_Dimensions (Array_Type);
      Comp_Type    : constant Entity_Id := Component_Type (Array_Type);
   begin
      if Is_Unconstrained_Array_Result (Expr_Irep) then
         --  If the result expression is already an unconstrained array result
         --  no processing required, just return the irep for the expression.
         return Expr_Irep;
      end if;

      Declare_Itype (Array_Type);
      --  Some of the following declarations must occur after the
      --  call to Declare_Itype as the array type may be an Itype and
      --  its details need to be recorded in the symbol table.
      --  For instance, Do_Type_Reference requires the Irep type information
      --  for the Array type to be in the global symbol table.
      declare
         Comp_I_Type      : constant Irep :=
           Make_Resolved_I_Type (Comp_Type);

         Array_I_Type     : constant Irep :=
           Do_Type_Reference (Array_Type);

         --  This array of the first and last of each dimension
         --  must have a lower bound of zero.
         Array_Dim_Bounds : Bounds_Array (0 .. (2 * N_Dimensions - 1));

         Index : Node_Id := First_Index (Array_Type);

         New_Name         : constant String :=
           Fresh_Var_Name ("array_result_");
         Array_Result_Obj : constant Irep :=
           Fresh_Var_Symbol_Expr (Array_I_Type, New_Name & "_obj");
         Array_Result_Fun : constant String := New_Name & "_fun";

         Func_Irep         : constant Irep :=
           Make_Code_Type (Parameters  => Make_Parameter_List, -- No params.
                           Ellipsis    => False,
                           Return_Type => Array_I_Type,
                           Inlined     => False,
                           Knr         => False);

         Result_Block      : constant Irep := Make_Code_Block (Source_Loc);
         Obj_Dec           : constant Irep := Make_Code_Decl
           (Symbol          => Array_Result_Obj,
            Source_Location => Source_Loc,
            I_Type          => Array_I_Type,
            Range_Check     => False);
      begin
         --  Declare the result variable
         Append_Op (Result_Block, Obj_Dec);

         --  Fill the bounds array.
         for I in 1 .. N_Dimensions loop
            declare
               Dim_Bounds  : constant Dimension_Bounds :=
                 Get_Dimension_Bounds (The_Array, I, Index);
            begin
               --  Assign the first value for this dimension.
               Array_Dim_Bounds (I * 2 - 2) :=
                 Typecast_If_Necessary
                   (Expr           => Dim_Bounds.Low,
                    New_Type       => Bounds_Component,
                    A_Symbol_Table => Global_Symbol_Table);
               --  Now the last value for this dimension.
               Array_Dim_Bounds (I * 2 - 1) :=
                Typecast_If_Necessary
                   (Expr           => Dim_Bounds.High,
                    New_Type       => Bounds_Component,
                    A_Symbol_Table => Global_Symbol_Table);
            end;
            Index := Next_Index (Index);
         end loop;

         --  Initialise the result Array_Struc
         Init_Array_Struc
           (Block       => Result_Block,
            Array_Struc => Array_Result_Obj,
            Array_Ptr   => Get_Pointer_To_Array (Expr_Irep, Comp_I_Type),
            Location    => Source_Loc,
            Bounds      => Array_Dim_Bounds);

         --  Now add the return statement.
         Append_Op (Result_Block,
                    Make_Code_Return (Return_Value    => Array_Result_Obj,
                                      Source_Location => Source_Loc));
         --  Create the array result function from the body
         --  and return a call to the function.
         declare
            Array_Result_Func_Symbol : constant Symbol :=
              New_Function_Symbol_Entry
             (Name           => Array_Result_Fun,
              Symbol_Type    => Func_Irep,
              Value          => Result_Block,
              A_Symbol_Table => Global_Symbol_Table);
            Func_Call : constant Irep :=
              Make_Side_Effect_Expr_Function_Call
                (Arguments       => Make_Argument_List,  -- Null arg list.
                 I_Function      => Symbol_Expr (Array_Result_Func_Symbol),
                 Source_Location => Source_Loc,
                 I_Type          => Array_I_Type,
                 Range_Check     => False);
         begin
            return Func_Call;
         end;
      end;
   end Make_Unconstrained_Array_Result;

   procedure Pass_Array_Friends (Actual_Array : Entity_Id;
                                 Array_Irep   : Irep;
                                 Args         : Irep) is
--        Array_Name   : constant String := Unique_Name (Actual_Array);
      Array_Type   : constant Entity_Id :=
        Underlying_Type (Etype (Actual_Array));

      Index_Iter   : Node_Id := First_Index (Array_Type);
   begin
      for Dimension in 1 .. Number_Dimensions (Array_Type) loop
         pragma Assert (Present (Index_Iter));
         declare
            Bounds : constant Dimension_Bounds :=
              (if Is_Unconstrained_Array_Result (Array_Irep) then
                    Get_Bounds_From_Struc (Array_Irep, Dimension)
               else
                  Do_Array_First_Last (Actual_Array, Dimension));
         begin
            Append_Argument (Args, Bounds.Low);
            Append_Argument (Args, Bounds.High);
         end;
         Index_Iter := Next_Index (Index_Iter);
      end loop;
   end Pass_Array_Friends;

   procedure Update_Array_From_Concatenation
           (Block       : Irep;
            Concat      : Node_Id;
            Dest_Bounds : Static_And_Dynamic_Bounds;
            Dest_Array  : Irep)
   is
      Source_Loc : constant Irep := Get_Source_Location (Concat);

      Accum_Index : Static_And_Dynamic_Index :=
        Static_And_Dynamic_Index'
          (Is_Static     => Dest_Bounds.Has_Static_Bounds,
           Static_Index  => UI_From_Int (Dest_Bounds.Low_Static),
           Dynamic_Index => Dest_Bounds.Low_Dynamic);

      procedure Process_Catenation (N : Node_Id);

      procedure Process_Catenation (N : Node_Id) is
         N_Kind : constant Node_Kind := Nkind (N);
      begin

         if N_Kind = N_Op_Concat then
            if Is_Component_Left_Opnd (N) then
               Assign_To_Array_Component
                 (Block      => Block,
                  The_Array  => Dest_Array,
                  Zero_Index => Get_Dynamic_Index (Accum_Index),
                  Value_Expr => Do_Expression (Left_Opnd (N)),
                  I_Type     => Get_Subtype (Get_Type (Dest_Array)),
                  Location   => Source_Loc);
               Add_One_To_Index (Accum_Index);
            else
               Process_Catenation
                 (N           => Left_Opnd (N));
            end if;
            if Is_Component_Right_Opnd (N) then
               Assign_To_Array_Component
                 (Block      => Block,
                  The_Array  => Dest_Array,
                  Zero_Index => Get_Dynamic_Index (Accum_Index),
                  Value_Expr => Do_Expression (Right_Opnd (N)),
                  I_Type     => Get_Subtype (Get_Type (Dest_Array)),
                  Location   => Source_Loc);
               Add_One_To_Index (Accum_Index);
            else
               Process_Catenation
                 (N           => Right_Opnd (N));
            end if;
         else
            declare
               --  In a concatenation the array can only have one dimension.
               Array_Entity : constant Node_Id :=
                 (if N_Kind in N_Entity then
                       N
                  elsif N_Kind in N_Has_Entity then
                       Entity (N)
                  elsif N_Kind in N_Has_Etype then
                       Etype (N)
                  else
                    N);
               Array_Bounds    : constant Static_And_Dynamic_Bounds :=
                 Multi_Dimension_Flat_Bounds (Array_Entity);
               Next_Length    : constant Static_And_Dynamic_Index :=
                 Get_Array_Size_From_Bounds (Array_Bounds);

               New_Index   : constant Static_And_Dynamic_Index :=
                 Add_To_Index (Accum_Index, Next_Length);

               High_Index  : constant Static_And_Dynamic_Index :=
                 Sub_One_From_Index (New_Index);

               Dest_Bounds : constant Static_And_Dynamic_Bounds :=
                 Static_And_Dynamic_Bounds'
                   (Is_Unconstrained  => False,
                    Has_Static_Bounds => Accum_Index.Is_Static,
                    Low_Static        => UI_To_Int (Accum_Index.Static_Index),
                    High_Static       => UI_To_Int (High_Index.Static_Index),
                    Low_Dynamic       => Accum_Index.Dynamic_Index,
                    High_Dynamic      => High_Index.Dynamic_Index);
            begin
               if not Array_Bounds.Is_Unconstrained then
                  Array_Assignment_Op
                    (Source_Expr  => N,
                     N_Dimensions => 1,
                     Dest_Bounds  => Dest_Bounds,
                     Target_Array => Dest_Array,
                     Block        => Block);
                  Accum_Index := New_Index;
               else
                  Report_Unhandled_Node_Empty
                    (N        => N,
                     Fun_Name => "Process_Catination",
                     Message  => "Unconstrained array expressions in " &
                       "concatinations are unsupported");
               end if;
            end;
         end if;
      end Process_Catenation;

   begin
      Process_Catenation (Concat);
   end Update_Array_From_Concatenation;

   procedure Update_Array_From_Slice
           (Block       : Irep;
            Slice       : Node_Id;
            Dest_Array  : Irep;
            Dest_Bounds : Static_And_Dynamic_Bounds)
   is
      --  Do expression of a slice returns the array from which the
      --  slice is taken.
      Underlying_Array : constant Irep := Do_Expression (Slice);

      --  Get the slice bounds which are represented as offsets from the
      --  start of the array upon which the slice is defined.
      Slice_Bounds : constant Static_And_Dynamic_Bounds :=
        Zero_Based_Bounds (Slice);
   begin
      --  A check that the source and destination arrays have the
      --  same length may be required.
      Check_Equal_Array_Lengths (Block, Slice_Bounds, Dest_Bounds);
      Copy_Array
        (Block         => Block,
         Dest_Bounds   => Dest_Bounds,
         Source_Bounds => Slice_Bounds,
         Dest_Irep     => Dest_Array,
         Source_Irep   => Underlying_Array);
   end Update_Array_From_Slice;

   procedure Update_Array_From_String_Literal
     (Block        : Irep;
      Str_Lit      : Node_Id;
      Dest_Array   : Irep)
   is
      Source_Location   : constant Irep := Get_Source_Location (Str_Lit);
      --  String literals are stored in string constants table described
      --  Stringst.
      --  Their lower bound is always 1 and therefore the string length
      --  is also the string litera['s high bound.
      Str_Id            : constant String_Id := Strval (Str_Lit);
      Str_Lit_Length     : constant Nat := String_Length (Str_Id);
      Str_Lit_Low       : constant Pos := 1;
      Component_Itype   : constant Irep :=
        Get_Subtype (Get_Type (Dest_Array));
   begin
      for I in Str_Lit_Low .. Str_Lit_Length loop
         Assign_To_Array_Component
              (Block      => Block,
               The_Array  => Dest_Array,
               Zero_Index =>
                 Integer_Constant_To_Expr
                   (Value           => UI_From_Int (I - 1),
                    Expr_Type       => Index_T,
                    Source_Location => Source_Location),
               Value_Expr => Integer_Constant_To_Expr
                 (Value           => UI_From_Int
                      (Nat (Get_String_Char (Str_Id, I))),
                  Expr_Type       => Component_Itype,
                  Source_Location => Source_Location),
               I_Type     => Component_Itype,
               Location   => Source_Location);
      end loop;
   end Update_Array_From_String_Literal;

end Arrays;
