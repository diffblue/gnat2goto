with Ada.Characters.Handling; use Ada.Characters.Handling;
with Namet;                   use Namet;
with Nlists;                  use Nlists;
with Einfo;                   use Einfo;
with Aspects;                 use Aspects;
with Sem_Util;                use Sem_Util;
with Follow;                  use Follow;
with Range_Check;             use Range_Check;
with GOTO_Utils;              use GOTO_Utils;
with Tree_Walk;               use Tree_Walk;

with Sinput;                  use Sinput;
with Text_IO;                 use Text_IO;

package body ASVAT.Modelling is

   function Find_Model (Model : String) return Model_Sorts;

   function Make_In_Type_Function (E : Entity_Id) return Irep;
   --  The In_Type_Function model must have a single svcalar parameter
   --  of mode in, the object to be tested, and have a Boolean return type

   function Make_Nondet_Function (E : Entity_Id) return Irep;
   --  The Nondet_Function model must be a parameterless function with
   --  a scalar result subtype.

   function Make_Unchecked_Conversion_Function (E : Entity_Id) return Irep;
   --  The Unchecked_Conversion_Function model has a single parameter s of
   --  Source_Type of mode in, performs checks and returns a Target_Type.

   function Build_In_Type_Function (N          : Node_Id;
                                    Param      : Irep) return Irep;

   --  ensures that Value is valid. If not scalar will recursivly validate
   --  components. Returns a boolean (True if valid)
   function Validate_Value (N : Node_Id;
                            Value : Irep;
                            Type_String : String) return Irep;

   ----------------
   -- Find_Model --
   ----------------

   function Find_Model (Model : String) return Model_Sorts is
      Result : Model_Sorts := Not_A_Model;
      Upper_Model : constant String := To_Upper (Model);
   begin
      for A_Model in Valid_Model loop
         if Upper_Model = Model_Sorts'Image (A_Model) then
            Result := A_Model;
            exit;
         end if;
      end loop;
      return Result;
   end Find_Model;

--     -------------------------
--     -- Get_Annotation_Name --
--     -------------------------
--
--     function Get_Annotation_Name (N : Node_Id) return String is
--       (Get_Name_String
--          (Chars (Expression
--                  (First (Pragma_Argument_Associations (N))))));

   -------------------------
   -- Get_Model_From_Anno --
   -------------------------

   function Get_Model_From_Anno (N : Node_Id) return Model_Sorts is
      Anno_Value : constant Node_Id := Expression (N);
      Is_Aggregate : constant Boolean :=
        Nkind (Anno_Value) = N_Aggregate;
   begin
      --  The value of an ASVAT annotation must be a non empty aggregate
      if Is_Aggregate and then Present (Expressions (Anno_Value))
      then
         declare
            First_Arg  : constant Node_Id :=
              First (Expressions (Anno_Value));
            Second_Arg : constant Node_Id :=
              (if Present (First_Arg) then
                    Next (First_Arg)
               else First_Arg);
            Is_ASVAT : constant Boolean :=
              Nkind (First_Arg) = N_Identifier and then
              Get_Name_String (Chars (First_Arg)) = "asvat";
            Has_Model : constant Boolean :=
              (Is_ASVAT and Present (Second_Arg)) and then
              Nkind (Second_Arg) = N_Identifier;
         begin
            if Is_ASVAT then
               if Present (Second_Arg) and then
                 Nkind (Second_Arg) /= N_Identifier
               then
                  Report_Unhandled_Node_Empty
                    (N,
                     "Is_ASVAT_Annotation",
                     "ASVAT model must be an identifier");
               elsif not Present (Second_Arg) then
                  Report_Unhandled_Node_Empty
                    (N,
                     "Is_ASVAT_Annotation",
                     "ASVAT anotation must supply a model");
               end if;
            end if;
            return (if Is_ASVAT and Has_Model then
                       Find_Model (Get_Name_String (Chars (Second_Arg)))
                    else
                       Not_A_Model);
         end;
      else
         if Nkind (Anno_Value) = N_Identifier then
            if Get_Name_String (Chars (Anno_Value)) = "asvat" then
               Report_Unhandled_Node_Empty
                 (N,
                  "Is_ASVAT_Annotation",
                  "ASVAT annotation must have a model");
            end if;
         end if;
         return Not_A_Model;
      end if;
   end Get_Model_From_Anno;

   --------------------
   -- Get_Model_Sort --
   --------------------

   function Get_Model_Sort (E : Entity_Id) return Model_Sorts is
      Anno           : constant Node_Id := Find_Aspect (E, Aspect_Annotate);

   begin
      if Present (Anno) then
         return Get_Model_From_Anno (Anno);
      elsif Is_Generic_Instance (E) and then
        Get_Name_String
          (Chars (Next_Entity (E))) =
        "unchecked_conversion"
      then
         return Unchecked_Conversion;
      else
         return Not_A_Model;
      end if;
   end Get_Model_Sort;

   --------------------------
   -- Make_In_Type_Function --
   --------------------------

   function Make_In_Type_Function (E : Entity_Id) return Irep is
      Source_Location : constant Irep := Get_Source_Location (E);
      Function_Name   : constant String := Unique_Name (E);
      Function_Id     : constant Symbol_Id := Intern (Function_Name);
      Parameter       : constant Node_Id :=
        First (Parameter_Specifications (Declaration_Node (E)));
      Param_Entity    : constant Node_Id := Defining_Identifier (Parameter);

      pragma Assert
        ((Ekind (E) = E_Function and
           Present (Parameter)) and then
           (not (Out_Present (Parameter) or Present (Next (Parameter))) and
                Is_Scalar_Type (Etype (Param_Entity)) and
              Is_Boolean_Type (Etype (E))),
         "In_Type_Function model must have a single scalar " &
           "mode in parameter and a Boolean return subtype");

      --  The subprogram is declared from the source text.
      pragma Assert (Global_Symbol_Table.Contains (Function_Id));

      Param_Name : constant String :=
        Unique_Name (Defining_Identifier (Parameter));
      Param_Type        : constant Node_Id := Etype (Param_Entity);
      Followed_Type     : constant Irep :=
        Follow_Symbol_Type (Do_Type_Reference (Param_Type),
                            Global_Symbol_Table);
      Param_Irep        : constant Irep :=
        Make_Symbol_Expr
          (Source_Location => Source_Location,
           I_Type          => Followed_Type,
           Range_Check     => False,
           Identifier      => Param_Name);
   begin
      return Build_In_Type_Function
        (E, Param_Irep);
   end Make_In_Type_Function;
--     function Make_In_Type_Function (E : Entity_Id) return Irep is
--        Source_Location : constant Irep := Get_Source_Location (E);
--        Function_Name   : constant String := Unique_Name (E);
--        Function_Id     : constant Symbol_Id := Intern (Function_Name);
--        Parameter       : constant Node_Id :=
--          First (Parameter_Specifications (Declaration_Node (E)));
--     Param_Entity    : constant Node_Id := Defining_Identifier (Parameter);
--
--        pragma Assert
--          ((Ekind (E) = E_Function and
--             Present (Parameter)) and then
--             (not (Out_Present (Parameter) or Present (Next (Parameter))) and
--                  Is_Scalar_Type (Etype (Param_Entity)) and
--                Is_Boolean_Type (Etype (E))),
--           "In_Type_Function model must have a single scalar " &
--             "mode in parameter and a Boolean return subtype");
--
--        --  The subprogram is declared from the source text.
--        pragma Assert (Global_Symbol_Table.Contains (Function_Id));
--
--        --  Make a function body which is just a return statement with
--        --  an and expression which is the in type condition
--        Function_Body     : constant Irep :=
--          Make_Code_Block (Source_Location);
--        Param_Name : constant String :=
--          Unique_Name (Defining_Identifier (Parameter));
--        Param_Type        : constant Node_Id := Etype (Param_Entity);
--        Followed_Type     : constant Irep :=
--          Follow_Symbol_Type (Do_Type_Reference (Param_Type),
--                              Global_Symbol_Table);
--        Param_Irep        : constant Irep :=
--          Make_Symbol_Expr
--            (Source_Location => Source_Location,
--             I_Type          => Followed_Type,
--             Range_Check     => False,
--             Identifier      => Param_Name);
--        Resolved_Var : constant Irep :=
--          Cast_Enum (Param_Irep, Global_Symbol_Table);
--     begin
--        if Kind (Followed_Type) in
--          I_Bounded_Unsignedbv_Type | I_Bounded_Signedbv_Type
--            | I_Bounded_Floatbv_Type | I_Unsignedbv_Type | I_Signedbv_Type
--              | I_Floatbv_Type | I_C_Enum_Type
--        then
--           declare
--              Resolved_Type : constant Irep :=
--                (if Kind (Followed_Type) = I_C_Enum_Type then
--                      Get_Subtype (Followed_Type)
--                 else
--                    Followed_Type);
--
--              Low_Bound_Irep : constant Irep :=
--                Cast_Enum (Get_Bound (E, Resolved_Type, Bound_Low),
--                           Global_Symbol_Table);
--
--              High_Bound_Irep : constant Irep :=
--                Cast_Enum (Get_Bound (E, Resolved_Type, Bound_High),
--                           Global_Symbol_Table);
--
--              Low_Bound_Condition : constant Irep :=
--                Make_Op_Geq
--                  (Rhs =>
--                     Typecast_If_Necessary
--                       (Low_Bound_Irep,
--                        Get_Type (Resolved_Var),
--                        Global_Symbol_Table),
--                   Lhs             => Resolved_Var,
--                   Source_Location => Source_Location,
--                   Overflow_Check  => False,
--                   I_Type          => Make_Bool_Type,
--                   Range_Check     => False);
--
--              High_Bound_Condition : constant Irep :=
--                Make_Op_Leq
--                  (Rhs             =>
--                     Typecast_If_Necessary
--                       (High_Bound_Irep,
--                        Get_Type (Resolved_Var),
--                        Global_Symbol_Table),
--                   Lhs             => Resolved_Var,
--                   Source_Location => Source_Location,
--                   Overflow_Check  => False,
--                   I_Type          => Make_Bool_Type,
--                   Range_Check     => False);
--
--              And_Conditions : constant Irep :=
--                Make_Op_And
--                  (Source_Location => Source_Location,
--                   I_Type          => Make_Bool_Type,
--                   Range_Check     => False);
--
--              Return_Statement : constant Irep :=
--                Make_Code_Return
--                  (Return_Value    => And_Conditions,
--                   Source_Location => Source_Location,
--                   I_Type          => Make_Bool_Type,
--                   Range_Check     => False);
--           begin
--              Print_Modelling_Message
--                ("Making in type function " & Function_Name &
--                   " (" & Get_Name_String
--                   (Chars (Defining_Identifier (Parameter))) &
--                   " : " & Unique_Name (Param_Type) &
--                   ") return Boolean)", Sloc (E));
--              Print_Modelling_Message
--                ("Check (" &
--                   Param_Name & " >= " & Unique_Name (Param_Type)
--                 & "'First and " &
--                Param_Name & " <= " & Unique_Name (Param_Type) & "'Last);",
--                 Sloc (E));
--
--              Append_Op (And_Conditions, Low_Bound_Condition);
--              Append_Op (And_Conditions, High_Bound_Condition);
--              Append_Op (Function_Body, Return_Statement);
--              return Function_Body;
--           end;
--        else
--           return Report_Unhandled_Node_Irep
--             (E,
--              "Make_In_Type_Function",
--              Irep_Kind'Image (Kind (Followed_Type)) &
--                " objects not supported");
--        end if;
--     end Make_In_Type_Function;

   ----------------
   -- Make_Model --
   ----------------

   procedure Make_Model (E : Entity_Id; Model : Model_Sorts) is
      Subprog_Id      : constant Symbol_Id := Intern (Unique_Name (E));

      --  The subprogram must be in the symbol table as it has been declared
      --  in the subprogram text.
      --  Make an appropriate body for the model subprogram.
      Subprog_Body : constant Irep :=
        (case Model is
            when Nondet_Function  => Make_Nondet_Function (E),
            when In_Type_Function => Make_In_Type_Function (E),
            when Unchecked_Conversion =>
               Make_Unchecked_Conversion_Function (E),
            when others =>
               Report_Unhandled_Node_Irep
              (N        => E,
               Fun_Name => "Make_Model",
               Message  => "ASVAT model " & Model_Sorts'Image (Model) &
                 " is currently unsupported"));

      Subprog_Sym : Symbol := Global_Symbol_Table (Subprog_Id);
   begin
      Subprog_Sym.Value := Subprog_Body;
      --  The model body is now made the body of the subprogram.
      Global_Symbol_Table.Replace (Subprog_Id, Subprog_Sym);
   end Make_Model;

   --------------------------
   -- Make_Nondet_Function --
   --------------------------

   function Make_Nondet_Function (E : Entity_Id) return Irep is
      Source_Location : constant Irep := Get_Source_Location (E);
      Function_Name   : constant String := Unique_Name (E);
      Function_Id      : constant Symbol_Id := Intern (Function_Name);

      pragma Assert (Ekind (E) = E_Function and
                       (not Present
                         (First
                            (Parameter_Specifications
                               (Declaration_Node (E))))
                     and then Is_Scalar_Type (Etype (E))),
                     "Nondet_Function model must be a parameterless " &
                       "function with a scalar return subtype");
      --  The subprogram is declared from the source text.
      pragma Assert (Global_Symbol_Table.Contains (Function_Id));

      --  Make a function body which declares an unitialised variable
      --  (and therefore nondet) of the function return type.
      Function_Symbol   : constant Symbol := Global_Symbol_Table (Function_Id);
      Function_Body     : constant Irep := Make_Code_Block (Source_Location);
      Return_Type_Irep  : constant Irep :=
        Get_Return_Type (Function_Symbol.SymType);
      Uninitialised_Var : constant String := "result___" & Function_Name;
      Var_Id            : constant Symbol_Id := Intern (Uninitialised_Var);
      Var_Expr          : constant Irep := Make_Symbol_Expr
        (Source_Location => Source_Location,
         Identifier      => Uninitialised_Var,
         I_Type          => Return_Type_Irep);
      Declaration       : constant Irep := Make_Code_Decl
        (Symbol          => Var_Expr,
         Source_Location => Source_Location);
      Return_Statement  : constant Irep := Make_Code_Return
        (Return_Value    => Var_Expr,
         Source_Location => Source_Location);
   begin
      Print_Modelling_Message
        ("Making nondet function " & Function_Name &
           " : " & Unique_Name (Etype (E)), Sloc (E));

      Append_Op (Function_Body, Declaration);

      --  Enter the uninitialised variable into the symbol table.
      pragma Assert (not Global_Symbol_Table.Contains (Var_Id),
                     "Symbol table already contains " & Uninitialised_Var);
      New_Object_Symbol_Entry (Object_Name       => Var_Id,
                               Object_Type       => Return_Type_Irep,
                               Object_Init_Value => Ireps.Empty,
                               A_Symbol_Table    => Global_Symbol_Table);

      --  Make the function body
      Append_Op (Function_Body, Return_Statement);

      return Function_Body;

   end Make_Nondet_Function;

   ----------------------------------------
   -- Make_Unchecked_Conversion_Function --
   ----------------------------------------

   function Make_Unchecked_Conversion_Function (E : Entity_Id) return Irep
   is
      function Make_Unchecked (Expr : Irep; Check : String) return Irep;
      function Get_Type_Size (N : Node_Id) return Irep;

      Source_Location : constant Irep := Get_Source_Location (E);
      Function_Name   : constant String := Unique_Name (E);
      --  Make a function body which takes a source and copies it to
      --  a return value of Target_Type.
      Function_Body     : constant Irep := Make_Code_Block (Source_Location);
      Target_Type_Node  : constant Node_Id := Etype (Etype (Etype (E)));
      Return_Type_Irep  : constant Irep :=
        Follow_Symbol_Type (Do_Type_Reference (Target_Type_Node),
                            Global_Symbol_Table);

      Target_Var : constant String := "target___" & Function_Name;
      Destination_Type  : constant Irep :=
        Make_Pointer_Type (Return_Type_Irep);

      Destination : constant Irep := Make_Symbol_Expr
        (Source_Location => Source_Location,
         Identifier => Target_Var,
         I_Type => Destination_Type);

      Return_Var : constant Irep := Make_Symbol_Expr
        (Source_Location => Source_Location,
         Identifier => Target_Var,
         I_Type => Return_Type_Irep);

      Decl_Statement  : constant Irep := Make_Code_Decl
        (Symbol          => Return_Var,
         Source_Location => Source_Location);

      Return_Statement  : constant Irep := Make_Code_Return
        (Return_Value    => Return_Var,
         Source_Location => Source_Location);

      Source_Var : constant String := Unique_Name (First_Entity (E));
      Source_Type_Node  : constant Node_Id := Etype (Etype (First_Entity (E)));

      Source_Type_Irep  : constant Irep :=
        Follow_Symbol_Type (Do_Type_Reference (Source_Type_Node),
                            Global_Symbol_Table);

      Source_Type  : constant Irep :=
        Make_Pointer_Type (Source_Type_Irep);

      Source : constant Irep := Make_Symbol_Expr
        (Source_Location => Source_Location,
         Identifier => Source_Var,
         I_Type => Source_Type);

      Target_Size_String : constant String := "target_size___" & Function_Name;
      Target_Size_Sym : constant Irep := Make_Symbol_Expr
        (Source_Location => Source_Location,
         Identifier => Target_Size_String,
         I_Type => CProver_Size_T);

      Target_Size_Decl  : constant Irep := Make_Code_Decl
        (Symbol          => Target_Size_Sym,
         Source_Location => Source_Location);

      Target_Size_Statement  : Irep;

      Source_Size_String : constant String := "source_size___" & Function_Name;
      Source_Size_Sym : constant Irep := Make_Symbol_Expr
        (Source_Location => Source_Location,
         Identifier => Source_Size_String,
         I_Type => CProver_Size_T);

      Source_Size_Decl  : constant Irep := Make_Code_Decl
        (Symbol          => Source_Size_Sym,
         Source_Location => Source_Location);

      Source_Size_Statement  : Irep;

      Target_Size : Irep;
      Source_Size : Irep;

      Element_Size : constant Uint := Uint_8;

      Mem_Copy : Irep;

      function Make_Unchecked (Expr : Irep; Check : String) return Irep
      is
         Args : constant Irep := Make_Argument_List;
         Expr_As_Int : constant Irep :=
           Typecast_If_Necessary (Expr           => Expr,
                                  New_Type       => Int32_T,
                                  A_Symbol_Table => Global_Symbol_Table);
         Sym_Expr : constant Irep :=
           Symbol_Expr (Get_Ada_Check_Symbol
                        ("__CPROVER_Ada_Unchecked_Conversion_" & Check,
                        Global_Symbol_Table, Internal_Source_Location));
         Function_Call : constant Irep :=
           Make_Code_Function_Call (Arguments       => Args,
                                    I_Function      => Sym_Expr,
                                    Lhs             =>
                                      Make_Nil (Internal_Source_Location),
                                    Source_Location =>
                                      Internal_Source_Location,
                                    I_Type          => Make_Void_Type);
      begin
         Append_Argument (Args, Expr_As_Int);
         return Function_Call;
      end Make_Unchecked;

--        function Get_Type_Size (N : Node_Id) return Irep
--        is
--        begin
--           if Has_Size (N) then
--              return
--                Typecast_If_Necessary (Computed_Size (N),
--                                       CProver_Size_T, Global_Symbol_Table);
--           elsif Has_Static_Size (N) then
--              return
--                Typecast_If_Necessary (Integer_Constant_To_Expr
--                                       (Value           =>
--                                            UI_From_Int
--        (Int (Static_Size (N))),
--                                        Expr_Type       => Int32_T,
--                                        Source_Location =>
--                                          Internal_Source_Location),
--                                       CProver_Size_T, Global_Symbol_Table);
--           else
--              Report_Unhandled_Node_Empty
--                (N, "Get_Type_Size",
--                 "Type has no valid size");
--              return Typecast_If_Necessary
--                (Get_Int32_T_Zero,
--                 CProver_Size_T, Global_Symbol_Table);
--           end if;
--        end Get_Type_Size;
      function Get_Type_Size (N : Node_Id) return Irep
      is
      begin
         if RM_Size (N) /= 0 then
            return
              Typecast_If_Necessary (Integer_Constant_To_Expr
                                     (Value           => RM_Size (N),
                                      Expr_Type       => Int32_T,
                                      Source_Location =>
                                        Internal_Source_Location),
                                     CProver_Size_T, Global_Symbol_Table);
         elsif Esize (N) /= 0 then
            return
              Typecast_If_Necessary (Integer_Constant_To_Expr
                                     (Value           => Esize (N),
                                      Expr_Type       => Int32_T,
                                      Source_Location =>
                                        Internal_Source_Location),
                                     CProver_Size_T, Global_Symbol_Table);
         else
            Report_Unhandled_Node_Empty
              (N, "Get_Type_Size",
               "Type has no valid size");
            return Typecast_If_Necessary
              (Get_Int32_T_Zero,
               CProver_Size_T, Global_Symbol_Table);
         end if;
      end Get_Type_Size;

   begin

      Append_Op (Function_Body, Target_Size_Decl);
      Append_Op (Function_Body, Source_Size_Decl);
      Append_Op (Function_Body, Decl_Statement);

      --  get target and source sizes
      Target_Size := Get_Type_Size (Target_Type_Node);
      Print_Irep (Get_Op0 (Target_Size));
      Source_Size := Get_Type_Size (Source_Type_Node);
      Print_Irep (Get_Op0 (Source_Size));

      Target_Size_Statement := Make_Code_Assign
        (Rhs             => Get_Type_Size (Target_Type_Node),
         Lhs             => Target_Size_Sym,
         Source_Location => Source_Location,
         I_Type          => CProver_Size_T);

      Append_Op (Function_Body, Target_Size_Statement);

      Source_Size_Statement := Make_Code_Assign
        (Rhs             => Get_Type_Size (Source_Type_Node),
         Lhs             => Source_Size_Sym,
         Source_Location => Source_Location,
         I_Type          => CProver_Size_T);

      Append_Op (Function_Body, Source_Size_Statement);

      --  check sizes are compatible
      --  report CPROVER_Ada_Unchecked_Conversion_Size if not
      Append_Op (Function_Body, Make_Unchecked
                 (Make_Op_Eq (Rhs             => Target_Size_Sym,
                              Lhs             => Source_Size_Sym,
                              Source_Location => Get_Source_Location (E),
                              I_Type          => CProver_Size_T), "Size"));

      --  convert target size to bytes
      Append_Op (Function_Body, Make_Code_Assign
                 (Rhs             => Make_Op_Div
                  (Rhs            => Typecast_If_Necessary
                   (Integer_Constant_To_Expr
                      (Value           => Element_Size,
                       Expr_Type       => Int32_T,
                       Source_Location => Source_Location),
                      CProver_Size_T, Global_Symbol_Table),
                   Lhs               => Target_Size_Sym,
                   Div_By_Zero_Check => False,
                   Source_Location   => Source_Location,
                   I_Type            => CProver_Size_T),
                  Lhs             => Target_Size_Sym,
                  Source_Location => Source_Location,
                  I_Type          => CProver_Size_T));

      --  do a mem copy from source to target
      Mem_Copy :=
        Make_Memcpy_Function_Call_Expr
          (Destination       => Destination,
           Source            => Source,
           Num_Elem          => Target_Size_Sym,
           Element_Type_Size => Element_Size,
           Source_Loc        => Get_Source_Location (E));

      Append_Op (Function_Body,
                 Make_Code_Assign
                   (Rhs             => Mem_Copy,
                    Lhs             => Destination,
                    Source_Location => Get_Source_Location (E)));

      --  check validity of resulting target
      --  report CPROVER_Ada_Unchecked_Conversion_Valid if not vaid
      Append_Op (Function_Body, Make_Unchecked
                 (Validate_Value (E, Destination,
                    Unique_Name (Target_Type_Node)), "Valid"));

      Append_Op (Function_Body, Return_Statement);

      Print_Irep (Function_Body);
      Print_Irep (Mem_Copy);
      Print_Irep (Get_Arguments (Mem_Copy));
      Print_Irep (Return_Statement);

      return Function_Body;
   end Make_Unchecked_Conversion_Function;

   -------------------------
   -- Make_Valid_Function --
   -------------------------

   function Make_Valid_Function (N : Node_Id;
                                 Value : Irep;
                                 Type_Name : String) return Irep is
      Source_Loc : constant Irep := Get_Source_Location (N);

      Value_Type : constant Irep := Get_Type (Value);

      function Make_Valid return Symbol;

      function Make_Valid return Symbol
      is
         Func_Name : constant String :=
         "__CPROVER_valid_" & Type_Name;
         Body_Block : constant Irep := Make_Code_Block (Source_Loc);
         Func_Params : constant Irep := Make_Parameter_List;
         Value_Arg : constant Irep :=
           Create_Fun_Parameter (Fun_Name        => Func_Name,
                                 Param_Name      => "value",
                                 Param_Type      => Value_Type,
                                 Param_List      => Func_Params,
                                 A_Symbol_Table  => Global_Symbol_Table,
                                 Source_Location => Source_Loc);
         Func_Type : constant Irep :=
           Make_Code_Type (Parameters  => Func_Params,
                           Ellipsis    => False,
                           Return_Type => CProver_Bool_T,
                           Inlined     => False,
                           Knr         => False);
         Value_Param : constant Irep := Param_Symbol (Value_Arg);
      begin
         Append_Op (Body_Block, Build_In_Type_Function
                    (N, Value_Param));

         return New_Function_Symbol_Entry
           (Name        => Func_Name,
            Symbol_Type => Func_Type,
            Value       => Body_Block,
            A_Symbol_Table => Global_Symbol_Table);
      end Make_Valid;

      Call_Args : constant Irep := Make_Argument_List;
   begin
      Append_Argument (Call_Args, Value);

      return Make_Side_Effect_Expr_Function_Call
        (Arguments       => Call_Args,
         I_Function      => Symbol_Expr (Make_Valid),
         Source_Location => Source_Loc,
         I_Type          => CProver_Bool_T);

   end Make_Valid_Function;

   ----------------------------
   -- Build_In_Type_Function --
   ----------------------------

   function Build_In_Type_Function (N          : Node_Id;
                                    Param      : Irep) return Irep is
      Source_Location : constant Irep := Get_Source_Location (N);

      --  Make a function body which is just a return statement with
      --  an and expression which is the in type condition
      Function_Body     : constant Irep :=
        Make_Code_Block (Source_Location);
      Followed_Type     : Irep :=
        Follow_Symbol_Type (Get_Type (Param),
                            Global_Symbol_Table);
      Resolved_Var : constant Irep :=
        Cast_Enum (Param, Global_Symbol_Table);
   begin
      if Kind (Followed_Type) = I_Pointer_Type then
         Followed_Type := Follow_Symbol_Type (Get_Subtype (Followed_Type),
                                              Global_Symbol_Table);
      end if;
      if Kind (Followed_Type) in
        I_Bounded_Unsignedbv_Type | I_Bounded_Signedbv_Type
          | I_Bounded_Floatbv_Type | I_Unsignedbv_Type | I_Signedbv_Type
            | I_Floatbv_Type | I_C_Enum_Type
      then
         declare
            Resolved_Type : constant Irep :=
              (if Kind (Followed_Type) = I_C_Enum_Type then
                    Get_Subtype (Followed_Type)
               else
                  Followed_Type);

            Low_Bound_Irep : constant Irep :=
              Cast_Enum (Get_Bound (N, Resolved_Type, Bound_Low),
                         Global_Symbol_Table);

            High_Bound_Irep : constant Irep :=
              Cast_Enum (Get_Bound (N, Resolved_Type, Bound_High),
                         Global_Symbol_Table);

            Low_Bound_Condition : constant Irep :=
              Make_Op_Geq
                (Rhs =>
                   Typecast_If_Necessary
                     (Low_Bound_Irep,
                      Get_Type (Resolved_Var),
                      Global_Symbol_Table),
                 Lhs             => Resolved_Var,
                 Source_Location => Source_Location,
                 Overflow_Check  => False,
                 I_Type          => Make_Bool_Type,
                 Range_Check     => False);

            High_Bound_Condition : constant Irep :=
              Make_Op_Leq
                (Rhs             =>
                   Typecast_If_Necessary
                     (High_Bound_Irep,
                      Get_Type (Resolved_Var),
                      Global_Symbol_Table),
                 Lhs             => Resolved_Var,
                 Source_Location => Source_Location,
                 Overflow_Check  => False,
                 I_Type          => Make_Bool_Type,
                 Range_Check     => False);

            And_Conditions : constant Irep :=
              Make_Op_And
                (Source_Location => Source_Location,
                 I_Type          => Make_Bool_Type,
                 Range_Check     => False);

            Return_Statement : constant Irep :=
              Make_Code_Return
                (Return_Value    => And_Conditions,
                 Source_Location => Source_Location,
                 I_Type          => Make_Bool_Type,
                 Range_Check     => False);
         begin
            Append_Op (And_Conditions, Low_Bound_Condition);
            Append_Op (And_Conditions, High_Bound_Condition);
            Append_Op (Function_Body, Return_Statement);
            return Function_Body;
         end;
      else
         return Report_Unhandled_Node_Irep
           (N,
            "Build_In_Type_Function",
            Irep_Kind'Image (Kind (Followed_Type)) &
              " objects not supported");
      end if;
   end Build_In_Type_Function;

   --------------------
   -- Validate_Value --
   --------------------
   function Validate_Value (N : Node_Id;
                            Value : Irep;
                            Type_String : String) return Irep is
      Value_Type : constant Irep :=
        (if Kind (Get_Type (Value)) = I_Pointer_Type then
            Get_Subtype (Get_Type (Value))
         else
            Get_Type (Value));
      Followed_Type : constant Irep := Follow_Symbol_Type
        (Value_Type,
         Global_Symbol_Table);

      --  TODO
      --  need to work out how to handle pointers either in this operation
      --  or in calling operation
   begin
      if Kind (Followed_Type) in
        I_Bounded_Unsignedbv_Type | I_Bounded_Signedbv_Type
          | I_Bounded_Floatbv_Type | I_Unsignedbv_Type | I_Signedbv_Type
            | I_Floatbv_Type | I_C_Enum_Type
      then
         Print_Irep (Value_Type);
         return Make_Valid_Function (N, Value, Type_String);
      elsif Kind (Followed_Type) = I_Struct_Type then
         declare
            Comp_List : Irep_List;
            Current_Element : List_Cursor;
            Record_Block : constant Irep :=
              Make_Code_Block (Get_Source_Location (N));
         begin
            Print_Irep (Followed_Type);
            Print_Irep (Get_Components (Followed_Type));
            Comp_List := Get_Component (Get_Components (Followed_Type));
            Current_Element := List_First (Comp_List);
            loop
               Append_Op (Record_Block,
                          Validate_Value (N, List_Element (Comp_List,
                            Current_Element),
                            Type_String));
               Current_Element := List_Next (Comp_List, Current_Element);
               exit when not List_Has_Element (Comp_List, Current_Element);
            end loop;
            return Record_Block;
         end;
      else
         Print_Irep (Value_Type);
         Report_Unhandled_Node_Empty
           (N,
            "Validate_Value",
            Irep_Kind'Image (Kind (Followed_Type)) &
              " objects not supported");
         return CProver_False;
      end if;
   end Validate_Value;

   -----------------------------
   -- Print_Modelling_Message --
   -----------------------------

   procedure Print_Modelling_Message (Mess : String; Loc : Source_Ptr) is
   begin
      if Print_Message then
         Put_Line (Build_Location_String (Loc) & " ASVAT modelling: ");
         Put_Line (Mess);
      end if;
   end Print_Modelling_Message;

end ASVAT.Modelling;
