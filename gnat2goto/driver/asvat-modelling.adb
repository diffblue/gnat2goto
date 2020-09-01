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

      Anno_Model     : constant Model_Sorts :=
          (if Present (Anno) then
              Get_Model_From_Anno (Anno)
         else
            Not_A_Model);

   begin
      return Anno_Model;
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

      --  Make a function body which is just a return statement with
      --  an and expression which is the in type condition
      Function_Body     : constant Irep :=
        Make_Code_Block (Source_Location);
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
      Resolved_Var : constant Irep :=
        Cast_Enum (Param_Irep, Global_Symbol_Table);
   begin
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
              Cast_Enum (Get_Bound (E, Resolved_Type, Bound_Low),
                         Global_Symbol_Table);

            High_Bound_Irep : constant Irep :=
              Cast_Enum (Get_Bound (E, Resolved_Type, Bound_High),
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
            Print_Modelling_Message
              ("Making in type function " & Function_Name &
                 " (" & Get_Name_String
                 (Chars (Defining_Identifier (Parameter))) &
                 " : " & Unique_Name (Param_Type) &
                 ") return Boolean)", Sloc (E));
            Print_Modelling_Message
              ("Check (" &
                 Param_Name & " >= " & Unique_Name (Param_Type)
               & "'First and " &
                 Param_Name & " <= " & Unique_Name (Param_Type) & "'Last);",
               Sloc (E));

            Append_Op (And_Conditions, Low_Bound_Condition);
            Append_Op (And_Conditions, High_Bound_Condition);
            Append_Op (Function_Body, Return_Statement);
            return Function_Body;
         end;
      else
         return Report_Unhandled_Node_Irep
           (E,
            "Make_In_Type_Function",
            Irep_Kind'Image (Kind (Followed_Type)) &
              " objects not supported");
      end if;
   end Make_In_Type_Function;

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
