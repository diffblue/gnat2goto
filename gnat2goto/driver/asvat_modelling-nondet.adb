with Namet;             use Namet;
with Nlists;            use Nlists;
with Uintp;             use Uintp;
with Einfo;             use Einfo;
with Sem_Eval;          use Sem_Eval;
with GOTO_Utils;        use GOTO_Utils;
with Symbol_Table_Info; use Symbol_Table_Info;
with Tree_Walk;         use Tree_Walk;
with Follow;            use Follow;
with Range_Check;       use Range_Check;
with Arrays;            use Arrays;
with Text_IO;           use Text_IO;

package body ASVAT_Modelling.Nondet is

   procedure Append_Nondet_Recurse (Var_Name : String;
                                    Var_Type : Node_Id;
                                    Block    : Irep;
                                    Var_Irep : Irep;
                                    Location : Irep;
                                    Model    : Model_Sorts;
                                    Base_Entity : Node_Id)
   with Import;

   -----------------------
   -- Append_Nondet_Var --
   -----------------------

   procedure Append_Nondet_Var (Var_Name : String;
                                Var_Type : Node_Id;
                                Block    : Irep;
                                Model    : Model_Sorts;
                                E        : Entity_Id)
   is
      Source_Location : constant Irep := Get_Source_Location (E);
      Var_Symbol_Id : constant Symbol_Id := Intern (Var_Name);
      pragma Assert
        (Global_Symbol_Table.Contains (Var_Symbol_Id),
         "Append_Nondet_Var: Variable name is not in symbol table");
      Var_Symbol : constant Symbol := Global_Symbol_Table (Var_Symbol_Id);
      Given_Type : constant Irep := Var_Symbol.SymType;

      Sym : constant Irep :=
        Make_Symbol_Expr
          (Source_Location => Source_Location,
           Identifier      => Var_Name,
           I_Type          => Given_Type);

      Var_Irep : constant Irep :=
        (if Kind (Given_Type) = I_Pointer_Type then
              Make_Dereference_Expr
           (Object          => Sym,
            Source_Location => Source_Location,
            I_Type          => Get_Subtype (Given_Type),
            Range_Check     => False)
         else
            Sym);
   begin
      --  The variable may be strutured so the structure must be traversed
      --  and every scalar component must be set to nondet and optionally
      --  assumed to be in type.  This is done by recursively traversing the
      --  structure.
      Append_Nondet_Recurse
        (Var_Name    => Var_Name,
         Var_Type    => Var_Type,
         Block       => Block,
         Var_Irep    => Var_Irep,
         Location    => Source_Location,
         Model       => Model,
         Base_Entity => E);
   end Append_Nondet_Var;

   -------------------
   -- Do_Nondet_Var --
   -------------------

   function Do_Nondet_Var (Var_Name, Var_Type : String;
                           E : Entity_Id) return Irep
   is
      Source_Location : constant Irep := Get_Source_Location (E);
      Var_Symbol_Id : constant Symbol_Id := Intern (Var_Name);
      pragma Assert
        (Global_Symbol_Table.Contains (Var_Symbol_Id),
         "Append_Nondet_Var: Variable name is not in symbol table");
      Var_Symbol : constant Symbol := Global_Symbol_Table (Var_Symbol_Id);
      Fun_Name : constant String := "nondet___" & Var_Type;
      Fun_Symbol_Id : constant Symbol_Id := Intern (Fun_Name);
   begin
      Put_Line ("Nondetting " & Var_Name);
      Print_Irep (Var_Symbol.SymType);

      --  First a nondet function is required to assign to the variable.
      --  One for the type may already exist.
      if not Global_Symbol_Table.Contains (Fun_Symbol_Id) then
         Make_Nondet_Function (Fun_Name    => Fun_Name,
                               Result_Type => Var_Type,
                               Statements  => Ireps.Empty,
                               E           => E);
      end if;

      --  Now the nondet function is declared, the LHS and RHS of the
      --  assignment can be declared.
      declare
         --  If the variable is a formal out parameter it wll be a pointer
         --  and it needs to be dereferenced.
         Given_Type : constant Irep := Var_Symbol.SymType;

         Sym : constant Irep :=
           Make_Symbol_Expr
             (Source_Location => Source_Location,
              Identifier      => Var_Name,
              I_Type          => Given_Type);

         LHS : constant Irep :=
           (if Kind (Given_Type) = I_Pointer_Type then
                 Make_Dereference_Expr
              (Object          => Sym,
               Source_Location => Source_Location,
               I_Type          => Get_Subtype (Given_Type),
               Range_Check     => False)
            else
               Sym);

         RHS : constant Irep :=
           Do_Parameterless_Function_Call
             (Fun_Name => Fun_Name,
              E        => E);
      begin
         Print_Modelling_Message ("Assign " &
                                    Var_Name & " := " & Fun_Name,
                                  Sloc (E));
         Print_Irep (LHS);
         return
           Make_Code_Assign
             (Lhs => LHS,
              Rhs => RHS,
              Source_Location => Source_Location);
      end;
--  TODO First attempt at making Do_Nondet_Var recursive
--        else
--           --  Individual components of the array must be nondetted.
--           if Number_Dimensions (E) = 1 then
--              if Compile_Time_Known_Bounds (E) then
--                 declare
--                    function Get_Range (N : Node_Id) return Node_Id;
--
--                    function  Get_Range (N : Node_Id) return Node_Id is
--                       Scalar_Range_Node : constant Node_Id :=
--                         Scalar_Range (N);
--                    begin
--                       case Nkind (Scalar_Range_Node) is
--                          when N_Range => return Scalar_Range_Node;
--                          when N_Subtype_Indication =>
--                             return Get_Range
--                               (Constraint
--                                  (Range_Expression (Scalar_Range_Node)));
--                          when N_Attribute_Reference =>
--                             return
--                               Get_Range
--                                 (First (Expressions (Scalar_Range_Node)));
--                          when others => return Types.Empty;
--                       end case;
--                    end Get_Range;
--
--                    Index_1 : constant Node_Id := First_Index (E);
--                    Array_Range : constant Node_Id :=
--                      Get_Range (Etype (Index_1));
--
--                    --  The bounds are known at compile time.
--                --  If the index subtype is an enumeration type, Eval_Value
--                 --  will return the position number of the bound.
--                    Low_Index : constant Integer :=
--                      Integer (UI_To_Int
--                               (Expr_Value (Low_Bound (Array_Range))));
--
--                    High_Index : constant Integer :=
--                      Integer (UI_To_Int
--                               (Expr_Value (High_Bound (Array_Range))));
--
--                    --  All goto arrays are based at 0, just subtracting
--                    --  low bound from high bound gives the last index of an
--                    --  equivalent length array base at 0.
--
--                    Zero_Based_Last : constant Integer :=
--                      High_Index - Low_Index;
--
--                    Comp_Type : constant Node_Id := Component_Type (E);
--
--                    Given_Type : constant Irep := Do_Type_Reference (E);
--                    Actual_Type : constant Irep :=
--                      (if Kind (Given_Type) = I_Pointer_Type then
--                            Get_Subtype (Given_Type)
--                       else
--                          Given_Type);
--
--                    Array_Irep : constant Irep :=
--                      Make_Symbol_Expr
--                        (Source_Location => Loc,
--                         I_Type          => Actual_Type,
--                         Range_Check     => False,
--                         Identifier      => Unique_Object_Name);
--
--                    Data_Irep : constant Irep :=
--                      Get_Data_Member (Array_Irep, Global_Symbol_Table);
--                    Data_Type : constant Irep := Get_Type (Data_Irep);
--                    Element_Type : constant Irep :=
--                      Get_Subtype (Data_Type);
--
--                 begin
--                    for I in Low_Index .. High_Index loop
--                       declare
--                          Indexed_Data : constant Irep :=
--                            Offset_Array_Data
--                              (Base => Array_Irep,
--                               Offset =>
--                                 Build_Index_Constant
--                                   (Value      => Int (I),
--                                    Source_Loc => Source_Location));
--
--                          Index_Expr : constant Irep :=
--                            Make_Dereference_Expr
--                              (Object          => Indexed_Data,
--                               Source_Location => Loc,
--                               I_Type          => Element_Type,
--                               Range_Check     => False);
--
--                       begin
--                          Put_Line ("Compnent type");
--                          Print_Irep (Do_Type_Reference (Comp_Type));
--                          Put_Line ("Data_Irep");
--                          Print_Irep (Data_Irep);
--                          Put_Line ("Element_Irep");
--                          Print_Irep (Element_Type);
--                       --  Recursively call Do_Nondet_Var on each component
--                          --  of the array.
--                          Do_Nondet_Var
--                            (Var_Name => ,
--                             Var_Type => ,
--                             E        => )
--                       end;
--                    end loop;
--                 end;
--      end if;
   end Do_Nondet_Var;

   --------------------
   -- Do_Var_In_Type --
   --------------------

   function Do_Var_In_Type (Var_Name, Var_Type  : String;
                            Var_Irep, Type_Irep : Irep;
                            E                   : Entity_Id;
                            Bound               : Bound_Sort) return Irep is
      Source_Location : constant Irep := Get_Source_Location (E);
      Followed_Type : constant Irep :=
        Follow_Symbol_Type (Type_Irep, Global_Symbol_Table);
      Resolved_Var : constant Irep :=
        Cast_Enum (Var_Irep, Global_Symbol_Table);
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

            Bound_Irep : constant Irep :=
              (if Bound = Lower then
                  Cast_Enum (Get_Bound (E, Resolved_Type, Bound_Low),
                 Global_Symbol_Table)
               else
                  Cast_Enum (Get_Bound (E, Resolved_Type, Bound_High),
                 Global_Symbol_Table));

            Bound_Condition : constant Irep :=
              (if Bound = Lower then
                  Make_Op_Geq
                 (Rhs =>
                      Typecast_If_Necessary
                    (Bound_Irep,
                     Get_Type (Resolved_Var),
                     Global_Symbol_Table),
                  Lhs             => Resolved_Var,
                  Source_Location => Source_Location,
                  Overflow_Check  => False,
                  I_Type          => Make_Bool_Type,
                  Range_Check     => False)
               else
                  Make_Op_Leq
                 (Rhs             =>
                      Typecast_If_Necessary
                    (Bound_Irep,
                     Get_Type (Resolved_Var),
                     Global_Symbol_Table),
                  Lhs             => Resolved_Var,
                  Source_Location => Source_Location,
                  Overflow_Check  => False,
                  I_Type          => Make_Bool_Type,
                  Range_Check     => False));

            Ret : Irep;
         begin
            Put_Line ("Followed_Type");
            Print_Irep (Followed_Type);
            Put_Line ("Resolved_Var");
            Print_Irep (Resolved_Var);
            if Kind (Resolved_Var) = I_Dereference_Expr then
               Print_Irep (Get_Object (Resolved_Var));
               if Kind (Get_Object (Resolved_Var)) = I_Op_Add then
                  Print_Irep (Get_Lhs (Get_Object (Resolved_Var)));
               else
                  Put_Line ("Not an Add");
               end if;

            else
               Put_Line ("Not a dereference");
            end if;

            Put_Line ("Bound_Irep");
            Print_Irep (Bound_Irep);
            Put_Line ("Condition");
            Print_Irep (Bound_Condition);

            Print_Modelling_Message
              ("Assume (" &
               (if Bound = Lower then
                       Var_Name & " >= " & Var_Type & "'First);"
                  else
                     Var_Name & " <= " & Var_Type & "'Last);"),
               Sloc (E));
            Ret :=
              Make_Assume_Call (Assumption     => Bound_Condition,
                                Source_Loc     => Source_Location,
                                A_Symbol_Table => Global_Symbol_Table);
            Print_Irep (Ret);
            return Ret;
         end;
      else
         return Report_Unhandled_Node_Irep
           (E,
            "Do_Var_In_Type",
            Irep_Kind'Image (Kind (Followed_Type)) & " objects not supported");
      end if;

   end Do_Var_In_Type;

   -------------------------
   -- Make_Selector_Names --
   -------------------------

   procedure Make_Selector_Names (Unique_Object_Name : String;
                                  Root_Irep : Irep;
                                  Block : Irep;
                                  Root_Type : Node_Id;
                                  E : Entity_Id;
                                  Loc : Irep) is
      Type_Irep : constant Irep := Do_Type_Reference (Root_Type);
   begin
      Put_Line ("In Make_Selector_Names");
      if Is_Scalar_Type (Root_Type) then
         Put_Line ("A scalar");
         Append_Op (Block,
                    Do_Var_In_Type (Var_Name  => Unique_Object_Name,
                                    Var_Type  => Unique_Name (Root_Type),
                                    Var_Irep  => Root_Irep,
                                    Type_Irep => Type_Irep,
                                    E         => E,
                                    Bound     => Lower));
         Append_Op (Block,
                    Do_Var_In_Type (Var_Name  => Unique_Object_Name,
                                    Var_Type  => Unique_Name (Root_Type),
                                    Var_Irep  => Root_Irep,
                                    Type_Irep => Type_Irep,
                                    E         => E,
                                    Bound     => Highier));
         Put_Line ("Appended");
      elsif Is_Record_Type (Root_Type) then
         if not Has_Discriminants (Root_Type) then
            declare
               Comp : Node_Id :=
                 First_Component (Root_Type);
            begin
               while Present (Comp) loop
                  declare
                     Comp_Name : constant String :=
                       Unique_Object_Name & "__" &
                       Get_Name_String (Chars (Comp));
                     Comp_Unique : constant String := Unique_Name (Comp);
                     Comp_Type : constant Node_Id :=
                       Etype (Comp);
                  begin
                     Make_Selector_Names
                       (Comp_Name & " == " & Comp_Unique,
                        Make_Member_Expr (Compound         => Root_Irep,
                                          Source_Location  => Loc,
                                          I_Type           => Do_Type_Reference
                                            (Comp_Type),
                                          Component_Name   => Comp_Unique),
                        Block,
                        Comp_Type,
                        E,
                        Loc);
                  end;
                  Comp := Next_Component (Comp);
               end loop;
            end;
         else
            Report_Unhandled_Node_Empty
              (E,
               "Make_Selector_Names",
               "Discrimiminated records are currently unsupported");
         end if;
      elsif Is_Array_Type (Root_Type) then
         --  For the moment only tackle 1 dimensional arrays
         if Number_Dimensions (Root_Type) = 1 then
            --  Currently gnatogoto only handles bounds of arrays
            --  which are known at compile time
            if Compile_Time_Known_Bounds (Root_Type) then
               declare
                  function Get_Range (N : Node_Id) return Node_Id;

                  function  Get_Range (N : Node_Id) return Node_Id is
                     Scalar_Range_Node : constant Node_Id :=
                       Scalar_Range (N);
                  begin
                     case Nkind (Scalar_Range_Node) is
                        when N_Range => return Scalar_Range_Node;
                        when N_Subtype_Indication =>
                           return Get_Range
                             (Constraint
                                (Range_Expression (Scalar_Range_Node)));
                        when N_Attribute_Reference =>
                           return
                             Get_Range
                               (First (Expressions (Scalar_Range_Node)));
                        when others => return Types.Empty;
                     end case;
                  end Get_Range;

                  Index_1 : constant Node_Id := First_Index (Root_Type);
                  Array_Range : constant Node_Id :=
                    Get_Range (Etype (Index_1));

                  --  The bounds are known at compile time.
                  --  If the index subtype is an enumeration type, Eval_Value
                  --  will return the position number of the bound.
                  Low_Index : constant Integer :=
                    Integer (UI_To_Int
                             (Expr_Value (Low_Bound (Array_Range))));

                  High_Index : constant Integer :=
                    Integer (UI_To_Int
                             (Expr_Value (High_Bound (Array_Range))));

                  --  All goto arrays are based at 0, just subtracting
                  --  low bound from high bound gives the last index of an
                  --  equivalent length array base at 0.

                  Comp_Type : constant Node_Id := Component_Type (Root_Type);

                  Given_Type : constant Irep := Get_Type (Root_Irep);
                  Actual_Type : constant Irep :=
                    (if Kind (Given_Type) = I_Pointer_Type then
                          Get_Subtype (Given_Type)
                     else
                        Given_Type);

                  Array_Irep : constant Irep :=
                    Make_Symbol_Expr
                      (Source_Location => Loc,
                       I_Type          => Actual_Type,
                       Range_Check     => False,
                       Identifier      => Unique_Object_Name);

                  First_Irep : constant Irep :=
                    Get_First_Index (Array_Irep);

                  Last_Irep : constant Irep :=
                    Get_Last_Index (Array_Irep);

               begin
                  for I in Low_Index .. High_Index loop
                     declare
                        Checked_Index : constant Irep :=
                          Make_Index_Assert_Expr
                            (N           => E,
                             Index       =>
                               Build_Index_Constant
                                 (Value      => Int (I),
                                  Source_Loc => Loc),
                             First_Index => First_Irep,
                             Last_Index  => Last_Irep);

                        Zero_Based_Index : constant Irep :=
                          Make_Op_Sub (Rhs             => First_Irep,
                                       Lhs             => Checked_Index,
                                       Source_Location => Loc,
                                       Overflow_Check  => False,
                                       I_Type          => CProver_Size_T,
                                       Range_Check     => False);

                        Indexed_Data : constant Irep :=
                          Offset_Array_Data
                            (Base => Array_Irep,
                             Offset => Zero_Based_Index);

                        Data_Irep : constant Irep :=
                          Get_Data_Member (Array_Irep, Global_Symbol_Table);
                        Data_Type : constant Irep := Get_Type (Data_Irep);
                        Element_Type : constant Irep :=
                          Get_Subtype (Data_Type);

                        Index_Expr : constant Irep :=
                          Make_Dereference_Expr
                            (Object          => Indexed_Data,
                             Source_Location => Loc,
                             I_Type          => Element_Type,
                             Range_Check     => False);

                     begin
                        Put_Line ("Compnent type");
                        Print_Irep (Do_Type_Reference (Comp_Type));
                        Put_Line ("Data_Irep");
                        Print_Irep (Data_Irep);
                        Put_Line ("Element_Irep");
                        Print_Irep (Element_Type);
                        Make_Selector_Names
                          (Unique_Object_Name =>
                           Unique_Object_Name & " (" &
                             Integer'Image (I) & ")",
                           Root_Irep          => Index_Expr,
                           Block              => Block,
                           Root_Type          => Comp_Type,
                           E                  => E,
                           Loc                => Loc);
                     end;
                  end loop;
               end;
            else
               Report_Unhandled_Node_Empty
                 (E,
                  "Make_Selector_Names",
                  "Only bounds known at compile time");
            end if;
         else
            Report_Unhandled_Node_Empty (E,
                                         "Make_Selector_Names",
                                         "Only one dimensional arrays");
         end if;
      else
         Report_Unhandled_Node_Empty
           (E,
            "Make_Selector_Names",
            "Unknown object type");
      end if;
   end Make_Selector_Names;

end ASVAT_Modelling.Nondet;
