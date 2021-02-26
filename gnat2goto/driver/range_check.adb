with GOTO_Utils;            use GOTO_Utils;

with Binary_To_Hex;         use Binary_To_Hex;
with Follow;                use Follow;
with Symbol_Table_Info;     use Symbol_Table_Info;
with Tree_Walk;             use Tree_Walk;

package body Range_Check is

   ---------------------
   -- Store_Nat_Bound --
   ---------------------

   function Store_Nat_Bound (Number : Bound_Type_Nat) return Integer
   is
      Length : constant Integer := Integer (All_Bounds_Table.Length);
      Where : Nat_Bounds_Map.Cursor;
      Success_Insert : Boolean;
   begin
      All_Bounds_Table.Append (Nat_Bound);
      Nat_Bounds_Table.Insert (Key      => Length,
                               New_Item => Number,
                               Position => Where,
                               Inserted => Success_Insert);
      pragma Assert (Success_Insert);
      return Length;
   end Store_Nat_Bound;

   ----------------------
   -- Store_Real_Bound --
   ----------------------

   function Store_Real_Bound (Number : Bound_Type_Real) return Integer
   is
      Length : constant Integer := Integer (All_Bounds_Table.Length);
      Where : Real_Bounds_Map.Cursor;
      Success_Insert : Boolean;
   begin
      All_Bounds_Table.Append (Real_Bound);
      Real_Bounds_Table.Insert (Key      => Length,
                                New_Item => Number,
                                Position => Where,
                                Inserted => Success_Insert);
      pragma Assert (Success_Insert);
      return Length;
   end Store_Real_Bound;

   ----------------------
   -- Store_Symbol_Bound --
   ----------------------

   function Store_Symbol_Bound (Number : Bound_Type_Symbol) return Integer
   is
      Length : constant Integer := Integer (All_Bounds_Table.Length);
      Where : Symbol_Bounds_Map.Cursor;
      Success_Insert : Boolean;
   begin
      All_Bounds_Table.Append (Symb_Bound);
      Expr_Bounds_Table.Insert (Key      => Length,
                                New_Item => Number,
                                Position => Where,
                                Inserted => Success_Insert);
      pragma Assert (Success_Insert);
      return Length;
   end Store_Symbol_Bound;

   function Get_Bound_Type (Bound_Index : Integer) return Bound_Type is
   begin
      return All_Bounds_Table.Element (Bound_Index);
   end Get_Bound_Type;

   function Get_Bound (N : Node_Id; Bound_Type : Irep; Pos : Bound_Low_Or_High)
                             return Irep
   is
   begin
      case Kind (Bound_Type) is
         when I_Bounded_Signedbv_Type
            | I_Bounded_Unsignedbv_Type
            | I_Bounded_Floatbv_Type
            | I_C_Enum_Type =>
            return Get_Bound_Of_Bounded_Type (Bound_Type, Pos);
         when I_Unsignedbv_Type =>
            --  this case is probably unnecessary:
            --  1: how does one create non-bounded unsigned types anyway
            --  2: CBMC has facilities to check unsigned overflow
            declare
               Type_Width : constant Integer := Get_Width (Bound_Type);
               Bound_Value : constant Uint :=
                 (if Pos = Bound_Low
                  then Uint_0
                  else UI_From_Int (2 ** Type_Width - 1));
            begin
               return Make_Constant_Expr
                 (Source_Location => Get_Source_Location (N),
                  I_Type          => Maybe_Double_Type_Width (Bound_Type),
                  Range_Check     => False,
                  Value           => Convert_Uint_To_Hex
                    (Bound_Value,
                     Types.Pos (Type_Width * 2)));
            end;
         when I_Signedbv_Type =>
            declare
               Type_Width : constant Integer := Get_Width (Bound_Type);
               Bound_Value : constant Uint :=
                 (if Pos = Bound_Low
                  then UI_From_Int (-(2 ** (Type_Width - 1)))
                  else UI_From_Int (2 ** (Type_Width - 1) - 1));
            begin
               return Make_Constant_Expr
                 (Source_Location => Get_Source_Location (N),
                  I_Type          => Maybe_Double_Type_Width (Bound_Type),
                  Range_Check     => False,
                  Value           => Convert_Uint_To_Hex
                    (Bound_Value,
                     Types.Pos (Type_Width * 2)));
            end;
         when I_Floatbv_Type =>
            declare
               Smallest_Float : constant String := "C7EFFFFFE0000000";
               --  0xff7fffff : single-precision min stored in double precision
               Largest_Float : constant String := "47EFFFFFE0000000";
               --  0x7f7fffff : single-precision max stored in double precision
               Result_Type : constant Irep :=
                 (case To_Float_Format (Get_Width (Bound_Type)) is
                     when others => Float64_T);
               --  We may want to extend it here for wider floats
            begin
               return Make_Constant_Expr
                 (Source_Location => Get_Source_Location (N),
                  I_Type          => Result_Type,
                  Range_Check     => False,
                  Value           => (if Pos = Bound_Low
                                      then Smallest_Float
                                      else Largest_Float));
            end;
         when others =>
               return Report_Unhandled_Node_Irep (N, "Get_Bound",
                                                  "Unsupported range type");
      end case;
   end Get_Bound;

   function Get_Bound_Of_Bounded_Type (Bound_Type : Irep;
                                       Pos : Bound_Low_Or_High) return Irep
   is
      Bound_Index : constant Integer := (if Pos = Bound_Low
                                         then Get_Lower_Bound (Bound_Type)
                                         else Get_Upper_Bound (Bound_Type));
      Source_Loc : constant Irep := Internal_Source_Location;
   begin
      case Get_Bound_Type (Bound_Index) is
         when Nat_Bound =>
            return Make_Constant_Expr (Source_Location => Source_Loc,
                                       I_Type          => Bound_Type,
                                       Range_Check     => False,
                     Value => Load_Nat_Bound_In_Hex (Bound_Index, Bound_Type));
         when Real_Bound =>
            return Make_Constant_Expr (Source_Location => Source_Loc,
                                       I_Type          => Bound_Type,
                                       Range_Check     => False,
                   Value => Load_Real_Bound_In_Hex (Bound_Index, Bound_Type));
         when Symb_Bound =>
            return Load_Symbol_Bound (Bound_Index);
      end case;
   end Get_Bound_Of_Bounded_Type;

   function Make_Div_Zero_Assert_Expr (N : Node_Id;
                                       Value : Irep;
                                       Divisor : Irep) return Irep
   is
      Value_Type : constant Irep :=
        Follow_Symbol_Type (Get_Type (Value), Global_Symbol_Table);
      Source_Loc : constant Irep := Get_Source_Location (N);
      Compare_Type : constant Irep :=
        (if Kind (Value_Type) = I_Bounded_Floatbv_Type or else
         Kind (Value_Type) = I_Floatbv_Type then
              Float64_T else Int64_T);
      Type_String : constant String :=
        (if Kind (Value_Type) = I_Bounded_Floatbv_Type or else
         Kind (Value_Type) = I_Floatbv_Type then
              "_Flt" else "_Int");

      function Build_Assert_Function return Symbol;

      ---------------------------
      -- Build_Assert_Function --
      ---------------------------

      --  Build a symbol for the following function
      --  Value_Type division_check(Value_Type value, Value_Type divisor)
      --  {
      --    __CPROVER_Ada_Division_Check (divisor != 0);
      --    return value;
      --  }
      function Build_Assert_Function return Symbol
      is
         Func_Name : constant String := "__division_check" & Type_String;
         Body_Block : constant Irep := Make_Code_Block (Source_Loc);
         Func_Params : constant Irep := Make_Parameter_List;
         Value_Arg : constant Irep :=
           Create_Fun_Parameter (Fun_Name        => Func_Name,
                                 Param_Name      => "value",
                                 Param_Type      => Compare_Type,
                                 Param_List      => Func_Params,
                                 A_Symbol_Table  => Global_Symbol_Table,
                                 Source_Location => Source_Loc);
         Divisor_Arg : constant Irep :=
           Create_Fun_Parameter (Fun_Name        => Func_Name,
                                 Param_Name      => "divisor",
                                 Param_Type      => Compare_Type,
                                 Param_List      => Func_Params,
                                 A_Symbol_Table  => Global_Symbol_Table,
                                 Source_Location => Source_Loc);
         Func_Type : constant Irep :=
           Make_Code_Type (Parameters  => Func_Params,
                           Ellipsis    => False,
                           Return_Type => Value_Type,
                           Inlined     => False,
                           Knr         => False);
         Value_Param : constant Irep := Param_Symbol (Value_Arg);
         Divisor_Param : constant Irep := Param_Symbol (Divisor_Arg);
         Return_Inst : constant Irep :=
           Make_Code_Return (Return_Value    => Typecast_If_Necessary (
                             Value_Param, Value_Type,
                             Global_Symbol_Table),
                             Source_Location => Source_Loc,
                             I_Type          => Ireps.Empty);
         Division_Check_Args : constant Irep := Make_Argument_List;
         Divisor_Neq_Zero_Expr : constant Irep :=
           Make_Op_Notequal (Rhs             =>
                               Typecast_If_Necessary (Get_Int32_T_Zero,
                                 Compare_Type, Global_Symbol_Table),
                             Lhs             => Divisor_Param,
                             Source_Location => Source_Loc,
                             Overflow_Check  => False,
                             I_Type          => Make_Bool_Type,
                             Range_Check     => False);
         Neq_Zero_As_Int : constant Irep :=
           Typecast_If_Necessary (Expr           => Divisor_Neq_Zero_Expr,
                                  New_Type       => Int32_T,
                                  A_Symbol_Table => Global_Symbol_Table);
         Division_Check_Sym_Expr : constant Irep :=
           Symbol_Expr (Get_Ada_Check_Symbol ("__CPROVER_Ada_Division_Check",
                        Global_Symbol_Table, Source_Loc));
         Division_Check_Call : constant Irep :=
           Make_Code_Function_Call (Arguments       => Division_Check_Args,
                                    I_Function      => Division_Check_Sym_Expr,
                                    Lhs             => Make_Nil (Source_Loc),
                                    Source_Location => Source_Loc,
                                    I_Type          => Make_Void_Type);
      begin
         Append_Argument (Division_Check_Args, Neq_Zero_As_Int);
         Append_Op (Body_Block, Division_Check_Call);
         Append_Op (Body_Block, Return_Inst);

         return New_Function_Symbol_Entry
           (Name        => Func_Name,
            Symbol_Type => Func_Type,
            Value       => Body_Block,
            A_Symbol_Table => Global_Symbol_Table);
      end Build_Assert_Function;

      Call_Args : constant Irep := Make_Argument_List;

   begin
      Append_Argument (Call_Args,
                       Typecast_If_Necessary
                         (Expr           => Value,
                          New_Type       => Compare_Type,
                          A_Symbol_Table => Global_Symbol_Table));
      Append_Argument (Call_Args,
                       Typecast_If_Necessary
                         (Expr           => Divisor,
                          New_Type       => Compare_Type,
                          A_Symbol_Table => Global_Symbol_Table));

      return Make_Side_Effect_Expr_Function_Call
        (Arguments       => Call_Args,
         I_Function      => Symbol_Expr (Build_Assert_Function),
         Source_Location => Source_Loc,
         I_Type          => Value_Type);
   end Make_Div_Zero_Assert_Expr;

   function Make_Index_Assert_Expr (N : Node_Id; Index : Irep;
                                    First_Index : Irep; Last_Index : Irep)
                                    return Irep
   is
      Index_Type : constant Irep :=
        Follow_Symbol_Type (Get_Type (Index), Global_Symbol_Table);
      pragma Assert (Kind (Index_Type) in Class_Type);
   begin
      return Make_Range_Assert_Expr (N                    => N,
                                     Value                => Index,
                                     Lower_Bound          => First_Index,
                                     Upper_Bound          => Last_Index,
                                     Expected_Return_Type => Index_Type,
                          Check_Name           => "Index_Check");
   end Make_Index_Assert_Expr;

   function Make_Overflow_Assert_Expr (N : Node_Id; Value : Irep) return Irep
   is
      Value_Type : constant Irep :=
        Follow_Symbol_Type (Get_Type (Value), Global_Symbol_Table);
      Low_Value : constant Irep := Get_Bound (N, Value_Type, Bound_Low);
      High_Value : constant Irep := Get_Bound (N, Value_Type, Bound_High);
      Maybe_Casted_Value : constant Irep := Value;
   begin
      pragma Assert (Kind (Value) in Class_Expr and then
                     Kind (Value_Type) in Class_Type);

      pragma Assert (Get_Type (Low_Value) = Get_Type (High_Value));

      if Kind (Value) in Class_Binary_Expr
      then
         declare
            Cast_Lhs : constant Irep :=
              Typecast_If_Necessary (Get_Lhs (Value), Get_Type (Low_Value),
                                     Global_Symbol_Table);
            Cast_Rhs : constant Irep :=
              Typecast_If_Necessary (Get_Rhs (Value), Get_Type (Low_Value),
                                     Global_Symbol_Table);
         begin
            Set_Lhs (Maybe_Casted_Value, Cast_Lhs);
            Set_Rhs (Maybe_Casted_Value, Cast_Rhs);
            Set_Type (Maybe_Casted_Value, Get_Type (Low_Value));
         end;
      end if;

      if Kind (Value) in Class_Unary_Expr
      then
         declare
            Cast_Op : constant Irep :=
              Typecast_If_Necessary (Get_Op0 (Value), Get_Type (Low_Value),
                                     Global_Symbol_Table);
         begin
            Set_Op0 (Maybe_Casted_Value, Cast_Op);
            Set_Type (Maybe_Casted_Value, Get_Type (Low_Value));
         end;
      end if;

      return Make_Range_Assert_Expr (N           => N,
                                     Value       => Maybe_Casted_Value,
                                     Lower_Bound => Low_Value,
                                     Upper_Bound => High_Value,
                                     Expected_Return_Type => Value_Type,
                                Check_Name  => "Overflow_Check");
   end Make_Overflow_Assert_Expr;

   function Make_Range_Assert_Expr (N : Node_Id; Value : Irep;
                                    Lower_Bound : Irep; Upper_Bound : Irep;
                                    Expected_Return_Type : Irep;
                                    Check_Name : String)
                                    return Irep
   is
      Call_Args : constant Irep := Make_Argument_List;
      Underlying_Lower_Type : constant Irep :=
        Follow_Symbol_Type (Get_Type (Lower_Bound), Global_Symbol_Table);
      Underlying_Upper_Type : constant Irep :=
        Follow_Symbol_Type (Get_Type (Upper_Bound), Global_Symbol_Table);
      Source_Loc : constant Irep := Get_Source_Location (N);
      Compare_Type : constant Irep :=
        (if Kind (Underlying_Lower_Type) = I_Bounded_Floatbv_Type or else
         Kind (Underlying_Lower_Type) = I_Floatbv_Type then
              Float64_T else Int64_T);
      Type_String : constant String :=
        (if Kind (Underlying_Lower_Type) = I_Bounded_Floatbv_Type or else
         Kind (Underlying_Lower_Type) = I_Floatbv_Type then
              "_Flt" else "_Int");

      function Build_Assert_Function return Symbol;

      ---------------------------
      -- Build_Assert_Function --
      ---------------------------

      --  Build a symbol for the following function
      --  Actual_Type range_check(Actual_Type value, Actual_Type lower_bound,
      --                          Actual_Type upper_bound) {
      --    `Check_Name` (value >= lower_bound && value <= upper_bound);
      --    return value;
      --  }
      function Build_Assert_Function return Symbol
      is
         Func_Name : constant String := ("range_check__"
                                         & Check_Name & Type_String);
         Body_Block : constant Irep :=
           Make_Code_Block (Get_Source_Location (N));
         Func_Params : constant Irep := Make_Parameter_List;
         Value_Arg : constant Irep :=
           Create_Fun_Parameter (Fun_Name        => Func_Name,
                                 Param_Name      => "value",
                                 Param_Type      => Compare_Type,
                                 Param_List      => Func_Params,
                                 A_Symbol_Table  => Global_Symbol_Table,
                                 Source_Location => Source_Loc);
         Lower_Bound_Arg : constant Irep :=
           Create_Fun_Parameter (Fun_Name        => Func_Name,
                                 Param_Name      => "low",
                                 Param_Type      => Compare_Type,
                                 Param_List      => Func_Params,
                                 A_Symbol_Table  => Global_Symbol_Table,
                                 Source_Location => Source_Loc);
         Upper_Bound_Arg : constant Irep :=
           Create_Fun_Parameter (Fun_Name        => Func_Name,
                                 Param_Name      => "high",
                                 Param_Type      => Compare_Type,
                                 Param_List      => Func_Params,
                                 A_Symbol_Table  => Global_Symbol_Table,
                                 Source_Location => Source_Loc);
         Func_Type : constant Irep := Make_Code_Type (
                            --  Function parameters should only be created via
                            --  Create_Fun_Parameter
                            Parameters  => Func_Params,
                            Ellipsis    => False,
                            Return_Type => Expected_Return_Type,
                            Inlined     => False,
                            Knr         => False);
         Value_Param : constant Irep := Param_Symbol (Value_Arg);
         Lower_Bound_Param : constant Irep := Param_Symbol (Lower_Bound_Arg);
         Upper_Bound_Param : constant Irep := Param_Symbol (Upper_Bound_Arg);
         --
         Return_Inst : constant Irep :=
           Make_Code_Return (Return_Value    =>
                               Typecast_If_Necessary (
                                 Value_Param, Expected_Return_Type,
                                 Global_Symbol_Table),
                             Source_Location => Source_Loc,
                             I_Type          => Ireps.Empty);
         Range_Check_Args : constant Irep := Make_Argument_List;
         Range_Expr : constant Irep :=
           Make_Range_Expression (Value_Expr  => Value_Param,
                                  Lower_Bound => Lower_Bound_Param,
                                  Upper_Bound => Upper_Bound_Param);
         Range_Expr_As_Int : constant Irep :=
           Typecast_If_Necessary (Expr           => Range_Expr,
                                  New_Type       => Int32_T,
                                  A_Symbol_Table => Global_Symbol_Table);
         Range_Check_Sym_Expr : constant Irep :=
           Symbol_Expr (Get_Ada_Check_Symbol ("__CPROVER_Ada_" & Check_Name,
                        Global_Symbol_Table, Source_Loc));
         Range_Check_Call : constant Irep :=
           Make_Code_Function_Call (Arguments       => Range_Check_Args,
                                    I_Function      => Range_Check_Sym_Expr,
                                    Lhs             => Make_Nil (Source_Loc),
                                    Source_Location => Source_Loc,
                                    I_Type          => Make_Void_Type);
      begin
         Append_Argument (Range_Check_Args, Range_Expr_As_Int);
         Append_Op (Body_Block, Range_Check_Call);
         Append_Op (Body_Block, Return_Inst);

         return New_Function_Symbol_Entry
           (Name        => Func_Name,
            Symbol_Type => Func_Type,
            Value       => Body_Block,
            A_Symbol_Table => Global_Symbol_Table);
      end Build_Assert_Function;

      Call_Inst : constant Irep := Make_Side_Effect_Expr_Function_Call
        (Arguments => Call_Args,
         I_Function => Symbol_Expr (Build_Assert_Function),
         Source_Location => Get_Source_Location (N),
         I_Type => Expected_Return_Type);
   begin
      Set_Function (Source_Loc, Get_Context_Name (N));
      pragma Assert (Underlying_Lower_Type = Underlying_Upper_Type);

      Append_Argument (Call_Args,
                       Typecast_If_Necessary
                         (Expr           => Value,
                          New_Type       => Compare_Type,
                          A_Symbol_Table => Global_Symbol_Table));

      Append_Argument (Call_Args,
                       Typecast_If_Necessary
                         (Expr           => Lower_Bound,
                          New_Type       => Compare_Type,
                          A_Symbol_Table => Global_Symbol_Table));
      Append_Argument (Call_Args,
                       Typecast_If_Necessary
                         (Expr           => Upper_Bound,
                          New_Type       => Compare_Type,
                          A_Symbol_Table => Global_Symbol_Table));

      return Call_Inst;
   end Make_Range_Assert_Expr;

   ----------------------------
   -- Make_Range_Assert_Expr --
   ----------------------------

   function Make_Range_Assert_Expr (N : Node_Id; Value : Irep;
                                    Bounds_Type : Irep)
                                    return Irep
   is
      Followed_Bound_Type : constant Irep :=
        Follow_Symbol_Type (Bounds_Type,
                            Global_Symbol_Table);
      Lower_Bound : constant Irep :=
        Get_Bound (N, Followed_Bound_Type, Bound_Low);
      Upper_Bound : constant Irep :=
        Get_Bound (N, Followed_Bound_Type, Bound_High);
   begin
      pragma Assert (Kind (Value) in Class_Expr and then
                     Kind (Get_Type (Value)) in Class_Type);

      return Make_Range_Assert_Expr (N           => N,
                 Value       => Value,
                 Lower_Bound => Lower_Bound,
                 Upper_Bound => Upper_Bound,
                 Expected_Return_Type => Get_Type (Value),
                 Check_Name  => "Range_Check");

   end Make_Range_Assert_Expr;

   -------------------------------
   -- Make_Range_Expression --
   -------------------------------

   function Make_Range_Expression (Value_Expr : Irep;
                                   Lower_Bound : Irep;
                                   Upper_Bound : Irep)
                                   return Irep
   is
      --  The bounds and or the value may be enumeration types.
      --  If so, they are converted to a bitvector type.
      --  When the enumeration is declared each literal s given the
      --  value of its position (starting from 0).
      Bound_Type_Raw      : constant Irep :=
        Follow_Symbol_Type (Get_Type (Lower_Bound), Global_Symbol_Table);
      Value_Expr_Type_Raw : constant Irep :=
        Follow_Symbol_Type (Get_Type (Value_Expr), Global_Symbol_Table);

      Bound_Type : constant Irep :=
        (if Kind (Bound_Type_Raw) = I_C_Enum_Type then
              Int32_T
         else
            Bound_Type_Raw);

      Value_Expr_Type : constant Irep :=
        (if Kind (Value_Expr_Type_Raw) = I_C_Enum_Type then
              Int32_T
         else
            Value_Expr_Type_Raw);

      Resolved_Value_Expr : constant Irep :=
        (if Kind (Value_Expr_Type_Raw) = I_C_Enum_Type then
              Typecast_If_Necessary
           (Expr           => Value_Expr,
            New_Type       => Int32_T,
            A_Symbol_Table => Global_Symbol_Table)
         else
            Value_Expr);

      type Adjusted_Value_And_Bounds_T is
        record
           Value_Expr : Irep;
           Upper_Bound : Irep;
           Lower_Bound : Irep;
        end record;

      function Get_Adjusted_Value_And_Bounds
         return Adjusted_Value_And_Bounds_T;
      function Get_Adjusted_Value_And_Bounds
         return Adjusted_Value_And_Bounds_T
      is
         Greater_Width : constant Boolean :=
          Get_Width (Bound_Type) > Get_Width (Value_Expr_Type);
      begin
         if Greater_Width then
            return (
                    Value_Expr => Typecast_If_Necessary
                      (Resolved_Value_Expr, Bound_Type, Global_Symbol_Table),
                    Upper_Bound => Upper_Bound,
                    Lower_Bound => Lower_Bound);
         else
            return (
                    Value_Expr => Resolved_Value_Expr,
                    Upper_Bound => Typecast_If_Necessary
                      (Upper_Bound, Value_Expr_Type, Global_Symbol_Table),
                    Lower_Bound => Typecast_If_Necessary
                      (Lower_Bound, Value_Expr_Type, Global_Symbol_Table));
         end if;
      end Get_Adjusted_Value_And_Bounds;

      Values_Adjusted : constant Adjusted_Value_And_Bounds_T :=
       Get_Adjusted_Value_And_Bounds;

      Op_Geq : constant Irep := Make_Op_Geq (
        Lhs => Values_Adjusted.Value_Expr,
        Rhs => Values_Adjusted.Lower_Bound,
        I_Type => Make_Bool_Type,
        Source_Location => Get_Source_Location (Value_Expr));
      Op_Leq : constant Irep := Make_Op_Leq (
        Lhs => Values_Adjusted.Value_Expr,
        Rhs => Values_Adjusted.Upper_Bound,
        I_Type => Make_Bool_Type,
        Source_Location => Get_Source_Location (Value_Expr));

      Source_Location : constant Irep := Get_Source_Location
        (Value_Expr);
   begin
      pragma Assert (Kind (Bound_Type) in
                       I_Bounded_Unsignedbv_Type
                       | I_Bounded_Signedbv_Type
                       | I_Bounded_Floatbv_Type
                       | I_Unsignedbv_Type
                       | I_Signedbv_Type
                       | I_Floatbv_Type
                       | I_Ada_Mod_Type);
      return R : constant Irep := Make_Op_And
        (Source_Location => Source_Location,
         I_Type => Make_Bool_Type)
      do
         Append_Op (R, Op_Geq);
         Append_Op (R, Op_Leq);
      end return;
   end Make_Range_Expression;

   -----------------------
   -- Load_Bound_In_Hex --
   -----------------------

   function Load_Nat_Bound_In_Hex (Index : Integer; Actual_Type : Irep)
                               return String
   is
      Bit_Width : constant Pos := Pos (Get_Width (Actual_Type));
      Bound : constant Uint := Uint (Nat_Bounds_Table.Element (Index));
   begin
      return Convert_Uint_To_Hex (
                 Value     => Bound,
                 Bit_Width => Bit_Width);
   end Load_Nat_Bound_In_Hex;

   ----------------------------
   -- Load_Real_Bound_In_Hex --
   ----------------------------

   function Load_Real_Bound_In_Hex (Index : Integer; Actual_Type : Irep)
                                    return String
   is
      Bit_Width : constant Float_Format :=
        To_Float_Format (Get_Width (Actual_Type));
      Bound : constant Ureal :=
        Ureal (Real_Bounds_Table.Element (Index));
   begin
      case Bit_Width is
         when IEEE_32_Bit => return Convert_Ureal_To_Hex_32bits_IEEE (Bound);
         when IEEE_64_Bit => return Convert_Ureal_To_Hex_64bits_IEEE (Bound);
      end case;
   end Load_Real_Bound_In_Hex;

   function Load_Symbol_Bound (Index : Integer) return Irep
   is
   begin
      return Irep (Expr_Bounds_Table.Element (Index));
   end Load_Symbol_Bound;

end Range_Check;
