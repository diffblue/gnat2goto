with Atree;                 use Atree;
with Sinfo;                 use Sinfo;

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
            | I_Bounded_Floatbv_Type =>
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
               return Make_Constant_Expr (Source_Location => No_Location,
                                          I_Type          => Bound_Type,
                                          Range_Check     => False,
                                          Value =>
                                            Convert_Uint_To_Hex (Bound_Value,
                                              Types.Pos (Type_Width)));
            end;
         when I_Signedbv_Type =>
            declare
               Type_Width : constant Integer := Get_Width (Bound_Type);
               Bound_Value : constant Uint :=
                 (if Pos = Bound_Low
                  then UI_From_Int (-(2 ** (Type_Width - 1)))
                  else UI_From_Int (2 ** (Type_Width - 1) - 1));
            begin
               return Make_Constant_Expr (Source_Location => No_Location,
                                          I_Type          => Bound_Type,
                                          Range_Check     => False,
                                          Value =>
                                            Convert_Uint_To_Hex (Bound_Value,
                                              Types.Pos (Type_Width)));
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
      Source_Loc : constant Source_Ptr := No_Location;
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

   ----------------------------
   -- Make_Range_Assert_Expr --
   ----------------------------

   function Make_Range_Assert_Expr (N : Node_Id; Value : Irep;
                                    Bounds_Type : Irep)
                                    return Irep
   is
      Call_Args : constant Irep := Make_Argument_List;
      Actual_Type : constant Irep := Follow_Symbol_Type (Get_Type (Value),
                                                         Global_Symbol_Table);
      Followed_Bound_Type : constant Irep := Follow_Symbol_Type (Bounds_Type,
                                                         Global_Symbol_Table);
      Source_Loc : constant Source_Ptr := Sloc (N);

      function Build_Assert_Function return Symbol;

      ---------------------------
      -- Build_Assert_Function --
      ---------------------------

      --  Build a symbol for the following function
      --  Actual_Type range_check(Actual_Type value, Actual_Type lower_bound,
      --                          Actual_Type upper_bound) {
      --    assert (value >= lower_bound && value <= upper_bound);
      --    return value;
      --  }
      function Build_Assert_Function return Symbol
      is
         Func_Name : constant String := Fresh_Var_Name ("range_check");
         Body_Block : constant Irep := Make_Code_Block (Sloc (N));
         Description : constant Irep := Make_String_Constant_Expr (
                                             Source_Location => Source_Loc,
                                             I_Type          => Ireps.Empty,
                                             Range_Check     => False,
                                             Value           => "Range Check");
         Func_Params : constant Irep := Make_Parameter_List;
         Value_Arg : constant Irep :=
           Create_Fun_Parameter (Fun_Name        => Func_Name,
                                 Param_Name      => "value",
                                 Param_Type      => Actual_Type,
                                 Param_List      => Func_Params,
                                 A_Symbol_Table  => Global_Symbol_Table,
                                 Source_Location => Source_Loc);
         Lower_Bound_Arg : constant Irep :=
           Create_Fun_Parameter (Fun_Name        => Func_Name,
                                 Param_Name      => "low",
                                 Param_Type      => Bounds_Type,
                                 Param_List      => Func_Params,
                                 A_Symbol_Table  => Global_Symbol_Table,
                                 Source_Location => Source_Loc);
         Upper_Bound_Arg : constant Irep :=
           Create_Fun_Parameter (Fun_Name        => Func_Name,
                                 Param_Name      => "high",
                                 Param_Type      => Bounds_Type,
                                 Param_List      => Func_Params,
                                 A_Symbol_Table  => Global_Symbol_Table,
                                 Source_Location => Source_Loc);
         Func_Type : constant Irep := Make_Code_Type (
                            --  Function parameters should only be created via
                            --  Create_Fun_Parameter
                            Parameters  => Func_Params,
                            Ellipsis    => False,
                            Return_Type => Actual_Type,
                            Inlined     => False,
                            Knr         => False);
         Value_Param : constant Irep := Param_Symbol (Value_Arg);
         Lower_Bound_Param : constant Irep := Param_Symbol (Lower_Bound_Arg);
         Upper_Bound_Param : constant Irep := Param_Symbol (Upper_Bound_Arg);
         --
         Return_Inst : constant Irep :=
           Make_Code_Return (Return_Value    => Value_Param,
                             Source_Location => Sloc (N),
                             I_Type          => Ireps.Empty);
      begin
         Append_Op (Body_Block,
                    Make_Assert_Call (Expression (N),
                      Make_Range_Expression
                        (Value_Param, Lower_Bound_Param, Upper_Bound_Param),
                      Description));
         Append_Op (Body_Block, Return_Inst);

         return New_Function_Symbol_Entry (Name        => Func_Name,
                                        Symbol_Type => Func_Type,
                                        Value       => Body_Block,
                                        A_Symbol_Table => Global_Symbol_Table);
      end Build_Assert_Function;

      Lower_Bound : constant Irep :=
        Get_Bound (N, Followed_Bound_Type, Bound_Low);
      Upper_Bound : constant Irep :=
        Get_Bound (N, Followed_Bound_Type, Bound_High);

      Call_Inst : constant Irep := Make_Side_Effect_Expr_Function_Call (
        Arguments => Call_Args,
        I_Function => Symbol_Expr (Build_Assert_Function),
        Source_Location => Sloc (N),
        I_Type => Actual_Type);
   begin
      Append_Argument (Call_Args, Value);
      Append_Argument (Call_Args, Lower_Bound);
      Append_Argument (Call_Args, Upper_Bound);

      return Call_Inst;
   end Make_Range_Assert_Expr;

   -------------------------------
   -- Make_Range_Expression --
   -------------------------------

   function Make_Range_Expression (Value_Expr : Irep;
                                   Lower_Bound : Irep;
                                   Upper_Bound : Irep)
                                   return Irep
   is
      Bound_Type : constant Irep :=
        Follow_Symbol_Type (Get_Type (Lower_Bound), Global_Symbol_Table);
      Value_Expr_Type : constant Irep :=
        Follow_Symbol_Type (Get_Type (Value_Expr), Global_Symbol_Table);

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
               (Value_Expr, Bound_Type, Global_Symbol_Table),
             Upper_Bound => Upper_Bound,
             Lower_Bound => Lower_Bound);
         else
            return (
             Value_Expr => Value_Expr,
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

      Source_Location : constant Source_Ptr := Get_Source_Location
        (Value_Expr);
   begin
      pragma Assert (Kind (Bound_Type) in
                       I_Bounded_Unsignedbv_Type
                       | I_Bounded_Signedbv_Type
                       | I_Bounded_Floatbv_Type
                       | I_Unsignedbv_Type
                       | I_Signedbv_Type);

      return R : constant Irep := Make_Op_And (
          Source_Location => Source_Location,
          I_Type => Make_Bool_Type) do
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

   ----------------------
   -- Make_Assert_Call --
   ----------------------

   function Make_Assert_Call (N : Node_Id; Assertion : Irep;
                              Description : Irep)
                              return Irep is
      Assert_Args  : constant Irep := Make_Argument_List;
      Sym_Assert   : constant Irep := Make_Symbol_Expr (
        Source_Location => Sloc (N),
        I_Type => Make_Code_Type (
          Parameters => Make_Parameter_List,
          Ellipsis => False,
          Return_Type => Make_Void_Type,
          Inlined => False,
          Knr => False),
        Identifier => "__CPROVER_assert");
      SE_Call_Expr : constant Irep :=
        Make_Code_Function_Call (
          Arguments => Assert_Args,
          I_Function => Sym_Assert,
          Lhs => Make_Nil (Sloc (N)),
          Source_Location => Sloc (N),
          I_Type => Make_Void_Type);
   begin
      Append_Argument (Assert_Args, Assertion);
      Append_Argument (Assert_Args, Description);

      return SE_Call_Expr;
   end Make_Assert_Call;

end Range_Check;
