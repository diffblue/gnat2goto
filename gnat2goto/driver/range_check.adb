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

   function Get_Bound (Bound_Type : Irep; Pos : Bound_Low_Or_High) return Irep
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
   end Get_Bound;

   ----------------------------
   -- Make_Range_Assert_Expr --
   ----------------------------

   function Make_Range_Assert_Expr (N : Node_Id; Value : Irep;
                                    Bounds_Type : Irep)
                                    return Irep
   is
      Call_Inst : constant Irep := New_Irep (I_Side_Effect_Expr_Function_Call);
      Call_Args : constant Irep := New_Irep (I_Argument_List);
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
         Func_Params : constant Irep := New_Irep (I_Parameter_List);
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
        Get_Bound (Followed_Bound_Type, Bound_Low);
      Upper_Bound : constant Irep :=
        Get_Bound (Followed_Bound_Type, Bound_High);
   begin
      Append_Argument (Call_Args, Value);
      Append_Argument (Call_Args, Lower_Bound);
      Append_Argument (Call_Args, Upper_Bound);

      Set_Function (I     => Call_Inst,
                    Value => Symbol_Expr (Build_Assert_Function));
      Set_Arguments (I     => Call_Inst,
                     Value => Call_Args);
      Set_Source_Location (I     => Call_Inst,
                           Value => Sloc (N));
      Set_Type (I     => Call_Inst,
                Value => Actual_Type);
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

      Op_Geq : constant Irep := New_Irep (I_Op_Geq);
      Op_Leq : constant Irep := New_Irep (I_Op_Leq);

      Adjusted_Value_Expr : Irep;
      Adjusted_Lower_Bound : Irep;
      Adjusted_Upper_Bound : Irep;
      Source_Location : constant Source_Ptr := Get_Source_Location
        (Value_Expr);
   begin
      pragma Assert (Kind (Bound_Type) in
                       I_Bounded_Unsignedbv_Type
                       | I_Bounded_Signedbv_Type
                       | I_Bounded_Floatbv_Type);
      --  The compared expressions (value and bound) have to be of the
      --  same type
      if Get_Width (Bound_Type) > Get_Width (Value_Expr_Type)
      then
         --  If the value checked for being in the range is of smaller
         --  type then we need to cast it to the type of the bounds
         Adjusted_Value_Expr :=
           Typecast_If_Necessary (Value_Expr, Bound_Type, Global_Symbol_Table);
         Adjusted_Lower_Bound := Lower_Bound;
         Adjusted_Upper_Bound := Upper_Bound;
      else
         --  If the bounds are of smaller type then we cast the bounds
         --  to the type of the value being checked
         Adjusted_Value_Expr := Value_Expr;
         Adjusted_Lower_Bound :=
           Typecast_If_Necessary (Lower_Bound, Value_Expr_Type,
                                  Global_Symbol_Table);
         Adjusted_Upper_Bound :=
           Typecast_If_Necessary (Upper_Bound, Value_Expr_Type,
                                  Global_Symbol_Table);
      end if;
      Set_Lhs (Op_Geq, Adjusted_Value_Expr);
      Set_Rhs (Op_Geq, Adjusted_Lower_Bound);
      Set_Type (Op_Geq, Make_Bool_Type);
      Set_Lhs (Op_Leq, Adjusted_Value_Expr);

      Set_Rhs (Op_Leq, Adjusted_Upper_Bound);
      Set_Type (Op_Leq, Make_Bool_Type);
      return R : constant Irep := New_Irep (I_Op_And) do
         Append_Op (R, Op_Geq);
         Append_Op (R, Op_Leq);
         Set_Type (R, Make_Bool_Type);
         Set_Source_Location (R, Source_Location);
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
      Bit_Width : constant Float_Format := To_Float_Format (Actual_Type);
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
      SE_Call_Expr : constant Irep :=
        New_Irep (I_Code_Function_Call);
      Sym_Assert   : constant Irep := New_Irep (I_Symbol_Expr);
      Assert_Args  : constant Irep := New_Irep (I_Argument_List);
   begin
      Set_Identifier (Sym_Assert, "__CPROVER_assert");
      Set_Type (Sym_Assert, New_Irep (I_Code_Type));

      Append_Argument (Assert_Args, Assertion);
      Append_Argument (Assert_Args, Description);

      Set_Lhs (I     => SE_Call_Expr,
               Value => Make_Nil (Sloc (N)));
      Set_Source_Location (SE_Call_Expr, Sloc (N));
      Set_Function        (SE_Call_Expr, Sym_Assert);
      Set_Arguments       (SE_Call_Expr, Assert_Args);
      Set_Type            (SE_Call_Expr, Make_Void_Type);
      return SE_Call_Expr;
   end Make_Assert_Call;

end Range_Check;
