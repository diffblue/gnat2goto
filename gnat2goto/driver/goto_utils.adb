with Namet;   use Namet;
with Nlists;  use Nlists;
with Aspects; use Aspects;
with Binary_To_Hex;         use Binary_To_Hex;

with Ada.Text_IO;           use Ada.Text_IO;
with Follow; use Follow;

package body GOTO_Utils is

   Size_T : Irep := Ireps.Empty;
   function CProver_Size_T return Irep
   is
   begin
      if Size_T = Ireps.Empty then
         Size_T := Make_Symbol_Type ("__CPROVER_size_t");
      end if;
      return Size_T;
   end CProver_Size_T;

   ---------------------
   -- Make_Address_Of --
   ---------------------

   function Make_Address_Of (Base : Irep) return Irep is
      R : constant Irep := New_Irep (I_Address_Of_Expr);
   begin
      Set_Object (R, Base);
      Set_Type   (R, Make_Pointer_Type (Get_Type (Base)));
      return R;
   end Make_Address_Of;

   function Is_Prefix (Prefix : String; Base_String : String) return Boolean
   is
   begin
      if Prefix'Length < Base_String'Length and then
        Prefix = Base_String (Base_String'First .. Prefix'Length)
      then
         return True;
      else
         return False;
      end if;
   end Is_Prefix;

   -------------------
   -- Make_Int_Type --
   -------------------

   function Make_Int_Type (Width : Positive) return Irep is
      I : constant Irep := New_Irep (I_Signedbv_Type);
   begin
      Set_Width (I, Width);
      return I;
   end Make_Int_Type;

   function Make_Signedint_Type (Width : Positive) return Irep is
   begin
      return Make_Signedbv_Type (I_Subtype => Make_Nil_Type, Width => Width);
   end Make_Signedint_Type;

   function Make_Signedbv_Type (Width : Positive) return Irep is
   begin
      return Make_Signedbv_Type (I_Subtype => Make_Nil_Type, Width => Width);
   end Make_Signedbv_Type;

   function Make_Unsignedbv_Type (Width : Positive) return Irep is
   begin
      return Make_Unsignedbv_Type (I_Subtype => Make_Nil_Type, Width => Width);
   end Make_Unsignedbv_Type;

   -----------------------
   -- Make_Pointer_Type --
   -----------------------

   function Make_Pointer_Type (Base : Irep) return Irep is
      R : constant Irep := New_Irep (I_Pointer_Type);
   begin
      Set_Subtype (R, Base);
      Set_Width   (R, Pointer_Type_Width);
      return R;
   end Make_Pointer_Type;

   --------------------
   -- Fresh_Var_Name --
   --------------------

   function Fresh_Var_Name (Infix : String) return String is
      Binder_Number_Str_Raw : constant String :=
        Integer'Image (Synthetic_Variable_Counter);
      Binder_Number_Str : constant String :=
        Binder_Number_Str_Raw (2 .. Binder_Number_Str_Raw'Last);
   begin
      --  Note this is intentionally an illegal Ada identifier
      --  to avoid clashes.
      Synthetic_Variable_Counter := Synthetic_Variable_Counter + 1;
      return "__" & Infix & Binder_Number_Str;
   end Fresh_Var_Name;

   ---------------------------
   -- Fresh_Var_Symbol_Expr --
   ---------------------------

   function Fresh_Var_Symbol_Expr (Ty : Irep; Infix : String) return Irep is
      Id : constant String := Fresh_Var_Name (Infix);
      Ret : constant Irep := New_Irep (I_Symbol_Expr);
   begin
      Set_Identifier (Ret, Id);
      Set_Type (Ret, Ty);
      return Ret;
   end Fresh_Var_Symbol_Expr;

   ------------------
   -- Param_Symbol --
   ------------------

   function Param_Symbol (Param : Irep) return Irep is
      Ret : constant Irep := New_Irep (I_Symbol_Expr);
   begin
      Set_Identifier (Ret, Get_Identifier (Param));
      Set_Type (Ret, Get_Type (Param));
      return Ret;
   end Param_Symbol;

   -----------------
   -- Symbol_Expr --
   -----------------

   function Symbol_Expr (Sym : Symbol) return Irep is
      Ret : constant Irep := New_Irep (I_Symbol_Expr);
   begin
      Set_Identifier (Ret, Unintern (Sym.Name));
      Set_Type (Ret, Sym.SymType);
      return Ret;
   end Symbol_Expr;

   procedure New_Object_Symbol_Entry (Object_Name : Symbol_Id;
                                      Object_Type : Irep;
                                      Object_Init_Value : Irep;
                                      A_Symbol_Table : in out Symbol_Table)
   is
      Object_Symbol : Symbol;
   begin
      Object_Symbol.Name       := Object_Name;
      Object_Symbol.BaseName   := Object_Name;
      Object_Symbol.PrettyName := Object_Name;
      Object_Symbol.SymType    := Object_Type;
      Object_Symbol.Mode       := Intern ("C");
      Object_Symbol.Value      := Object_Init_Value;
      Object_Symbol.IsLValue   := True;

      A_Symbol_Table.Insert (Object_Name, Object_Symbol);
   end New_Object_Symbol_Entry;

   procedure New_Subprogram_Symbol_Entry (Subprog_Name : Symbol_Id;
                                          Subprog_Type : Irep;
                                          Subprog_Body : Irep;
                                          A_Symbol_Table : in out Symbol_Table)
   is
      Subprog_Symbol : Symbol;
   begin
      Subprog_Symbol.Name       := Subprog_Name;
      Subprog_Symbol.BaseName   := Subprog_Name;
      Subprog_Symbol.PrettyName := Subprog_Name;
      Subprog_Symbol.SymType    := Subprog_Type;
      Subprog_Symbol.Mode       := Intern ("C");
      Subprog_Symbol.Value      := Subprog_Body;

      A_Symbol_Table.Insert (Subprog_Name, Subprog_Symbol);
   end New_Subprogram_Symbol_Entry;

   procedure New_Type_Symbol_Entry (Type_Name : Symbol_Id; Type_Of_Type : Irep;
                                    A_Symbol_Table : in out Symbol_Table) is
      Type_Symbol : Symbol;
   begin
      Type_Symbol.SymType    := Type_Of_Type;
      Type_Symbol.IsType     := True;
      Type_Symbol.Name       := Type_Name;
      Type_Symbol.PrettyName := Type_Name;
      Type_Symbol.BaseName   := Type_Name;
      Type_Symbol.Mode       := Intern ("C");

      A_Symbol_Table.Insert (Type_Name, Type_Symbol);
   end New_Type_Symbol_Entry;

   procedure New_Valueless_Object_Symbol_Entry (Constant_Name : Symbol_Id;
                                        A_Symbol_Table : in out Symbol_Table)
   is
      Object_Symbol : Symbol;
   begin
      Object_Symbol.Name       := Constant_Name;
      Object_Symbol.BaseName   := Constant_Name;
      Object_Symbol.PrettyName := Constant_Name;
      Object_Symbol.SymType    := Make_Nil (No_Location);
      Object_Symbol.Mode       := Intern ("C");
      Object_Symbol.Value      := Make_Nil (No_Location);

      A_Symbol_Table.Insert (Constant_Name, Object_Symbol);
   end New_Valueless_Object_Symbol_Entry;

   procedure New_Enum_Member_Symbol_Entry (
      Member_Name : Symbol_Id; Base_Name : Symbol_Id; Enum_Type : Irep;
      Value_Expr : Irep; A_Symbol_Table : in out Symbol_Table) is
      Member_Symbol : Symbol;
   begin
      Member_Symbol.Name             := Member_Name;
      Member_Symbol.PrettyName       := Base_Name;
      Member_Symbol.BaseName         := Base_Name;
      Member_Symbol.Mode             := Intern ("C");
      Member_Symbol.IsStaticLifetime := True;
      Member_Symbol.IsStateVar       := True;
      Member_Symbol.SymType          := Enum_Type;
      Member_Symbol.Value            := Value_Expr;

      A_Symbol_Table.Insert (Member_Symbol.Name, Member_Symbol);
   end New_Enum_Member_Symbol_Entry;

   --------------------------------
   -- New_Parameter_Symbol_Entry --
   --------------------------------

   procedure New_Parameter_Symbol_Entry (Name_Id :               Symbol_Id;
                                         BaseName :              String;
                                         Symbol_Type :           Irep;
                                         A_Symbol_Table : in out Symbol_Table)
   is
      New_Symbol : Symbol;
   begin
      New_Symbol.SymType := Symbol_Type;
      New_Symbol.Name := Name_Id;
      New_Symbol.PrettyName := Intern (BaseName);
      New_Symbol.BaseName := Intern (BaseName);
      New_Symbol.Mode := Intern ("C");

      --  Setting it as a parameter
      New_Symbol.IsParameter := True;
      New_Symbol.IsLValue := True;
      New_Symbol.IsFileLocal := True;
      New_Symbol.IsThreadLocal := True;

      if A_Symbol_Table.Contains (Key => Name_Id) then
         Put_Line (Standard_Error,
                   "----------At: New_Parameter_Symbol_Entry----------");
         Put_Line (Standard_Error,
                   "----------Trying to create known symbol.----------");
         Put_Line (Standard_Error, "----------" & BaseName & "----------");
      else
         A_Symbol_Table.Insert (Name_Id, New_Symbol);
      end if;
   end New_Parameter_Symbol_Entry;

   -------------------------------
   -- New_Function_Symbol_Entry --
   -------------------------------

   function New_Function_Symbol_Entry (Name : String; Symbol_Type : Irep;
                                       Value : Irep;
                                       A_Symbol_Table : in out Symbol_Table)
                                       return Symbol is
      New_Symbol : Symbol;
   begin
      New_Symbol.SymType := Symbol_Type;
      New_Symbol.Name := Intern (Name);
      New_Symbol.PrettyName := New_Symbol.Name;
      New_Symbol.BaseName := New_Symbol.Name;
      New_Symbol.Mode := Intern ("C");
      New_Symbol.Value := Value;

      if A_Symbol_Table.Contains (Key => Intern (Name)) then
         Put_Line (Standard_Error,
                   "----------At: New_Function_Symbol_Entry----------");
         Put_Line (Standard_Error,
                   "----------Trying to create known symbol.----------");
         Put_Line (Standard_Error, "----------" & Name & "----------");
      else
         A_Symbol_Table.Insert (Intern (Name), New_Symbol);
      end if;
      return New_Symbol;
   end New_Function_Symbol_Entry;

   --------------------------
   -- Create_Fun_Parameter --
   --------------------------

   --  To be called when one needs to build a function inside gnat2goto
   --  Modifies symbol table and Param_List as a side effect
   --  Returns irep of type I_Code_Parameter
   function Create_Fun_Parameter (Fun_Name : String; Param_Name : String;
                                  Param_Type : Irep; Param_List : Irep;
                                  A_Symbol_Table : in out Symbol_Table;
                                  Source_Location : Source_Ptr := No_Location)
                                  return Irep is
      Unique_Name : constant String :=
        Fun_Name & "::" & Fresh_Var_Name (Param_Name);
      Func_Param_Id : constant Symbol_Id := Intern (Unique_Name);
      --  Create an irep for the parameter
      Value_Arg : constant Irep :=
        Make_Code_Parameter (Source_Location => Source_Location,
                             Default_Value   => Ireps.Empty,
                             I_Type          => Param_Type,
                             Base_Name       => Unique_Name,
                             This            => False,
                             Identifier      => Unintern (Func_Param_Id));
   begin
      --  Creates a symbol for the parameter
      New_Parameter_Symbol_Entry (Name_Id        => Func_Param_Id,
                                  BaseName       => Unique_Name,
                                  Symbol_Type    => Param_Type,
                                  A_Symbol_Table => A_Symbol_Table);
      Append_Parameter (Param_List, Value_Arg);
      return Value_Arg;
   end Create_Fun_Parameter;

   function Compute_Memory_Op_Size (Num_Elem : Irep; Element_Type_Size : Uint;
                                    Source_Loc : Source_Ptr := No_Location)
                                    return Irep is

      Member_Size : constant Irep :=
        Make_Constant_Expr (Source_Location => Source_Loc,
                            I_Type          => CProver_Size_T,
                            Range_Check     => False,
                            Value           =>
                              --  bytes to bits (div by 8)
                       Convert_Uint_To_Hex (Value     => Element_Type_Size / 8,
                                            Bit_Width => 64));
   begin
      pragma Assert (Get_Type (Num_Elem) = CProver_Size_T);
      return Make_Op_Mul (Rhs             => Member_Size,
                          Lhs             => Num_Elem,
                          Source_Location => Source_Loc,
                          Overflow_Check  => False,
                          I_Type          => CProver_Size_T);
   end Compute_Memory_Op_Size;

   function Typecast_If_Necessary (Expr : Irep; New_Type : Irep;
                                   A_Symbol_Table : Symbol_Table) return Irep
   is
      Followed_Old_Type : constant Irep :=
        Follow_Symbol_Type (Get_Type (Expr), A_Symbol_Table);
      Followed_New_Type : constant Irep :=
        Follow_Symbol_Type (New_Type, A_Symbol_Table);
   begin
      if Followed_Old_Type = Followed_New_Type then
         return Expr;
      else
         return Make_Op_Typecast (Op0             => Expr,
                                 Source_Location => Get_Source_Location (Expr),
                                  I_Type          => New_Type);
      end if;
   end Typecast_If_Necessary;

   function Build_Function (Name : String; RType : Irep; Func_Params : Irep;
                            FBody : Irep; A_Symbol_Table : in out Symbol_Table)
                            return Symbol is
      Func_Name : constant String := Fresh_Var_Name (Name);
      Func_Type : constant Irep := Make_Code_Type (Parameters  => Func_Params,
                                                   Ellipsis    => False,
                                                   Return_Type => RType,
                                                   Inlined     => False,
                                                   Knr         => False);
   begin
      return New_Function_Symbol_Entry (Name        => Func_Name,
                                        Symbol_Type => Func_Type,
                                        Value       => FBody,
                                        A_Symbol_Table => A_Symbol_Table);
   end Build_Function;

   function Build_Identity_Body (Parameters : Irep) return Irep
   is
      Parameter_List : constant Irep_List := Get_Parameter (Parameters);
      First_Cursor : constant List_Cursor := List_First (Parameter_List);
      Body_Block : constant Irep :=
        Make_Code_Block (Source_Location => No_Location,
                         I_Type          => Make_Nil_Type);
   begin
      pragma Assert (List_Has_Element (Parameter_List, First_Cursor));
      Append_Op (Body_Block,
                 Make_Code_Return (Return_Value    =>
                    Param_Symbol (List_Element (Parameter_List, First_Cursor)),
                                   Source_Location => No_Location,
                                   I_Type          => Make_Nil_Type));
      return Body_Block;
   end Build_Identity_Body;

   function Build_Index_Constant (Value : Int; Source_Loc : Source_Ptr)
                                  return Irep
   is
   begin
      return Integer_Constant_To_Expr (UI_From_Int (Value), CProver_Size_T,
                                       Size_T_Width, Source_Loc);
   end Build_Index_Constant;

   function Build_Array_Size (First : Irep; Last : Irep) return Irep
   is
      Source_Loc : constant Source_Ptr := Get_Source_Location (First);
      Diff : constant Irep :=
        Make_Op_Sub (Rhs             => First,
                     Lhs             => Last,
                     Source_Location => Source_Loc,
                     Overflow_Check  => False,
                     I_Type          => CProver_Size_T);
      One : constant Irep :=
        Build_Index_Constant (Value      => 1,
                              Source_Loc => Source_Loc);
   begin
      return Make_Op_Add (Rhs             => One,
                          Lhs             => Diff,
                          Source_Location => Source_Loc,
                          Overflow_Check  => False,
                          I_Type          => CProver_Size_T);
   end Build_Array_Size;

   function Build_Array_Size (Array_Comp : Irep) return Irep
   is
      Source_Loc : constant Source_Ptr := Get_Source_Location (Array_Comp);
      First : constant Irep :=
        Make_Member_Expr (Compound         => Array_Comp,
                          Source_Location  => Source_Loc,
                          Component_Number => 0,
                          I_Type           => CProver_Size_T,
                          Component_Name   => "first1");
      Last : constant Irep :=
        Make_Member_Expr (Compound         => Array_Comp,
                          Source_Location  => Source_Loc,
                          Component_Number => 1,
                          I_Type           => CProver_Size_T,
                          Component_Name   => "last1");

   begin
      return Build_Array_Size (First      => First,
                               Last       => Last);
   end Build_Array_Size;

   function To_Float_Format (Float_Type : Irep) return Float_Format
   is
      Float_Width : constant Integer := Get_Width (Float_Type);
      Unsupported_Float_Width : exception;
   begin
      case Float_Width is
         when 32 => return IEEE_32_Bit;
         when 64 => return IEEE_64_Bit;
         when others =>
            raise Unsupported_Float_Width
              with "Unsupported float width: " &  Integer'Image (Float_Width);
      end case;
   end To_Float_Format;

   function Float_Mantissa_Size (Float_Type : Irep) return Integer
   is
      Format : constant Float_Format := To_Float_Format (Float_Type);
   begin
      case Format is
         when IEEE_32_Bit =>
            return 23;
         when IEEE_64_Bit =>
            return 52;
      end case;
   end Float_Mantissa_Size;

   ---------------------
   -- Name_Has_Prefix --
   ---------------------

   function Name_Has_Prefix (N : Node_Id; Prefix : String) return Boolean is
   begin
      if not Present (Name (N)) then
         return False;
      else
         declare
            Short_Name : constant String :=
              Get_Name_String (Chars (Name (N)));
         begin
            return Prefix'Length <= Short_Name'Length and then
              Prefix = Short_Name
                (Short_Name'First .. Short_Name'First - 1 + Prefix'Length);
         end;
      end if;
   end Name_Has_Prefix;

   ---------------------------
   -- Has_Nondet_Annotation --
   ---------------------------

   function Has_GNAT2goto_Annotation
     (Def_Id : Entity_Id;
      Annot  : String) return Boolean
   is
      Ent_Spec : constant Node_Id := Parent (Def_Id);
      Ent_Decl : constant Node_Id := Parent (Ent_Spec);

      function List_Contains_Annot (L : List_Id) return Boolean;

      --------------------------
      -- List_Contains_Annot --
      --------------------------

      function List_Contains_Annot (L : List_Id) return Boolean is
         E : Node_Id;

      begin
         if Is_Non_Empty_List (L) then
            E := First (L);
            if Nkind (E) = N_Identifier and then
              Get_Name_String (Chars (E)) = "gnat2goto"
            then
               Next (E);
               return Present (E) and then
                 Nkind (E) = N_Identifier and then
                 Get_Name_String (Chars (E)) = Annot;
            end if;
         end if;
         return False;
      end List_Contains_Annot;

   begin
      if Has_Aspects (Ent_Decl) then
         declare
            Asp : constant Node_Id := Find_Aspect (Def_Id, Aspect_Annotate);
            Expr : Node_Id;
         begin
            if Present (Asp) and then Present (Expression (Asp)) then
               Expr := Expression (Asp);
               return Present (Expressions (Expr)) and then
                 List_Contains_Annot (Expressions (Expr));
            end if;
         end;
      end if;
      --  TODO: handle annotations through pragma
      return False;
   end Has_GNAT2goto_Annotation;

   function Integer_Constant_To_BV_Expr
     (Value : Uint;
      Expr_Type : Irep;
      Source_Location : Source_Ptr)
      return Irep is
   begin
      return Integer_Constant_To_Expr (Value, Expr_Type, Get_Width (Expr_Type),
                                       Source_Location);
   end Integer_Constant_To_BV_Expr;

   function Integer_Constant_To_Expr
     (Value : Uint;
      Expr_Type : Irep;
      Type_Width : Integer;
      Source_Location : Source_Ptr)
   return Irep is
      Value_Hex : constant String := Convert_Uint_To_Hex
        (Value => Value,
         Bit_Width => Pos (Type_Width));
   begin
      return Make_Constant_Expr
        (Source_Location => Source_Location,
         I_Type => Expr_Type,
         Value => Value_Hex);
   end Integer_Constant_To_Expr;

   function Make_Simple_Side_Effect_Expr_Function_Call
     (Arguments : Irep_Array;
      Function_Expr : Irep;
      Source_Location : Source_Ptr) return Irep is
      Argument_List : constant Irep := Make_Argument_List;
   begin
      for Argument of Arguments loop
         Append_Argument (Argument_List, Argument);
      end loop;
      return Make_Side_Effect_Expr_Function_Call
        (Arguments => Argument_List,
         I_Function => Function_Expr,
         I_Type => Get_Return_Type
           (Get_Type (Function_Expr)),
         Source_Location => Source_Location);
   end Make_Simple_Side_Effect_Expr_Function_Call;

   procedure Register_Identifier_In_Symbol_Table
      (N : Irep; Val : Irep; Symtab : in out Symbol_Table) is
      Identifier_Name : constant Symbol_Id :=
         Intern (Get_Identifier (N));
      Identifier_Symbol : Symbol;
   begin
      Identifier_Symbol.Name       := Identifier_Name;
      Identifier_Symbol.BaseName   := Identifier_Name;
      Identifier_Symbol.PrettyName := Identifier_Name;
      Identifier_Symbol.SymType    := Get_Type (N);
      Identifier_Symbol.IsLValue   := True;
      Identifier_Symbol.IsStaticLifetime := True;
      Identifier_Symbol.Mode       := Intern ("C");
      Identifier_Symbol.Value      := Val;
      Symtab.Insert (Identifier_Name, Identifier_Symbol);
   end Register_Identifier_In_Symbol_Table;
end GOTO_Utils;
