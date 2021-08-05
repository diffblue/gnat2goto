with Namet;   use Namet;
with Nlists;  use Nlists;
with Aspects; use Aspects;
with Binary_To_Hex;         use Binary_To_Hex;

with Ada.Text_IO;           use Ada.Text_IO;
with Follow; use Follow;
with Ada.Strings;
with Ada.Strings.Fixed;
with Sinput; use Sinput;

package body GOTO_Utils is

   --  Irep constants
   --    These are constants for Ireps that we use over and over again
   --    having these saves us some typing, and also a bit of
   --    performance/memory.

   Size_T : Irep := Ireps.Empty;
   function CProver_Size_T return Irep
   is
   begin
      if Size_T = Ireps.Empty then
         Size_T := Make_Symbol_Type ("__CPROVER_size_t");
      end if;
      return Size_T;
   end CProver_Size_T;

   Void_T : Irep := Ireps.Empty;
   function CProver_Void_T return Irep
   is
   begin
      if Void_T = Ireps.Empty then
         Void_T := Make_Void_Type;
      end if;
      return Void_T;
   end CProver_Void_T;

   Nil_T : Irep := Ireps.Empty;
   function CProver_Nil_T return Irep
   is
   begin
      if Nil_T = Ireps.Empty then
         Nil_T := Make_Nil_Type;
      end if;
      return Nil_T;
   end CProver_Nil_T;

   Bool_T : Irep := Ireps.Empty;
   function CProver_Bool_T return Irep
   is
   begin
      if Bool_T = Ireps.Empty then
         Bool_T := Make_Bool_Type;
      end if;
      return Bool_T;
   end CProver_Bool_T;

   Nil : Irep := Ireps.Empty;
   function CProver_Nil return Irep
   is
   begin
      if Nil = Ireps.Empty then
         Nil := Make_Nil (Internal_Source_Location);
      end if;
      return Nil;
   end CProver_Nil;

   True_V : Irep := Ireps.Empty;
   function CProver_True return Irep
   is
   begin
      if True_V = Ireps.Empty then
         True_V := Make_Constant_Expr
           (Source_Location => Internal_Source_Location,
            I_Type => CProver_Bool_T,
            Value => "true");
      end if;
      return True_V;
   end CProver_True;

   --  ToDo - why does this produce a corrupt Internal source location
   --  if it is not generated anew each time it is called?
   --  Is something corrupting this string somehow?
   Internal_Source_Location_V : Irep := Ireps.Empty;
   function Internal_Source_Location return Irep is
   begin
--        if Internal_Source_Location_V = Ireps.Empty then
         Internal_Source_Location_V := Make_Source_Location
           (File => "<internal>",
            Line => "",
            Column => "",
            Comment => "");
--        end if;
      return Internal_Source_Location_V;
   end Internal_Source_Location;

   Float_T : Irep := Ireps.Empty;
   function Float32_T return Irep
   is
   begin
      if Float_T = Ireps.Empty then
         Float_T := Make_Floatbv_Type (Width => 32,
                                       F     => 23);
      end if;
      return Float_T;
   end Float32_T;

   Double_T : Irep := Ireps.Empty;
   function Float64_T return Irep
   is
   begin
      if Double_T = Ireps.Empty then
         Double_T := Make_Floatbv_Type (Width => 64,
                                        F     => 52);
      end if;
      return Double_T;
   end Float64_T;

   Int_T : Irep := Ireps.Empty;
   function Int32_T return Irep
   is
   begin
      if Int_T = Ireps.Empty then
         Int_T := Make_Signedbv_Type (32);
      end if;
      return Int_T;
   end Int32_T;

   Long_T : Irep := Ireps.Empty;
   function Int64_T return Irep
   is
   begin
      if Long_T = Ireps.Empty then
         Long_T := Make_Signedbv_Type (64);
      end if;
      return Long_T;
   end Int64_T;

   Char_T : Irep := Ireps.Empty;
   function Int8_T return Irep
   is
   begin
      if Char_T = Ireps.Empty then
         Char_T := Make_Signedbv_Type (8);
      end if;
      return Char_T;
   end Int8_T;

   Unsigned_T : Irep := Ireps.Empty;
   function Uint32_T return Irep
   is
   begin
      if Unsigned_T = Ireps.Empty then
         Unsigned_T := Make_Unsignedbv_Type (32);
      end if;
      return Unsigned_T;
   end Uint32_T;

   Unsigned_Long_T : Irep := Ireps.Empty;
   function Uint64_T return Irep
   is
   begin
      if Unsigned_Long_T = Ireps.Empty then
         Unsigned_Long_T := Make_Unsignedbv_Type (64);
      end if;
      return Unsigned_Long_T;
   end Uint64_T;

   function Maybe_Double_Type_Width (Original_Type : Irep) return Irep
   is
   begin
      if Get_Width (Original_Type) /= 32 then
         return Original_Type;
      end if;

      if Original_Type = Int32_T or
        Kind (Original_Type) = I_Signedbv_Type
      then
         return Int64_T;
      end if;
      if Original_Type = Float32_T or
        Kind (Original_Type) = I_Floatbv_Type
      then
         return Float64_T;
      end if;
      if Original_Type = Uint32_T or
        Kind (Original_Type) = I_Unsignedbv_Type
      then
         return Uint64_T;
      end if;
      return Original_Type;
   end Maybe_Double_Type_Width;

   False_V : Irep := Ireps.Empty;
   function CProver_False return Irep
   is
   begin
      if False_V = Ireps.Empty then
         False_V := Make_Constant_Expr
           (Source_Location => Internal_Source_Location,
            I_Type => CProver_Bool_T,
            Value => "false");
      end if;
      return False_V;
   end CProver_False;

   ---------------------
   -- Make_Address_Of --
   ---------------------

   function Make_Address_Of (Base : Irep) return Irep is
      (Make_Address_Of_Expr
         (Object => Base,
          I_Type => Make_Pointer_Type (Get_Type (Base)),
          Source_Location => Get_Source_Location (Base)));

   -----------------------
   -- Make_Pointer_Type --
   -----------------------

   function Make_Pointer_Type (Base : Irep) return Irep is
      (Make_Pointer_Type
         (I_Subtype => Base,
          Width => Pointer_Type_Width));

   function Make_Array_String_Type (Size : Integer) return Irep is
   begin
      return Make_Array_Type
        (I_Subtype => Int8_T,
         Size      => Make_Constant_Expr
           (Source_Location => Internal_Source_Location,
            I_Type          => Uint32_T,
            Range_Check     => False,
            Value           => Convert_Uint_To_Hex
              (UI_From_Int (Int (Size)),
               32)));
   end Make_Array_String_Type;

   function Make_Type_For_String (Text : String) return Irep is
   begin
      return Make_Array_String_Type (Text'Length);
   end Make_Type_For_String;

   function Make_String_Constant_Expr (Text : String; Source_Loc : Irep)
                                       return Irep is
      String_Type : constant Irep := Make_Type_For_String (Text);
   begin
      return Make_String_Constant_Expr (Source_Location => Source_Loc,
                                        I_Type          => String_Type,
                                        Range_Check     => False,
                                        Value           => Text);
   end Make_String_Constant_Expr;

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
      (Make_Symbol_Expr
         (Identifier => Fresh_Var_Name (Infix),
          I_Type => Ty,
          Source_Location => Internal_Source_Location));

   ------------------
   -- Param_Symbol --
   ------------------

   function Param_Symbol (Param : Irep) return Irep is
      (Make_Symbol_Expr
         (Identifier => Get_Identifier (Param),
          I_Type => Get_Type (Param),
          Source_Location => Get_Source_Location (Param)));

   -----------------
   -- Symbol_Expr --
   -----------------

   function Symbol_Expr (Sym : Symbol) return Irep is
      (Make_Symbol_Expr
        (Identifier => Unintern (Sym.Name),
         I_Type => Sym.SymType,
       -- TODO Should be Sym.Location, but that depends
       --      on the "change source location to irep" PR
         Source_Location => Internal_Source_Location));

   procedure New_Object_Symbol_Entry (Object_Name : Symbol_Id;
                                      Object_Type : Irep;
                                      Object_Init_Value : Irep;
                                      A_Symbol_Table : in out Symbol_Table)
   is
      Object_Symbol : constant Symbol :=
        (Name | BaseName | PrettyName => Object_Name,
         SymType => Object_Type,
         Mode => Intern ("C"),
         Value => Object_Init_Value,
         IsLValue => True,
         others => <>);
   begin
      --  The symbol may be in the table if it is a derived type declaration.
      if not A_Symbol_Table.Contains (Object_Name) then
         A_Symbol_Table.Insert (Object_Name, Object_Symbol);
      end if;
   end New_Object_Symbol_Entry;

   procedure New_Subprogram_Symbol_Entry (Subprog_Name : Symbol_Id;
                                          Subprog_Type : Irep;
                                          A_Symbol_Table : in out Symbol_Table)
   is
      Subprog_Symbol : constant Symbol :=
        (Name | BaseName | PrettyName => Subprog_Name,
         SymType => Subprog_Type,
         Mode => Intern ("C"),
         Value => CProver_Nil,
         others => <>);
   begin
      pragma Assert (not A_Symbol_Table.Contains (Subprog_Name));
      A_Symbol_Table.Insert (Subprog_Name, Subprog_Symbol);
   end New_Subprogram_Symbol_Entry;

   procedure New_Type_Symbol_Entry (Type_Name : Symbol_Id; Type_Of_Type : Irep;
                                    A_Symbol_Table : in out Symbol_Table) is
      Type_Symbol : constant Symbol :=
        (Name | BaseName | PrettyName => Type_Name,
         SymType => Type_Of_Type,
         Mode => Intern ("C"),
         IsType => True,
         others => <>);
   begin
      pragma Assert (not A_Symbol_Table.Contains (Type_Name));
      A_Symbol_Table.Insert (Type_Name, Type_Symbol);
   end New_Type_Symbol_Entry;

   procedure New_Valueless_Object_Symbol_Entry (Constant_Name : Symbol_Id;
                                        A_Symbol_Table : in out Symbol_Table)
   is
      Object_Symbol : constant Symbol :=
        (Name | BaseName | PrettyName => Constant_Name,
         SymType => CProver_Nil_T,
         Mode => Intern ("C"),
         Value => CProver_Nil,
         others => <>);
   begin
      pragma Assert (not A_Symbol_Table.Contains (Constant_Name));
      A_Symbol_Table.Insert (Constant_Name, Object_Symbol);
   end New_Valueless_Object_Symbol_Entry;

   procedure New_Enum_Member_Symbol_Entry (
      Member_Name : Symbol_Id; Base_Name : Symbol_Id; Enum_Type : Irep;
      Value_Expr : Irep; A_Symbol_Table : in out Symbol_Table) is
      Member_Symbol : constant Symbol :=
        (Name => Member_Name,
         BaseName | PrettyName => Base_Name,
         Mode => Intern ("C"),
         SymType => Enum_Type,
         Value => Value_Expr,
         IsStaticLifetime => True,
         IsStateVar => True,
         others => <>);
   begin
      pragma Assert (not A_Symbol_Table.Contains (Member_Symbol.Name));
      A_Symbol_Table.Insert (Member_Symbol.Name, Member_Symbol);
   end New_Enum_Member_Symbol_Entry;

   --------------------------------
   -- New_Parameter_Symbol_Entry --
   --------------------------------

   procedure New_Parameter_Symbol_Entry (Name_Id        :        Symbol_Id;
                                         BaseName       :        String;
                                         Symbol_Type    :        Irep;
                                         A_Symbol_Table : in out Symbol_Table)
   is
      New_Symbol : constant Symbol :=
        (Name => Name_Id,
         BaseName | PrettyName => Intern (BaseName),
         Mode => Intern ("C"),
         SymType => Symbol_Type,
         IsParameter | IsLValue | IsFileLocal | IsThreadLocal => True,
         others => <>);
   begin
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
      Fun_Name : constant Symbol_Id := Intern (Name);
   begin
      if A_Symbol_Table.Contains (Fun_Name) then
         return A_Symbol_Table.Element (Fun_Name);
      end if;

      declare
         New_Symbol : constant Symbol :=
           (Name | BaseName | PrettyName => Intern (Name),
            Mode => Intern ("C"),
            SymType => Symbol_Type,
            Value => Value,
            others => <>);
      begin
         pragma Assert (not A_Symbol_Table.Contains (Fun_Name));
         A_Symbol_Table.Insert (Fun_Name, New_Symbol);
         return New_Symbol;
      end;
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
                                  Source_Location : Irep :=
                                    Internal_Source_Location)
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

   --  To be called when one needs to build a function inside gnat2goto
   --  Modifies symbol table and Param_List as a side effect
   --  Returns irep of type I_Code_Parameter
   procedure Create_Fun_Parameter (Fun_Name : String; Param_Name : String;
                                  Param_Type : Irep; Param_List : Irep;
                                  A_Symbol_Table : in out Symbol_Table;
                            Source_Location : Irep := Internal_Source_Location)
   is
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
   end Create_Fun_Parameter;

   function Compute_Memory_Op_Size (Num_Elem : Irep; Element_Type_Size : Uint;
                                    Source_Loc : Irep :=
                                      Internal_Source_Location)
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
         return Make_Op_Typecast
           (Op0             => Expr,
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

   function Build_Index_Constant (Value : Int; Source_Loc : Irep)
                                  return Irep
   is
      Value_Hex : constant String :=
        Convert_Uint_To_Hex (Value     => UI_From_Int (Value),
                             Bit_Width => Size_T_Width);
   begin
      return Make_Constant_Expr (Source_Location => Source_Loc,
                                 I_Type          => CProver_Size_T,
                                 Range_Check     => False,
                                 Value           => Value_Hex);
   end Build_Index_Constant;

   function Build_Array_Size (First : Irep; Last : Irep) return Irep
   is
      Source_Loc : constant Irep := Get_Source_Location (First);
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
      Source_Loc : constant Irep := Get_Source_Location (Array_Comp);
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

   function To_Float_Format (Float_Width : Integer) return Float_Format
   is
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

   function Float_Mantissa_Size (Float_Width : Integer) return Integer
   is
      Format : constant Float_Format := To_Float_Format (Float_Width);
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

   function Integer_Constant_To_Expr
     (Value : Uint;
      Expr_Type : Irep;
      Source_Location : Irep)
   return Irep is
      Value_Hex : constant String := Convert_Uint_To_Hex
        (Value => Value,
         Bit_Width => Pos (Get_Width (Expr_Type)));
   begin
      return Make_Constant_Expr
        (Source_Location => Source_Location,
         I_Type => Expr_Type,
         Value => Value_Hex);
   end Integer_Constant_To_Expr;

   function Make_Simple_Side_Effect_Expr_Function_Call
     (Arguments : Irep_Array;
      Function_Expr : Irep;
      Source_Location : Irep) return Irep is
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
      Identifier_Symbol : constant Symbol :=
        (Name | BaseName | PrettyName => Identifier_Name,
         SymType => Get_Type (N),
         Mode => Intern ("C"),
         Value => Val,
         IsLValue | IsStaticLifetime => True,
         others => <>);
   begin
      pragma Assert (not Symtab.Contains (Identifier_Name));
      Symtab.Insert (Identifier_Name, Identifier_Symbol);
   end Register_Identifier_In_Symbol_Table;

   function Cast_Enum (Expr : Irep; A_Symbol_Table : Symbol_Table) return Irep
   is
   begin
      if Kind (Follow_Symbol_Type (Get_Type (Expr),
               A_Symbol_Table)) = I_C_Enum_Type
      then
         return Typecast_If_Necessary (Expr, Int32_T, A_Symbol_Table);
      else
         return Expr;
      end if;
   end Cast_Enum;

   function Make_Code_Type
     (Parameters : Irep;
      Return_Type : Irep)
     return Irep
   is
      (Make_Code_Type
         (Parameters => Parameters,
          Return_Type => Return_Type,
          Ellipsis => False,
          Inlined => False,
          Knr => False));

   function Source_Ptr_To_Irep (Src : Source_Ptr) return Irep
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      SFI : constant Source_File_Index := Get_Source_File_Index (Src);
      FN : constant File_Name_Type := File_Name (SFI);
   begin
      if Src = No_Location or Src <= Standard_Location
      then
         return Internal_Source_Location;
      else
         return Make_Source_Location
           (File => Get_Name_String (FN),
            Line => Trim
              (Logical_Line_Number'Image
                 (Get_Logical_Line_Number (Src)), Both),
            Column => Trim
              (Column_Number'Image (Get_Column_Number (Src)), Both),
            Comment => "");
      end if;
   end Source_Ptr_To_Irep;

   function Get_Source_Location (N : Node_Id) return Irep
   is (Source_Ptr_To_Irep (Sloc (N)));

   function Make_Source_Location
     (Comment : String := "";
      Working_Directory : String := "";
      File : String := "";
      Property_Id : String := "";
      I_Function : String := "";
      Property_Class : String := "";
      Line : String := "";
      Column : String := "")
     return Irep
   is
      (Make_Source_Location
         (Comment => Comment,
          Working_Directory => Working_Directory,
          File => File,
          Property_Id => Property_Id,
          Java_Bytecode_Index => "",
          I_Function => I_Function,
          Property_Class => Property_Class,
          Line => Line,
          Column => Column));

   function Make_Assert_Call (Assertion : Irep;
                              Description : Irep; Source_Loc : Irep;
                              A_Symbol_Table : in out Symbol_Table)
                              return Irep is
      Assert_Param_List : constant Irep := Make_Parameter_List;
      Assert_Name : constant String := "__CPROVER_assert";
      Sym_Assert : constant Irep :=
        Make_Symbol_Expr (Source_Location => Source_Loc,
                          I_Type          =>
                            Make_Code_Type (Parameters  => Assert_Param_List,
                                            Ellipsis    => False,
                                            Return_Type => Make_Void_Type,
                                            Inlined     => False,
                                            Knr         => False),
                          Range_Check     => False,
                          Identifier      => Assert_Name);
      Assert_Args  : constant Irep := Make_Argument_List;
   begin
      Create_Fun_Parameter (Fun_Name        => Assert_Name,
                            Param_Name      => "condition",
                            Param_Type      => CProver_Bool_T,
                            Param_List      => Assert_Param_List,
                            A_Symbol_Table  => A_Symbol_Table,
                            Source_Location => Source_Loc);
      Create_Fun_Parameter (Fun_Name        => Assert_Name,
                            Param_Name      => "comment",
                            Param_Type      => Make_Pointer_Type (Int8_T),
                            Param_List      => Assert_Param_List,
                            A_Symbol_Table  => A_Symbol_Table,
                            Source_Location => Source_Loc);

      Append_Argument (Assert_Args, Assertion);
      Append_Argument (Assert_Args, Description);

      return Make_Code_Function_Call (Arguments       => Assert_Args,
                                      I_Function      => Sym_Assert,
                                      Lhs             => Make_Nil (Source_Loc),
                                      Source_Location => Source_Loc,
                                      I_Type          => Make_Void_Type,
                                      Range_Check     => False);
   end Make_Assert_Call;

   function Make_Assume_Call (Assumption : Irep; Source_Loc : Irep;
                             A_Symbol_Table : in out Symbol_Table)
                              return Irep is
      Assume_Param_List : constant Irep := Make_Parameter_List;
      Assume_Name : constant String := "__CPROVER_assume";
      Sym_Assume : constant Irep :=
        Make_Symbol_Expr (Source_Location => Source_Loc,
                          I_Type          =>
                            Make_Code_Type (Parameters  => Assume_Param_List,
                                            Ellipsis    => False,
                                            Return_Type => Make_Void_Type,
                                            Inlined     => False,
                                            Knr         => False),
                          Range_Check     => False,
                          Identifier      => Assume_Name);
      Assume_Args  : constant Irep := Make_Argument_List;
   begin
      Create_Fun_Parameter (Fun_Name        => Assume_Name,
                            Param_Name      => "condition",
                            Param_Type      => CProver_Bool_T,
                            Param_List      => Assume_Param_List,
                            A_Symbol_Table  => A_Symbol_Table,
                            Source_Location => Source_Loc);

      Append_Argument (Assume_Args, Assumption);

      return Make_Code_Function_Call (Arguments       => Assume_Args,
                                      I_Function      => Sym_Assume,
                                      Lhs             => Make_Nil (Source_Loc),
                                      Source_Location => Source_Loc,
                                      I_Type          => Make_Void_Type,
                                      Range_Check     => False);
   end Make_Assume_Call;

   function Get_Int32_T_Zero return Irep
   is
   begin
      return Integer_Constant_To_Expr
        (Value           => Uint_0,
         Expr_Type       => Int32_T,
         Source_Location => Internal_Source_Location);
   end Get_Int32_T_Zero;

   function Get_Int64_T_Zero return Irep
   is
   begin
      return Integer_Constant_To_Expr
        (Value           => Uint_0,
         Expr_Type       => Int64_T,
         Source_Location => Internal_Source_Location);
   end Get_Int64_T_Zero;

   function Get_Int32_T_One return Irep
   is
   begin
      return Integer_Constant_To_Expr
        (Value           => Uint_1,
         Expr_Type       => Int32_T,
         Source_Location => Internal_Source_Location);
   end Get_Int32_T_One;

   function Get_Int64_T_One return Irep
   is
   begin
      return Integer_Constant_To_Expr
        (Value           => Uint_1,
         Expr_Type       => Int64_T,
         Source_Location => Internal_Source_Location);
   end Get_Int64_T_One;

   function Get_Ada_Check_Symbol (Name : String;
                                  A_Symbol_Table : in out Symbol_Table;
                                  Source_Loc : Irep)
                                  return Symbol is
   begin
      if A_Symbol_Table.Contains (Intern (Name)) then
         return A_Symbol_Table.Element (Intern (Name));
      else
         declare
            Func_Params : constant Irep := Make_Parameter_List;
            Expr_Param : constant Irep := Create_Fun_Parameter
              (Fun_Name        => Name,
               Param_Name      => "expr",
               Param_Type      => Int32_T,
               Param_List      => Func_Params,
               A_Symbol_Table  => A_Symbol_Table,
               Source_Location => Source_Loc);
            Func_Type : constant Irep :=
              Make_Code_Type (Parameters  => Func_Params,
                              Ellipsis    => False,
                              Return_Type => Make_Void_Type,
                              Inlined     => False,
                              Knr         => False);
            Default_Body : constant Irep :=
              Make_Code_Block (Source_Loc);
            Assert_Comment : constant Irep := Make_String_Constant_Expr
              (Source_Location => Source_Loc,
               I_Type          => Ireps.Empty,
               Range_Check     => False,
               Value           => "Ada Check assertion");
            Expr_Neq_Zero : constant Irep := Make_Op_Notequal
              (Rhs             => Get_Int32_T_Zero,
               Lhs             => Param_Symbol (Expr_Param),
               Source_Location => Source_Loc,
               Overflow_Check  => False,
               I_Type          => Make_Bool_Type,
               Range_Check     => False);
         begin
            Append_Op (Default_Body,
                       Make_Assert_Call
                         (Assertion   => Expr_Neq_Zero,
                          Description => Assert_Comment,
                          Source_Loc  => Source_Loc,
                          A_Symbol_Table => A_Symbol_Table));
            Append_Op (Default_Body,
                       Make_Assume_Call
                         (Assumption   => Expr_Neq_Zero,
                          Source_Loc   => Source_Loc,
                          A_Symbol_Table => A_Symbol_Table));
            return New_Function_Symbol_Entry
              (Name           => Name,
               Symbol_Type    => Func_Type,
               Value          => Default_Body,
               A_Symbol_Table => A_Symbol_Table);
         end;
      end if;
   end Get_Ada_Check_Symbol;

   function File_Name_Without_Extension (N : Node_Id) return String
   is
      Full_File_Name : constant String := Get_File (Get_Source_Location (N));
      Dot_Index : constant Natural := Ada.Strings.Fixed.Index
        (Source  => Full_File_Name,
         Pattern => ".");
   begin
      return Ada.Strings.Fixed.Head (Source => Full_File_Name,
                                     Count  => Dot_Index);
   end File_Name_Without_Extension;

   function String_To_Char_Pointer (String_Irep : Irep;
                                    A_Symbol_Table : Symbol_Table)
                                    return Irep
   is
      Source_Loc : constant Irep := Get_Source_Location (String_Irep);
      Size_T_Zero : constant Irep :=
        Integer_Constant_To_Expr (Value           => Uint_0,
                                  Expr_Type       => Uint32_T,
                                  Source_Location => Source_Loc);
      Index_Expr : constant Irep :=
        Make_Index_Expr (I_Array         => String_Irep,
                         Index           => Size_T_Zero,
                         Source_Location => Source_Loc,
                         I_Type          => Int8_T);
      Char_Pointer_Type : constant Irep := Make_Pointer_Type (Int8_T);
      Address_Expr : constant Irep :=
        Make_Address_Of_Expr (Object          => Index_Expr,
                              Source_Location => Source_Loc,
                              I_Type          => Char_Pointer_Type);
   begin
      return Typecast_If_Necessary (Expr           => Address_Expr,
                                    New_Type       => Char_Pointer_Type,
                                    A_Symbol_Table => A_Symbol_Table);
   end String_To_Char_Pointer;

   function Make_Let_Binding_Expr (
      Symbol : Irep;
      Value : Irep;
      Where : Irep;
      Source_Location : Irep;
      Overflow_Check : Boolean := False;
      I_Type : Irep := Ireps.Empty;
      Range_Check : Boolean := False
   ) return Irep
   is
      Variable_Tuple : constant Irep := Make_Tuple_Expr (
        I_Type => Make_Tuple_Type,
        Source_Location => Source_Location);
      Value_Tuple : constant Irep := Make_Tuple_Expr (
        I_Type => Make_Tuple_Type,
        Source_Location => Source_Location);
   begin
      Append_Op (Variable_Tuple, Symbol);
      Append_Op (Value_Tuple, Value);
      return Make_Let_Expr (Lhs => Make_Let_Binding (
                                    Lhs => Variable_Tuple,
                                    Rhs => Where,
                                    Source_Location => Source_Location),
                            Rhs => Value_Tuple,
                            Source_Location => Source_Location,
                            Overflow_Check => Overflow_Check,
                            I_Type => I_Type,
                            Range_Check => Range_Check);
   end Make_Let_Binding_Expr;

   function Get_Context_Name (Intermediate_Node : Node_Id)
                              return String is
   begin
      if not (Present (Intermediate_Node)) then
         return "";
      end if;
      case Nkind (Intermediate_Node) is
         when N_Subprogram_Body =>
            return Get_Name_String
              (Chars
                 (Defining_Unit_Name
                      (Specification
                           (Intermediate_Node))));
         when N_Package_Body =>
            return Get_Name_String
              (Chars
                 (Defining_Unit_Name
                      (Intermediate_Node)));
         when N_Pragma =>
            if Get_Name_String
              (Chars (Pragma_Identifier (Intermediate_Node))) =
              "postcondition"
              or else
                Get_Name_String
                  (Chars (Pragma_Identifier (Intermediate_Node))) =
              "precondition"
            then
               if Nkind (Parent (Corresponding_Aspect
                         (Intermediate_Node))) in N_Entry_Declaration |
               N_Generic_Package_Declaration  |
               N_Generic_Subprogram_Declaration |
               N_Package_Body_Stub |
               N_Package_Declaration |
               N_Protected_Body_Stub |
               N_Protected_Type_Declaration |
               N_Subprogram_Body_Stub |
               N_Subprogram_Declaration |
               N_Task_Body_Stub |
               N_Task_Type_Declaration
               then
                  return Get_Context_Name
                    (Corresponding_Body
                       (Parent (Corresponding_Aspect
                        (Intermediate_Node))));
               else
                  return Get_Context_Name (Parent (Intermediate_Node));
               end if;
            else
               return Get_Context_Name (Parent (Intermediate_Node));
            end if;
         when others =>
            return Get_Context_Name (Parent (Intermediate_Node));
      end case;
   end Get_Context_Name;

   function Type_To_String (Type_Irep : Irep) return String is
      Type_Kind : constant Irep_Kind := Kind (Type_Irep);
   begin
      if Type_Kind in Class_Bitvector_Type
      then
         --  This does not distinguish between instances of 2D
         --  types such as floats, that have the same total width.
         --  I don't think it is possible to create these from Ada.
         return Id (Type_Irep) & "_" &
           Ada.Strings.Fixed.Trim (Get_Width (Type_Irep)'Image,
                                   Ada.Strings.Left);
         --  The trim is for a lead space where the sign would be.
      elsif Type_Kind = I_Struct_Type or
        Type_Kind = I_Union_Type or
        Type_Kind = I_Class_Type
      then
         return Id (Type_Irep) & "_" & Get_Tag (Type_Irep);

      elsif Type_Kind = I_Enumeration_Type
      then
         return Id (Type_Irep) & "_" &
           Type_To_String (Get_Elements (Type_Irep));

      elsif Type_Kind = I_Array_Type or Type_Kind = I_Array_Type or
        Type_Kind = I_C_Enum_Type or Type_Kind = I_Complex_Type or
        Type_Kind = I_Incomplete_Array_Type or Type_Kind = I_Pointer_Type or
        Type_Kind = I_Reference_Type or Type_Kind = I_Vector_Type
      then
         return Id (Type_Irep) & "_" &
           Type_To_String (Get_Subtype (Type_Irep));

      else
         --  The default case, should be sufficient
         return Id (Type_Irep);
      end if;
   end Type_To_String;

   function Non_Private_Ekind (E : Entity_Id) return Entity_Kind is
     (Ekind (Non_Private_Type (E)));

   function Non_Private_Type (E : Entity_Id) return Entity_Id is
     (if Ekind (E) in Incomplete_Or_Private_Kind then
           Non_Private_Type (Full_View (E))
      else
         E);

   function Cast_To_Max_Width (May_Be_Cast, Model : Irep) return Irep is
      Expr_Type  : constant Irep := Get_Type (May_Be_Cast);
      Model_Type : constant Irep :=
        (if Kind (Model) in Class_Type then
              Model
         else
            Get_Type (Model));
      pragma Assert (Kind (Expr_Type) = Kind (Model_Type));
   begin
      return
        (if Kind (Model_Type) in Class_Bitvector_Type and then
         Get_Width (Model_Type) > Get_Width (Expr_Type)
         then
            Make_Op_Typecast
           (Op0             => May_Be_Cast,
            Source_Location => Get_Source_Location (May_Be_Cast),
            I_Type          => Model_Type)
         else
            May_Be_Cast);
   end Cast_To_Max_Width;

   function Make_Corresponding_Unbounded_Type (I_Type : Irep) return Irep is
      I_Kind : constant Irep_Kind := Kind (I_Type);
      Width  : constant Integer :=
        (if I_Kind in Class_Bitvector_Type then
            Get_Width (I_Type)
         else
            0);
   begin
      return
        (case I_Kind is
            when I_Bounded_Unsignedbv_Type => Make_Unsignedbv_Type (Width),
            when I_Bounded_Signedbv_Type   => Make_Signedbv_Type (Width),
            when I_Bounded_Floatbv_Type    =>
               Make_Floatbv_Type (Width, Get_F (I_Type)),
            when others => I_Type);
   end Make_Corresponding_Unbounded_Type;

   function Strip_Irep_Bounds (I_Expr : Irep) return Irep is
      Stripped_Type : constant Irep := Get_Type (I_Expr);
   begin
      return
        (if Kind (Stripped_Type) in I_Bounded_Unsignedbv_Type
             | I_Bounded_Signedbv_Type | I_Bounded_Floatbv_Type
         then
            Make_Op_Typecast
           (Op0             => I_Expr,
            Source_Location => Get_Source_Location (I_Expr),
            I_Type          =>
              Make_Corresponding_Unbounded_Type (Stripped_Type))
         else
            I_Expr);
   end Strip_Irep_Bounds;

end GOTO_Utils;
