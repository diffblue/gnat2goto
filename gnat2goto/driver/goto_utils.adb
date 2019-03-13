with Namet;   use Namet;
with Nlists;  use Nlists;
with Aspects; use Aspects;
with Binary_To_Hex;         use Binary_To_Hex;

with Ada.Text_IO;           use Ada.Text_IO;

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

   -------------------
   -- Make_Int_Type --
   -------------------

   function Make_Int_Type (Width : Positive) return Irep is
      I : constant Irep := New_Irep (I_Signedbv_Type);
   begin
      Set_Width (I, Width);
      return I;
   end Make_Int_Type;

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
      return Make_Op_Mul (Rhs             => Member_Size,
                          Lhs             =>
                            Typecast_If_Necessary (Expr      => Num_Elem,
                                            New_Type => CProver_Size_T),
                          Source_Location => Source_Loc,
                          Overflow_Check  => False,
                          I_Type          => CProver_Size_T);
   end Compute_Memory_Op_Size;

   function Typecast_If_Necessary (Expr : Irep; New_Type : Irep) return Irep
   is
   begin
      if Get_Type (Expr) = New_Type then
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

   function Build_Index_Constant (Value : Int; Index_Type : Irep;
                                  Source_Loc : Source_Ptr) return Irep
   is
      Type_Width : constant Int :=
        (if not (Kind (Index_Type) in Class_Bitvector_Type)
         then 32
         else Int (Get_Width (Index_Type)));
      Value_Hex : constant String :=
        Convert_Uint_To_Hex (Value     => UI_From_Int (Value),
                             Bit_Width => Type_Width);
   begin
      return Make_Constant_Expr (Source_Location => Source_Loc,
                                 I_Type          => Index_Type,
                                 Range_Check     => False,
                                 Value           => Value_Hex);
   end Build_Index_Constant;

   function Build_Array_Size (First : Irep; Last : Irep; Idx_Type : Irep)
                              return Irep
   is
      Source_Loc : constant Source_Ptr := Get_Source_Location (First);
      Diff : constant Irep :=
        Make_Op_Sub (Rhs             => First,
                     Lhs             => Last,
                     Source_Location => Source_Loc,
                     Overflow_Check  => False,
                     I_Type          => Idx_Type);
      One : constant Irep :=
        Build_Index_Constant (Value      => 1,
                              Index_Type => Idx_Type,
                              Source_Loc => Source_Loc);
   begin
      return Make_Op_Add (Rhs             => One,
                          Lhs             => Diff,
                          Source_Location => Source_Loc,
                          Overflow_Check  => False,
                          I_Type          => Idx_Type);
   end Build_Array_Size;

   function Build_Array_Size (Array_Comp : Irep; Idx_Type : Irep) return Irep
   is
      Source_Loc : constant Source_Ptr := Get_Source_Location (Array_Comp);
      First : constant Irep :=
        Make_Member_Expr (Compound         => Array_Comp,
                          Source_Location  => Source_Loc,
                          Component_Number => 0,
                          I_Type           => Idx_Type,
                          Component_Name   => "first1");
      Last : constant Irep :=
        Make_Member_Expr (Compound         => Array_Comp,
                          Source_Location  => Source_Loc,
                          Component_Number => 1,
                          I_Type           => Idx_Type,
                          Component_Name   => "last1");

   begin
      return Build_Array_Size (First      => First,
                               Last       => Last,
                               Idx_Type => Idx_Type);
   end Build_Array_Size;

   function Offset_Array_Data (Base : Irep; Offset : Irep; Pointer_Type : Irep;
                               Source_Loc : Source_Ptr) return Irep
   is
      Old_Data : constant Irep :=
        Make_Member_Expr (Compound         => Base,
                          Source_Location  => Source_Loc,
                          Component_Number => 2,
                          I_Type           => Pointer_Type,
                          Component_Name   => "data");
   begin
      return Make_Op_Add (Rhs             => Offset,
                          Lhs             => Old_Data,
                          Source_Location => Source_Loc,
                          Overflow_Check  => False,
                          I_Type          => Pointer_Type);
   end Offset_Array_Data;

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

end GOTO_Utils;
