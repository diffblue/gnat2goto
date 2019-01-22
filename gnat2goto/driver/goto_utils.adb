with Namet;   use Namet;
with Nlists;  use Nlists;
with Aspects; use Aspects;

with Ada.Text_IO;           use Ada.Text_IO;

package body GOTO_Utils is

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
      Func_Param_Id : constant Symbol_Id := Intern (Fun_Name & Param_Name);
      --  Create an irep for the parameter
      Value_Arg : constant Irep :=
        Make_Code_Parameter (Source_Location => Source_Location,
                             Default_Value   => Ireps.Empty,
                             I_Type          => Param_Type,
                             Base_Name       => Param_Name,
                             This            => False,
                             Identifier      => Unintern (Func_Param_Id));
   begin
      --  Creates a symbol for the parameter
      New_Parameter_Symbol_Entry (Name_Id        => Func_Param_Id,
                                  BaseName       => Param_Name,
                                  Symbol_Type    => Param_Type,
                                  A_Symbol_Table => A_Symbol_Table);
      Append_Parameter (Param_List, Value_Arg);
      return Value_Arg;
   end Create_Fun_Parameter;

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
