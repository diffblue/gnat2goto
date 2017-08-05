with Namet; use Namet;
with Sinfo; use Sinfo;
with Atree; use Atree;

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

end GOTO_Utils;
