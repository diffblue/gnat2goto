package body Symbol_Table_Info is

   Intern_Strings : constant GNATCOLL.Symbols.Symbol_Table_Access :=
     GNATCOLL.Symbols.Allocate;

   function Unintern (S : Symbol_Id) return String is
     (GNATCOLL.Symbols.Get (S).all);

   function Intern (S : String) return Symbol_Id is
     (GNATCOLL.Symbols.Find (Intern_Strings, S));

   function Symbol2Json (Sym : Symbol) return JSON_Value is
      Ret : constant JSON_Value := Create_Object;
   begin
      Ret.Set_Field ("type",               To_JSON (Sym.SymType));
      Ret.Set_Field ("value",              To_JSON (Sym.Value));
      Ret.Set_Field ("location",           To_JSON (Sym.Location));
      Ret.Set_Field ("name",               Unintern (Sym.Name));
      Ret.Set_Field ("module",             Unintern (Sym.Module));
      Ret.Set_Field ("base_name",          Unintern (Sym.BaseName));
      Ret.Set_Field ("mode",               Unintern (Sym.Mode));
      Ret.Set_Field ("pretty_name",        Unintern (Sym.PrettyName));
      Ret.Set_Field ("is_type",            Sym.IsType);
      Ret.Set_Field ("is_macro",           Sym.IsMacro);
      Ret.Set_Field ("is_exported",        Sym.IsExported);
      Ret.Set_Field ("is_input",           Sym.IsInput);
      Ret.Set_Field ("is_output",          Sym.IsOutput);
      Ret.Set_Field ("is_state_var",       Sym.IsStateVar);
      Ret.Set_Field ("is_property",        Sym.IsProperty);
      Ret.Set_Field ("is_static_lifetime", Sym.IsStaticLifetime);
      Ret.Set_Field ("is_thread_local",    Sym.IsThreadLocal);
      Ret.Set_Field ("is_lvalue",          Sym.IsLValue);
      Ret.Set_Field ("is_file_local",      Sym.IsFileLocal);
      Ret.Set_Field ("is_extern",          Sym.IsExtern);
      Ret.Set_Field ("is_volatile",        Sym.IsVolatile);
      Ret.Set_Field ("is_parameter",       Sym.IsParameter);
      Ret.Set_Field ("is_auxiliary",       Sym.IsAuxiliary);
      Ret.Set_Field ("is_weak",            Sym.IsWeak);
      return Ret;
   end Symbol2Json;

   function SymbolTable2Json (Symtab : Symbol_Table) return JSON_Array is
      Ret         : JSON_Array := Empty_Array;
      Symbol_Json : JSON_Value;
   begin
      for Symbol of Symtab loop
         Symbol_Json := Symbol2Json (Symbol);
         Append (Ret, Symbol_Json);
      end loop;
      return Ret;
   end SymbolTable2Json;

end Symbol_Table_Info;
