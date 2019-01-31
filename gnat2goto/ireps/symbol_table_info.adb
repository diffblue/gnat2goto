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
      Ret.Set_Field ("baseName",          Unintern (Sym.BaseName));
      Ret.Set_Field ("mode",               Unintern (Sym.Mode));
      Ret.Set_Field ("prettyName",        Unintern (Sym.PrettyName));
      Ret.Set_Field ("isType",            Sym.IsType);
      Ret.Set_Field ("isMacro",           Sym.IsMacro);
      Ret.Set_Field ("isExported",        Sym.IsExported);
      Ret.Set_Field ("isInput",           Sym.IsInput);
      Ret.Set_Field ("isOutput",          Sym.IsOutput);
      Ret.Set_Field ("isStateVar",       Sym.IsStateVar);
      Ret.Set_Field ("isProperty",        Sym.IsProperty);
      Ret.Set_Field ("isStaticLifetime", Sym.IsStaticLifetime);
      Ret.Set_Field ("isThreadLocal",    Sym.IsThreadLocal);
      Ret.Set_Field ("isLvalue",          Sym.IsLValue);
      Ret.Set_Field ("isFileLocal",      Sym.IsFileLocal);
      Ret.Set_Field ("isExtern",          Sym.IsExtern);
      Ret.Set_Field ("isVolatile",        Sym.IsVolatile);
      Ret.Set_Field ("isParameter",       Sym.IsParameter);
      Ret.Set_Field ("isAuxiliary",       Sym.IsAuxiliary);
      Ret.Set_Field ("isWeak",            Sym.IsWeak);
      return Ret;
   end Symbol2Json;

   function SymbolTable2Json (Symtab : Symbol_Table) return JSON_Value is
      Ret         : constant JSON_Value := Create_Object;
      Symbol_Table_Json : constant JSON_Value := Create_Object;
      Symbol_Json : JSON_Value;
   begin
      for Symbol of Symtab loop
         Symbol_Json := Symbol2Json (Symbol);
         Symbol_Table_Json.Set_Field (Unintern (Symbol.Name), Symbol_Json);
      end loop;
      Ret.Set_Field ("symbolTable", Symbol_Table_Json);
      return Ret;
   end SymbolTable2Json;

end Symbol_Table_Info;
