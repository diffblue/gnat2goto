package body Symbol_Table_Info is

   function Symbol2Json (Sym : Symbol) return JSON_Value is
      Ret : JSON_Value := Create_Object;
   begin
      Ret.Set_Field ("type",               Iinfo.Irep_To_Json (Sym.SymType));
      Ret.Set_Field ("value",              Iinfo.Irep_To_Json (Sym.Value));
      Ret.Set_Field ("location",           Iinfo.Irep_To_Json (Sym.Location));
      Ret.Set_Field ("name",               To_String (Sym.Name));
      Ret.Set_Field ("module",             To_String (Sym.Module));
      Ret.Set_Field ("baseName",          To_String (Sym.BaseName));
      Ret.Set_Field ("mode",               To_String (Sym.Mode));
      Ret.Set_Field ("prettyName",        To_String (Sym.PrettyName));
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
   end;

   function SymbolTable2Json (Symtab : Symbol_Table) return JSON_Array is
      Ret         : JSON_Array := Empty_Array;
      Symbol_Json : JSON_Value;
   begin
      for Symbol of Symtab loop
         Symbol_Json := Symbol2Json (Symbol);
         Append (Ret, Symbol_Json);
      end loop;
      return Ret;
   end;

end Symbol_Table_Info;
