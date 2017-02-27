
package body SymbolTable is
   
   function Symbol2Json(Sym : Symbol) return JSON_Value is
      JSON_Value Ret := Create_Object;
   begin
      Ret.Set_Field("type", Irep.Irep2Json(Sym.SymType));
      Ret.Set_Field("value", Irep.Irep2Json(Sym.Value));
      Ret.Set_Field("location", Irep.Irep2Json(Sym.Locaton));
      Ret.Set_Field("name", To_String(Sym.Name));
      Ret.Set_Field("module", To_String(Sym.Module));
      Ret.Set_Field("basename", To_String(Sym.BaseName));
      Ret.Set_Field("mode", To_String(Sym.Mode));
      Ret.Set_Field("pretty_name", To_String(Sym.PrettyName));
      Ret.Set_Field("is_type", Sym.IsType);
      Ret.Set_Field("is_macro", Sym.IsMacro);
      Ret.Set_Field("is_exported", Sym.IsExported);
      Ret.Set_Field("is_input", Sym.IsInput);
      Ret.Set_Field("is_output", Sym.IsOutput);
      Ret.Set_Field("is_state_var", Sym.IsStateVar);
      Ret.Set_Field("is_property", Sym.IsProperty);
      Ret.Set_Field("is_static_lifetime", Sym.IsStaticLifetime);
      Ret.Set_Field("is_thread_local", Sym.IsThreadLocal);
      Ret.Set_Field("is_lvalue", Sym.IsLValue);
      Ret.Set_Field("is_file_local", Sym.IsFileLocal);
      Ret.Set_Field("is_extern", Sym.IsExtern);
      Ret.Set_Field("is_volatile", Sym.IsVolatile);
      Ret.Set_Field("is_parameter", Sym.IsParameter);
      Ret.Set_Field("is_auxiliary", Sym.IsAuxiliary);
      Ret.Set_Field("is_weak", Sym.IsWeak);
      return Ret;
   end;
     
   function SymbolTable2Json(Symtab : SymbolTable) return JSON_Value is
      JSON_Array Ret := Empty_Array;
   begin
      
	
   
end SymbolTable
