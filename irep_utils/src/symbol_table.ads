
with GNATCOLL.JSON;
use GNATCOLL.JSON;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Irep;
with Ada.Containers.Vectors;
use Ada.Containers;
package SymbolTable is
   
   type Symbol is record
      Irep SymType;
      Irep Value;
      Irep Location;
      Unbounded_String Name;
      Unbounded_String Module;
      Unbounded_String BaseName;
      Unbounded_String Mode;
      Unbounded_String PrettyName;
      Boolean IsType;
      Boolean IsMacro;
      Boolean IsExported;
      Boolean IsInput;
      Boolean IsOutput;
      Boolean IsStateVar;
      Boolean IsProperty;
      Boolean IsStaticLifetime;
      Boolean IsThreadLocal;      
      Boolean IsLValue;
      Boolean IsFileLocal;
      Boolean IsExtern;
      Boolean IsVolatile;
      Boolean IsParameter;
      Boolean IsAuxiliary;
      Boolean IsWeak;
   end record;
   
   function Symbol2Json(Sym : Symbol) return JSON_Value;
   
   package SymbolVectors is new Vectors(Symbol, Integer);
   type SymbolTable is new SymbolVectors.Vector;
   
   function SymbolTable2Json(Symtab : SymbolTable) return JSON_Value;
   
end SymbolTable;
      
      
      
      
