
with GNATCOLL.JSON;
use GNATCOLL.JSON;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Iinfo;
use Iinfo;
with Ada.Containers.Vectors;
use Ada.Containers;
package Symbol_Table_Info is
   
   type Symbol is record
      SymType : Irep;
      Value : Irep;
      Location : Irep;
      Name : Unbounded_String;
      Module : Unbounded_String;
      BaseName : Unbounded_String;
      Mode : Unbounded_String;
      PrettyName : Unbounded_String;
      IsType : Boolean;
      IsMacro : Boolean;
      IsExported : Boolean;
      IsInput : Boolean;
      IsOutput : Boolean;
      IsStateVar : Boolean;
      IsProperty : Boolean;
      IsStaticLifetime : Boolean;
      IsThreadLocal : Boolean;      
      IsLValue : Boolean;
      IsFileLocal : Boolean;
      IsExtern : Boolean;
      IsVolatile : Boolean;
      IsParameter : Boolean;
      IsAuxiliary : Boolean;
      IsWeak : Boolean;
   end record;
   
   function Symbol2Json(Sym : Symbol) return JSON_Value;
   
   package SymbolVectors is new Vectors(Natural, Symbol);
   subtype Symbol_Table is SymbolVectors.Vector;
   
   function SymbolTable2Json(Symtab : Symbol_Table) return JSON_Array;
   
end Symbol_Table_Info;
      
      
      
      
