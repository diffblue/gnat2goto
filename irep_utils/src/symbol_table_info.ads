with Ada.Containers.Vectors;
with Ada.Containers;        use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.JSON;         use GNATCOLL.JSON;

with Iinfo;                 use Iinfo;

package Symbol_Table_Info is

   type Symbol is record
      SymType          : Irep;
      Value            : Irep;
      Location         : Irep;
      Name             : Unbounded_String;
      Module           : Unbounded_String;
      BaseName         : Unbounded_String;
      Mode             : Unbounded_String;
      PrettyName       : Unbounded_String;
      IsType           : Boolean;
      IsMacro          : Boolean;
      IsExported       : Boolean;
      IsInput          : Boolean;
      IsOutput         : Boolean;
      IsStateVar       : Boolean;
      IsProperty       : Boolean;
      IsStaticLifetime : Boolean;
      IsThreadLocal    : Boolean;
      IsLValue         : Boolean;
      IsFileLocal      : Boolean;
      IsExtern         : Boolean;
      IsVolatile       : Boolean;
      IsParameter      : Boolean;
      IsAuxiliary      : Boolean;
      IsWeak           : Boolean;
   end record;

   function Symbol2Json (Sym : Symbol) return JSON_Value;

   package SymbolVectors is new Vectors (Index_Type   => Natural,
                                         Element_Type => Symbol);
   subtype Symbol_Table is SymbolVectors.Vector;

   function SymbolTable2Json(Symtab : Symbol_Table) return JSON_Array;

end Symbol_Table_Info;
