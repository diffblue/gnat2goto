with Ada.Containers.Hashed_Maps;
with Ada.Containers;             use Ada.Containers;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with GNATCOLL.JSON;              use GNATCOLL.JSON;

with Iinfo;                      use Iinfo;

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
      IsType           : Boolean := False;
      IsMacro          : Boolean := False;
      IsExported       : Boolean := False;
      IsInput          : Boolean := False;
      IsOutput         : Boolean := False;
      IsStateVar       : Boolean := False;
      IsProperty       : Boolean := False;
      IsStaticLifetime : Boolean := False;
      IsThreadLocal    : Boolean := False;
      IsLValue         : Boolean := False;
      IsFileLocal      : Boolean := False;
      IsExtern         : Boolean := False;
      IsVolatile       : Boolean := False;
      IsParameter      : Boolean := False;
      IsAuxiliary      : Boolean := False;
      IsWeak           : Boolean := False;
   end record;

   function Symbol2Json (Sym : Symbol) return JSON_Value;

   package Symbol_Maps is new Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Symbol,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   subtype Symbol_Table is Symbol_Maps.Map;

   function SymbolTable2Json(Symtab : Symbol_Table) return JSON_Array;

end Symbol_Table_Info;
