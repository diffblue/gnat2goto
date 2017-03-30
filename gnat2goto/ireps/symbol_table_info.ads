with Ada.Containers.Hashed_Maps;
with Ada.Containers;             use Ada.Containers;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with Ireps;                      use Ireps;

with GNATCOLL.JSON;              use GNATCOLL.JSON;

package Symbol_Table_Info is

   type Symbol is record
      SymType          : Irep := Empty;    --  Should be Entity_Id?
      Value            : Irep := Empty;    --  ??? What does this mean
      Location         : Irep := Empty;    --  Should be Source_Ptr
      Name             : Unbounded_String; --  Can be derived form Key
      Module           : Unbounded_String; --  ??? What does this mean
      BaseName         : Unbounded_String; --  ??? What does this mean
      Mode             : Unbounded_String; --  Always 'Ada'?
      PrettyName       : Unbounded_String; --  ??? What does this mean
      IsType           : Boolean := False; --  Can be derived from Key
      IsMacro          : Boolean := False; --  Always False in Ada?
      IsExported       : Boolean := False; --  ??? What does this mean
      IsInput          : Boolean := False; --  ??? What does this mean
      IsOutput         : Boolean := False; --  ??? What does this mean
      IsStateVar       : Boolean := False; --  ??? What does this mean
      IsProperty       : Boolean := False; --  ??? What does this mean
      IsStaticLifetime : Boolean := False; --  ??? What does this mean
      IsThreadLocal    : Boolean := False; --  ??? What does this mean
      IsLValue         : Boolean := False; --  ??? What does this mean
      IsFileLocal      : Boolean := False; --  ??? What does this mean
      IsExtern         : Boolean := False; --  ??? What does this mean
      IsVolatile       : Boolean := False; --  Can be derived from Key
      IsParameter      : Boolean := False; --  Can be derived from Key
      IsAuxiliary      : Boolean := False; --  ??? What does this mean
      IsWeak           : Boolean := False; --  ??? What does this mean
   end record;

   function Symbol2Json (Sym : Symbol) return JSON_Value;

   package Symbol_Maps is new Hashed_Maps
     (Key_Type        => Unbounded_String,  --  Should be Entity_Id
      Element_Type    => Symbol,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   subtype Symbol_Table is Symbol_Maps.Map;

   function SymbolTable2Json (Symtab : Symbol_Table) return JSON_Array;

end Symbol_Table_Info;
