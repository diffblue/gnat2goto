with Ada.Containers.Hashed_Maps;
with Ada.Containers;             use Ada.Containers;

with Ireps;                      use Ireps;

with GNATCOLL.Symbols;
with GNATCOLL.JSON;              use GNATCOLL.JSON;

package Symbol_Table_Info is

   --  GOTO havily uses strings as identifiers and, in particular, often stores
   --  same identifiers several times. To avoid having many copies of the same
   --  string we use https://en.wikipedia.org/wiki/String_interning, which is
   --  already implemented in GNATCOLL.

   subtype Symbol_Id is GNATCOLL.Symbols.Symbol;
   --  Type of a string identifier, or symbol
   --
   --  Note: this type is really a fat pointer to Standard.String, which is
   --  128-bits (at least on x86_64 Linux). If this ever becomes a problem,
   --  we can replace it with a 32-bit integer (this number of strings should
   --  be enough) and use it as an index of a corresponding GNAT's String_Id.
   --  Don't use Ada.Containers.Vector for this, as they grow exponentially,
   --  but instead use GNAT.Table, which grow linearly. Also, don't use GNAT's
   --  Name_Id, because it relies on a 16-bit hash, which is fine for Ada
   --  names, but too small for both Ada and GOTO names (at least it is too
   --  small for Ada and Why3 names).

   function Intern   (S : String) return Symbol_Id;
   function Unintern (S : Symbol_Id) return String;
   --  Conversion between string and symbols named after LISP equivalents

   type Symbol is record
      SymType          : Irep := Empty;    --  Should be Entity_Id?
      Value            : Irep := Empty;    --  ??? What does this mean
      Location         : Irep := Empty;    --  Should be Source_Ptr
      Name             : Symbol_Id;        --  Can be derived form Key
      Module           : Symbol_Id;        --  ??? What does this mean
      BaseName         : Symbol_Id;        --  ??? What does this mean
      Mode             : Symbol_Id;        --  Always 'Ada'?
      PrettyName       : Symbol_Id;        --  ??? What does this mean
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

   use type GNATCOLL.Symbols.Symbol;  --  for "=" operator

   package Symbol_Maps is new Hashed_Maps
     (Key_Type        => Symbol_Id,  --  Should be Entity_Id
      Element_Type    => Symbol,
      Hash            => GNATCOLL.Symbols.Hash,
      Equivalent_Keys => "=");

   subtype Symbol_Table is Symbol_Maps.Map;

   function SymbolTable2Json (Symtab : Symbol_Table) return JSON_Value;

end Symbol_Table_Info;
