with Ada.Containers.Hashed_Maps;
with Ada.Containers;            use Ada.Containers;
with GNATCOLL.Symbols;
with Symbol_Table_Info;         use Symbol_Table_Info;
with Uintp;                     use Uintp;
with Ireps;                     use Ireps;
package ASVAT is
private
   type Entity_Info is record
      Specified_Rep_Size           : Uint;
      Specified_Rep_Component_Size : Uint;
      Model_Size                   : Natural;
      Computed_Model_Size          : Irep;
   end record;

   Empty_Entity_Info : constant Entity_Info :=
     (Specified_Rep_Size           => Uint_0,
      Specified_Rep_Component_Size => Uint_0,
      Model_Size                   => 0,
      Computed_Model_Size          => Ireps.Empty);

   use type GNATCOLL.Symbols.Symbol;  --  for "=" operator

   package Entity_Info_Maps is new Hashed_Maps
     (Key_Type        => Symbol_Id,  --  Should be Entity_Id
      Element_Type    => Entity_Info,
      Hash            => GNATCOLL.Symbols.Hash,
      Equivalent_Keys => "=");

   subtype Entity_Info_Table is Entity_Info_Maps.Map;

   Extra_Entity_Info : Entity_Info_Table;
end ASVAT;
