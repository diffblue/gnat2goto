with GNATCOLL.JSON;              use GNATCOLL.JSON;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

package Iinfo is

   type Irep;
   type Irep_Ptr is access Irep;

   package Irep_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Irep_Ptr,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   package Irep_Vectors is new Ada.Containers.Vectors
     (Element_Type => Irep_Ptr,
      Index_Type   => Positive);

   type Irep is record
      Id        : Unbounded_String;
      Sub       : Irep_Vectors.Vector;
      Named_Sub : Irep_Maps.Map;
      Comment   : Irep_Maps.Map;
   end record;

   package Trivial is
      --  Prevent inheritence (these always make a plain Irep):
      function Trivial_Irep (Value : String) return Irep;
      function Trivial_Irep (Value : Integer) return Irep;
      function Trivial_Irep (Value : Boolean) return Irep;
   end Trivial;

   function Irep_To_Json (Ir : Irep) return JSON_Value;
   function Alloc_Clone (Ir : Irep) return Irep_Ptr;

end Iinfo;
