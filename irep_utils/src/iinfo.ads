with GNATCOLL.JSON;         use GNATCOLL.JSON;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Iinfo is

   type Irep is record
      Id        : Unbounded_String;
      Sub       : JSON_Array := Empty_Array;
      Named_Sub : JSON_Value := Create_Object;
      Comment   : JSON_Value := Create_Object;
   end record;

   function Trivial_Irep (Value : String) return Irep;
   function Trivial_Irep (Value : Integer) return Irep;
   function Trivial_Irep (Value : Boolean) return Irep;

   function Irep_To_Json (Ir : Irep) return JSON_Value;

end Iinfo;
