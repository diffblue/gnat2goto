
with GNATCOLL.JSON;
use GNATCOLL.JSON;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
package Irep is
   
   type Irep is record
      Id : Unbounded_String;
      Sub : JSON_Array := Empty_Array;
      NamedSub : JSON_Value := Create_Object;
      Comment : JSON_Value := Create_Object;
   end record;
   
   function TrivialIrep(Value : String) return Irep;
   function TrivialIrep(Value : Integer) return Irep;
   function TrivialIrep(Value : Boolean) return Irep;
   
   function Irep2Json(Ir : Irep) return JSON_Value;
   
end Irep;
