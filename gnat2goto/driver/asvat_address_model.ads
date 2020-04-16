with Types;               use Types;
with Atree;               use Atree;
with Sinfo;               use Sinfo;
--  with Einfo;               use Einfo;
--  with Namet;               use Namet;
with Snames;              use Snames;
with Ireps;               use Ireps;
package ASVAT_Address_Model is

   type Address_To_Access_Functions is
     (Not_An_Adress_Function, To_Pointer_Function, To_Address_Function);

   function Do_ASVAT_Address_Of (N : Node_Id) return Irep
     with Pre => Nkind (N) = N_Attribute_Reference and then
     Get_Attribute_Id (Attribute_Name (N)) = Attribute_Address;

   function Get_Intrinsic_Address_Function (E : Entity_Id) return
     Address_To_Access_Functions;

   procedure Make_To_Pointer (E : Entity_Id);
--       with Pre => Is_Intrinsic_Subprogram (E) and
--       Get_Name_String (Chars (E)) = "to_pointer";

   procedure Make_To_Address (E : Entity_Id);
--       with Pre => Is_Intrinsic_Subprogram (E) and
--       Get_Name_String (Chars (E)) = "to_address";
end ASVAT_Address_Model;
