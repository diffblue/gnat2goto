with Types;              use Types;
with Ireps;              use Ireps;
package ASVAT.Size_Model is
   function Do_Attribute_Size (N : Node_Id) return Irep;

   function Has_Size_Rep_Clause (E : Entity_Id) return Boolean;
   function Get_Rep_Size (E : Entity_Id) return Uint
     with Pre => Has_Size_Rep_Clause (E);
   procedure Set_Rep_Size (E : Entity_Id; Size : Uint)
     with Pre => not Has_Size_Rep_Clause (E);

   function Has_Component_Size_Rep_Clause (E : Entity_Id) return Boolean;
   function Get_Rep_Component_Size (E : Entity_Id) return Uint
     with Pre => Has_Component_Size_Rep_Clause (E);
   procedure Set_Rep_Component_Size (E : Entity_Id; Size : Uint)
     with Pre => not Has_Component_Size_Rep_Clause (E);
end ASVAT.Size_Model;
