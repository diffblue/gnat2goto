with Types;                   use Types;
with Atree;                   use Atree;
with Sinfo;                   use Sinfo;
with Sem_Util;                use Sem_Util;
with Snames;                  use Snames;
with Ireps;                   use Ireps;
package ASVAT_Modelling is
   type Model_Sorts is (Not_A_Model, Nondet, Nondet_In_Type, Represents);
   subtype Valid_Model is Model_Sorts range Nondet .. Model_Sorts'Last;

   function Do_Nondet_Attribute
     (N : Node_Id; Type_Name : String) return Irep
   with Pre => Nkind (N) = N_Attribute_Reference;

   function Do_Nondet_Function_Call
     (Fun_Name : String; Loc : Source_Ptr) return Irep;

   function Do_Nondet_Valid (N : Node_Id) return Irep
   with Pre => Nkind (N) = N_Attribute_Reference and then
     Get_Attribute_Id (Attribute_Name (N)) = Attribute_Valid;

   function Get_Annotation_Name (N : Node_Id) return String
   with Pre => Nkind (N) = N_Pragma and then
               Get_Pragma_Id (N) = Pragma_Annotate;

   function Get_Import_Convention (N : Node_Id) return String
   with Pre => Nkind (N) = N_Pragma and then
               Get_Pragma_Id (N) = Pragma_Import;

   function Get_Import_External_Name (N : Node_Id) return String
   with Pre => Nkind (N) = N_Pragma and then
               Get_Pragma_Id (N) = Pragma_Import;
   --  Returns null string if the External_Name parameter is not present.

   function Get_Import_Link_Name (N : Node_Id) return String
   with Pre => Nkind (N) = N_Pragma and then
               Get_Pragma_Id (N) = Pragma_Import;
   --  Returns null string if the Link_Name parameter is not present.

   function Get_Model_From_Anno (N : Node_Id) return Model_Sorts
   with Pre => Nkind (N) = N_Aspect_Specification;

   function Get_Model_From_Import (N : Node_Id) return Model_Sorts
   with Pre => Nkind (N) = N_Pragma and then Get_Pragma_Id (N) = Pragma_Import;

   function Get_Model_Sort (E : Entity_Id) return Model_Sorts;

   function Is_Model (Model : Model_Sorts) return Boolean is
     (Model /= Not_A_Model);

   procedure Make_Model (E : Entity_Id; Model : Model_Sorts);

   procedure Make_Nondet_Function (Fun_Name, Result_Type : String;
                                   Statements : Irep;
                                   Loc : Source_Ptr);
end ASVAT_Modelling;
