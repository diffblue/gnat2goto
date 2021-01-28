with Atree;    use Atree;
with Einfo;    use Einfo;
with Sinfo;    use Sinfo;
with Types;    use Types;

with Ireps;    use Ireps;

package Gnat2goto_Itypes is
   procedure Do_Itype_Reference (N : Node_Id)
     with Pre => Nkind (N) = N_Itype_Reference;

   procedure Declare_Itype (Ty : Entity_Id);

private
   function Do_Itype_Definition (N : Node_Id) return Irep
     with Pre => Nkind (N) = N_Defining_Identifier;

   function Do_Itype_Array_Subtype (N : Entity_Id) return Irep
     with Pre => Is_Itype (N) and then Ekind (N) = E_Array_Subtype;

   function Do_Itype_Enumeration_Subtype (N : Entity_Id) return Irep
     with Pre => Is_Itype (N) and then
                 Nkind (N) in N_Entity and then
                 Ekind (N) = E_Enumeration_Subtype;

   function Do_Itype_Enumeration_Type (N : Entity_Id) return Irep
     with Pre => Is_Itype (N) and then
                 Nkind (N) in N_Entity and then
                 Ekind (N) = E_Enumeration_Type;

   function Do_Itype_String_Literal_Subtype (N : Entity_Id) return Irep
     with Pre => Is_Itype (N) and then Ekind (N) = E_String_Literal_Subtype;

   function Do_Itype_Integer_Subtype (N : Entity_Id) return Irep
     with Pre => Is_Itype (N) and then Ekind (N) = E_Signed_Integer_Subtype;

   function Do_Itype_Integer_Type (N : Entity_Id) return Irep
     with Pre => Is_Itype (N) and then Ekind (N) = E_Signed_Integer_Type;

   function Do_Itype_Record_Subtype (N : Entity_Id) return Irep
     with Pre => Is_Itype (N) and then Ekind (N) = E_Record_Subtype;

   function Do_Modular_Integer_Subtype (N : Entity_Id) return Irep
     with Pre => Is_Itype (N) and then Ekind (N) = E_Modular_Integer_Subtype;

   function Do_Itype_Array_Type (E : Entity_Id) return Irep
    with Pre => Is_Itype (E) and Ekind (E) = E_Array_Type;

end Gnat2goto_Itypes;
