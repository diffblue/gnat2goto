with Types;              use Types;
package ASVAT.Size_Model is
   --  The size of an object in ASVAT is the size of the model of the object.
   --  This may not be the same size as the object in the executable code of
   --  the program.
   --  When the size attribute is applied to a (sub)type, as opposed to an
   --  object, gnat2goto attempts to obtain the value from the front end,
   --  where possible, otherwise it uses the size required by the ASVAT model
   --  for an object of the subtype.
   --
   --  Furthermore, the size of objects and components of a subtype may
   --  not be known or provided by the front-end leaving the determination
   --  of the size taken by an object of the subtype to the back-end code
   --  generator.
   --  Gnat2goto is effectively the back-end generating an ASVAT model and
   --  has to provide a size value for a subtype when the front-end does not.
   --
   --  The ASVAT model does not perform any sort of packing so that any
   --  pragma pack or size representation clause applied to an entity for
   --  which the attribute size is obtained from the ASVAT model will be
   --  ignored.
   --

   --  The function to obtain the value of X'Size or S'Size.
   function Do_Attribute_Size (N : Node_Id) return Irep;

   --  The various sizes, either specified in the source text, or determined
   --  from the model are set and read by the following subprograms.
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

   --  Sometimes the size of the ASVAT model of of a type is required
   --  rather than the compiler size.
   --  These subprograms are used to set and get the model size
   --  of a type.
   procedure Set_Static_Size (E : Entity_Id; Model_Size : Natural);
   procedure Set_Computed_Size (E : Entity_Id; Size_Expr : Irep);
   function Has_Static_Size (E : Entity_Id) return Boolean;
   function Has_Size (E : Entity_Id) return Boolean;
   function Static_Size (E : Entity_Id) return Natural
     with Pre => Has_Static_Size (E);
   function Computed_Size (E : Entity_Id) return Irep
     with Pre => Has_Size (E);

   --  Model representations of entities are byte aligned.
   function Make_Byte_Aligned_Size (S : Uint) return Uint;
   function Make_Byte_Aligned_Size (S : Integer) return Integer;

end ASVAT.Size_Model;
