package ASVAT.Pragma_Info is

   procedure Set_Pre_Condition (E : Entity_Id; N : Node_Id);

   function Has_Pre_Condition (E : Entity_Id) return Boolean;

   --  function Do_Pre_Condition (E : Entity_Id) return Irep;

   function Get_Pre_Condition (E : Entity_Id) return Node_Id;

   procedure Set_Post_Condition (E : Entity_Id; N : Node_Id);

   function Has_Post_Condition (E : Entity_Id) return Boolean;

   function Get_Post_Condition (E : Entity_Id) return Node_Id;

end ASVAT.Pragma_Info;
