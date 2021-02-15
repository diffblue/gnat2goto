with Sem_Util;                use Sem_Util;
with Tree_Walk;               use Tree_Walk;
package body ASVAT.Pragma_Info is

   procedure Set_Pre_Condition (E : Entity_Id; N : Node_Id) is
      Enty_Id : constant Symbol_Id := Intern (Unique_Name (E));
      Already_In_Table : constant Boolean :=
        Extra_Entity_Info.Contains (Enty_Id);
      Info : Entity_Info :=
        (if Already_In_Table then
            Extra_Entity_Info (Enty_Id)
         else
            Empty_Entity_Info);
   begin
      Info.Pre_Condition := N;
      if Already_In_Table then
         Extra_Entity_Info.Replace (Enty_Id, Info);
         --  Probably need to flag something in this case TODO
      else
         Extra_Entity_Info.Insert (Enty_Id, Info);
      end if;

   end Set_Pre_Condition;

   function Has_Pre_Condition (E : Entity_Id) return Boolean is
      Enty_Id : constant Symbol_Id := Intern (Unique_Name (E));
   begin
      return Extra_Entity_Info.Contains (Enty_Id) and then
        Extra_Entity_Info (Enty_Id).Pre_Condition /= Types.Empty;
   end Has_Pre_Condition;

   --  function Do_Pre_Condition (E : Entity_Id) return Irep;

   function Get_Pre_Condition (E : Entity_Id) return Node_Id is
      Enty_Id : constant Symbol_Id := Intern (Unique_Name (E));
      Info : constant Entity_Info := Extra_Entity_Info.Element (Enty_Id);
   begin
      return Info.Pre_Condition;
   end Get_Pre_Condition;

   procedure Set_Post_Condition (E : Entity_Id; N : Node_Id) is
      Enty_Id : constant Symbol_Id := Intern (Unique_Name (E));
      Already_In_Table : constant Boolean :=
        Extra_Entity_Info.Contains (Enty_Id);
      Info : Entity_Info :=
        (if Already_In_Table then
            Extra_Entity_Info (Enty_Id)
         else
            Empty_Entity_Info);
   begin
      Info.Post_Condition := N;
      if Already_In_Table then
         Extra_Entity_Info.Replace (Enty_Id, Info);
         --  Probably need to flag something in this case TODO
      else
         Extra_Entity_Info.Insert (Enty_Id, Info);
      end if;

   end Set_Post_Condition;

   function Has_Post_Condition (E : Entity_Id) return Boolean is
      Enty_Id : constant Symbol_Id := Intern (Unique_Name (E));
   begin
      return Extra_Entity_Info.Contains (Enty_Id) and then
        Extra_Entity_Info (Enty_Id).Post_Condition /= Types.Empty;
   end Has_Post_Condition;

   function Get_Post_Condition (E : Entity_Id) return Node_Id is
      Enty_Id : constant Symbol_Id := Intern (Unique_Name (E));
      Info : constant Entity_Info := Extra_Entity_Info.Element (Enty_Id);
   begin
      return Info.Post_Condition;
   end Get_Post_Condition;

end ASVAT.Pragma_Info;
