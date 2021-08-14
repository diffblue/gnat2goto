procedure Attribute_Valid is
   type Action_Types is (Unknown, Push, Pop);
   subtype Valid_Action_Types is Action_Types range Push .. Pop;

   function Check_Action (A : Valid_Action_Types) return Boolean
   is
   begin
      if A'Valid then
         pragma Assert (A in Valid_Action_Types);
         return True;
      else
         pragma Assert (A in Valid_Action_Types);
         return False;
      end if;
   end Check_Action;

   Result : Boolean;
begin
   Result := Check_Action (Push);
   pragma Assert (Result);

   Result := Check_Action (Unknown);
   pragma Assert (not Result);
   null;
end Attribute_Valid;
