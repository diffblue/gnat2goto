-------------------------------------------------------------------------------
--  Separate definition for removing type bounds throughout Irep_Lists(s)
--  Implementation is adopted from ireps-to_json.adb
--  The rationale is to create a new list only when a new irep was created
--  by the respective Remove_Bounds.
-------------------------------------------------------------------------------
separate (Ireps)
function Remove_Bounds (L : Irep_List) return Irep_List
is
   The_List : Irep_List_Node;
   Ptr      : Internal_Irep_List;
   Arr      : constant Irep_List := New_List;
   Modified : Boolean := False;
begin
   if L /= 0 then
      The_List := Irep_List_Table.Table (To_Internal_List (L));
      Ptr      := To_Internal_List (Irep_List (The_List.A));
      while Ptr /= 0 loop
         declare
            Orig_Irep : constant Irep := Irep (Irep_List_Table.Table (Ptr).A);
            New_Irep  : constant Irep := Remove_Bounds (Orig_Irep);
         begin
            if Orig_Irep /= New_Irep then
               Append (Arr, New_Irep);
               Modified := True;
            else
               Append (Arr, Orig_Irep);
            end if;
            Ptr := Irep_List_Table.Table (Ptr).B;
         end;
      end loop;
   end if;

   pragma Assert (L'Size = Arr'Size);
   if Modified then
      return Arr;
   else
      return L;
   end if;
end Remove_Bounds;
