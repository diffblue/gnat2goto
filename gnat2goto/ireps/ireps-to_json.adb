separate (Ireps)
function To_JSON (L : Irep_List) return JSON_Array
is
   The_List : Irep_List_Node;
   Ptr      : Internal_Irep_List;
begin
   return Arr : JSON_Array := Empty_Array do
      if L /= 0 then
         The_List := Irep_List_Table.Table (To_Internal_List (L));
         Ptr      := To_Internal_List (Irep_List (The_List.A));
         while Ptr /= 0 loop
            Append (Arr, To_JSON (Irep (Irep_List_Table.Table (Ptr).A)));
            Ptr := Irep_List_Table.Table (Ptr).B;
         end loop;
      end if;
   end return;
end To_JSON;
