separate (Ireps)
function Follow_Irep (L : Irep_List;
                     Follow_Symbol : not null access function (Symbol_I : Irep)
                        return Irep) return Irep_List
is
   The_List : Irep_List_Node;
   Ptr      : Internal_Irep_List;
begin
   return Arr : constant Irep_List := New_List do
      if L /= 0 then
         The_List := Irep_List_Table.Table (To_Internal_List (L));
         Ptr      := To_Internal_List (Irep_List (The_List.A));
         while Ptr /= 0 loop
            Append (Arr,
                    Follow_Irep (Irep (Irep_List_Table.Table (Ptr).A),
                                 Follow_Symbol));
            Ptr := Irep_List_Table.Table (Ptr).B;
         end loop;
      end if;
   end return;
end Follow_Irep;
