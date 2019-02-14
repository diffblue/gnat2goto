-------------------------------------------------------------------------------
--  Separate definition for following the symbol types throughout Irep_Lists(s)
--  Implementation is adopted from ireps-to_json.adb
--  The rationale is to create a new list only when a new irep was created
--  by the respective Follow_Irep. First, that sounds efficient and second
--  Ireps were disappearing when new list was created (even if it was filled
--  with the same ireps).
-------------------------------------------------------------------------------
separate (Ireps)
function Follow_Irep (L : Irep_List;
                     Follow_Symbol : not null access function (Symbol_I : Irep)
                        return Irep) return Irep_List
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
            New_Irep  : constant Irep := Follow_Irep (Orig_Irep,
                                                      Follow_Symbol);
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
end Follow_Irep;
