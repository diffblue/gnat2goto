separate (Ireps)
procedure PS_List (L : Irep_List; Name : String)
is
   The_List : Irep_List_Node;
   Ptr      : Internal_Irep_List;
begin
   Write_Str ("List #" & Name & " (Irep_List=");
   Write_Int (Int (L));
   Write_Char (')');
   Write_Eol;

   if L = 0 then
      return;
   else
      The_List := Irep_List_Table.Table (To_Internal_List (L));
      Ptr      := To_Internal_List (Irep_List (The_List.A));
      Indent;
      while Ptr /= 0 loop
         PI_Irep (Irep (Irep_List_Table.Table (Ptr).A));
         Ptr := Irep_List_Table.Table (Ptr).B;
      end loop;
      Outdent;
   end if;
end PS_List;
