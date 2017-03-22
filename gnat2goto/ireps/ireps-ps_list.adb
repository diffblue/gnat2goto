separate (Ireps)
procedure PS_List (L : Irep_List; Name : String)
is
   C : List_Cursor := List_First (L);
begin
   if L = 0 then
      return;
   end if;

   Write_Str ("List #" & Name & " (Irep_List=");
   Write_Int (Int (L));
   Write_Char (')');
   Write_Eol;

   Indent;
   while List_Has_Element (L, C) loop
      PI_Irep (List_Element (L, C));
      C := List_Next (L, C);
   end loop;
   Outdent;
end PS_List;
