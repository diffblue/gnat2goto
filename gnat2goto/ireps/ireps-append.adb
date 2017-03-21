separate (Ireps)
procedure Append (L : Irep_List; I : Irep)
is
   New_Head : constant Irep_List_Node := (Is_Node => True,
                                          A       => Integer (I),
                                          B       => 0);
   The_List : Irep_List_Node renames
     Irep_List_Table.Table (To_Internal_List (L));

   New_Ptr : Internal_Irep_List;
begin
   Irep_List_Table.Append (New_Head);
   New_Ptr := Irep_List_Table.Last;

   if The_List.A = 0 then
      The_List.A := Integer (To_List (New_Ptr));
      The_List.B := New_Ptr;
   else
      Irep_List_Table.Table (The_List.B).B := New_Ptr;
      The_List.B := New_Ptr;
   end if;
end Append;
