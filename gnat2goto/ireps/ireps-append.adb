separate (Ireps)
procedure Append (L : Irep_List; I : Irep)
is
   New_Head : constant Irep_List_Node := (A => Integer (I),
                                          B => 0);
   New_Ptr : Internal_Irep_List;
begin
   Irep_List_Table.Append (New_Head);
   New_Ptr := Irep_List_Table.Last;

   if Irep_List_Table.Table (To_Internal_List (L)).A = 0 then
      Irep_List_Table.Table (To_Internal_List (L)).A :=
        Integer (To_List (New_Ptr));
      Irep_List_Table.Table (To_Internal_List (L)).B := New_Ptr;
   else
      Irep_List_Table.Table
        (Irep_List_Table.Table (To_Internal_List (L)).B).B := New_Ptr;
      Irep_List_Table.Table (To_Internal_List (L)).B := New_Ptr;
   end if;
end Append;
