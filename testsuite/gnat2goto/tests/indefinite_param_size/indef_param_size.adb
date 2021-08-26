procedure Indef_Param_Size (S1, S2 : String) is
begin
   pragma Assert (S1'Size = 16);
   pragma Assert (S2'Size = 0);
   null;
end Indef_Param_Size;
