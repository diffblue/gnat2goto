procedure Integer_Qualified_Expression (V1, V2 : out Integer) is
begin
   V1 := 5 + 3;
   V2 := Integer'(V1 + 17);  --  Should succeed 
   
end Integer_Qualified_Expression;
   
