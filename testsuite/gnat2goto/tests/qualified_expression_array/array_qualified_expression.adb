procedure Array_Qualified_Expression is
   type Int_Array is array (Integer range <>) of Integer;
   
   subtype Small_Array is Int_Array (1 .. 5);
   subtype Big_Array is Int_Array (1 .. 10);
   
   S1, S2, S3 : Small_Array;
   B1         : Big_Array;
   
   X : Integer;
begin
   S1 := Small_Array'(1, 2, 3, 4, 5);  -- Should succeed or no dynamic check
   B1 := Big_Array'(others => 19);     -- Should succeed
   S2 := Small_Array'(B1 (1 .. 5));    -- Should succeed
   X := B1'Last;
   S3 := Small_Array'(B1 (1 .. X));      -- Should fail
   
end Array_Qualified_Expression;
   
