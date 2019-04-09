package Incomplete_Dec is
   type Partial_Dec;
   
   procedure P (X : in out Partial_Dec);
   
   type Partial_Dec is record
      A : Integer;
   end record;
end Incomplete_Dec;
