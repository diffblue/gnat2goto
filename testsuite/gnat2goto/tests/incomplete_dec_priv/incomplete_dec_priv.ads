package Incomplete_Dec_Priv is
   type Incomplete_Dec;
   
   procedure P (X : in out Incomplete_Dec);
   
   type Incomplete_Dec is private;
private
   type Incomplete_Dec is record
      A : Integer;
   end record;
end Incomplete_Dec_Priv;
