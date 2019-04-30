package Private_Dec is
   type Dec is private;
   
   procedure P (X : in out Dec);

private
   type Dec is record
      A : Integer;
   end record;
end Private_Dec;
