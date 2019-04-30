package Private_Dec_Abstract is
   type Dec is abstract tagged private;

   procedure P (X : in out Dec);

private
   type Dec is abstract tagged record
      A : Integer;
   end record;
end Private_Dec_Abstract;
