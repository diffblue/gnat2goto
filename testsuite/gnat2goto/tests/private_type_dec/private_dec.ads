package Private_Dec is
   type Dec is private;

   procedure P (X : in out Dec);
   procedure Q (X : in out Integer);

private
   type Dec is record
      A : Integer;
   end record;
end Private_Dec;
