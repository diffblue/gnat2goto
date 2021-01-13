package Bar is
   function Add_One (X : Integer) return Integer is
      (X + 1);

   procedure Use_Add_One (X : in Integer; R : out Integer);
end Bar;