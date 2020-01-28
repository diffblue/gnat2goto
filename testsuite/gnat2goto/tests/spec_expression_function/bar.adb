package body Bar is
   procedure Use_Add_One (X : in Integer; R : out Integer) is
   begin
       R := Add_One (X);
   end Use_Add_One;
end Bar;
