procedure Bar is
   type Month is (January, February, March, April, May, June, July, August, September, October, November, December);
   subtype Winter is Month
     with Static_Predicate => Winter in December | January | February;

   function Is_January (X : Winter) return Boolean is (X = January);

   J : Winter := January;
   F : Winter := February;

   Result : Boolean;

begin
   Result := Is_January (J);
   pragma Assert (Result);

   Result := Is_January (F);
   pragma Assert (not Result);
end Bar;
