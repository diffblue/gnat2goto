procedure Foo is
   type Month is (January, February, March, April, May, June, July, August, September, October, November, December);

   subtype Only_Jan is Month
     with Static_Predicate => Only_Jan = January;

   function Is_January2 (X : Only_Jan) return Boolean is (X = January);

   J2 : Only_Jan := January;

   Result : Boolean;

begin
   Result := Is_January2 (J2);
   pragma Assert (Result);
end Foo;
