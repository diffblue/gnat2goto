with Generic_Package;
package body Extra_Package is

   type Trig_Float is digits 4;

   --  null-statement declaration happens here
   --  or more precisely the package Generic_Package
   --  has pragma Pure
   --  which is (somewhere) rewritten to null-statement
   package Renamed_Package is new
     Generic_Package(Trig_Float);

   function Unused_Function (X : Float) return Float
   is
   begin
      return X;
   end Unused_Function;

end Extra_Package;
