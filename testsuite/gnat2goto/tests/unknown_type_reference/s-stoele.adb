with Ada.Unchecked_Conversion;
package body System.Storage_Elements is
   function "+" (Left : Address; Right : Storage_Offset) return Address is
   begin
      return Storage_Elements.To_Address
        (To_Integer (Left) + To_Integer (To_Address (Right)));
   end "+";
end System.Storage_Elements;
