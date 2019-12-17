--   with Text_IO; use Text_IO;
procedure Indefinite_Size is
   pragma Assertion_Policy (Assert => Check);

   type A is array (Positive range  <>) of Integer;
begin
   --  Put_Line (Integer'Image (A'Size));
   pragma Assert (A'Size = 21);
end Indefinite_Size;
