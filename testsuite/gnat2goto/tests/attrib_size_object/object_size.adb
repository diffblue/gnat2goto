--  with Text_IO; use Text_IO;
procedure Object_Size is
   pragma Assertion_Policy (Assert => Check);

   subtype Small_Positive is Integer range 1 .. 64;

   Small_Pos : Small_Positive;
   for Small_Pos'Size use 8;

    Uncons_SP : Small_Positive;
begin
   --  Small_Positive'Size is the minimum number of bits to hold a value
   --  of Small_Positive, 7.
   Small_Pos := Small_Positive'Size;
   pragma Assert (Small_Pos = 7, "The value is " & Integer'Image (Small_Pos));
   --  Put_Line ("Small_Positive subtype size = " & Integer'Image (Small_Pos));
   --  A Size attribute clause has been applied to the declaration of the
   --  object Small_Pos and so Small_Pos'Size should equal 8.
   --  If the Size attribute clause had not been applied, since it is
   --  a subtype of Integer it would have had the default size of 32.
   Small_Pos := Small_Pos'Size;
   pragma Assert (Small_Pos = 8, "The value is " & Integer'Image (Small_Pos));
   --  Put_Line ("Small_Pos object size " & Integer'Image (Small_Pos));

   --  Uncons_SP is an object declaration of type Small_Positive which is not
   --  canstrained by a Size Attribute clause and, since it is a
   --  subtype of Standard.Integer, it will have the same number of bits
   --   as used for type Standard.Integer.
   Uncons_SP := Uncons_SP'Size;
   pragma Assert (Uncons_SP = 32, "The value is " & Integer'Image (Uncons_SP));
   --  Put_Line ("Small_Positive unconstrained object size = "
   --            & Integer'Image (Uncons_SP));

end Object_Size;
