procedure Good_Size is
   pragma Assertion_Policy (Assert => Check);

   type Byte is range 0 .. 255;
   for Byte'Size use 8;

   type Nibble is range 0 .. 15;

   I : Byte;
   J : Nibble;
   K : Integer;

   type R is record
      A : Integer;
      B : Boolean;
   end record;
   pragma Pack (R);

   R_Obj : constant R := (1, False);

begin
   --  A Size attribute clause has been applied to Byte and so Byte'Size
   --  should match the specified size for the type
   I := Byte'Size;
   pragma Assert (I = 8, "Value is " & Integer'Image (Integer (I)));
   --  The object I does not have a Size attribute clause and so should be
   --  the same as the specified type size.
   I := I'Size;
   pragma Assert (I = 8, "Value is " & Integer'Image (Integer (I)));

   --  A Size attribute clause has not been applied to the declaration of
   --  Nibble and so Nibble'Size should be the minimum number of bits that
   --  can hold a Nibble object. Nibble'Size should equal 4
   J := Nibble'Size;
   pragma Assert (J = 4, "Value is " & Integer'Image (Integer (J)));
   J := J'Size;
   --  The object J nor its type (Nibble) have Size attribute clauses
   --  applied and so J'Size should equal the default size chosen by the
   --  frontend to represent a 4 bit value. Gnat frontend chooses 8 but this
   --  does not mean that the backend has to do so, it can chooses a minimum
   --  of 4, given by Nibble'Size or any number of bits more than this.
   pragma Assert (J = 8, "Value is: " & Integer'Image (Integer (J)));

   --  K is Standard.Integer, Gnat compilers use 32 bits to represent this type.
   K := Integer'Size;
   pragma Assert (K = 32, "Value is: " & Integer'Image (Integer (K)));
   --  The default size of objects of type Standard.Integer is also 32.
   K := K'Size;
   pragma Assert (K = 32, "Value is: " & Integer'Image (Integer (K)));

   --  R is packed record, R'Size should be the minimum number of bits required
   --  to hold a value of R.
   pragma Assert (R'Size = 33, "Value is: " & Integer'Image (R'Size));
   --  R_Obj does not have a Size attribute clause and so R_Obj'Size will
   --  take the default number of bits chosen by the frontend which must
   --  be greater or equal to R'Size.
   pragma Assert (R_Obj'Size >= R'Size,
                  "Value is: " & Integer'Image (R_Obj'Size));
end Good_Size;
