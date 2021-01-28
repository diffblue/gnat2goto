procedure Forward_Dec is

   --  Test case to check the forward declarations such as a private type
   --  whose full type declaration include intermediate declarations between
   --  the partioal (forward) declaration and its full type declaration
   --  (completion).
   package P
   is

      type T is private;

      MaxNameLength : constant := 16;

      -- A function is used here because gnat2goto does not do
      --  package initialisation yet.
      function NullFile return T;

      function Val_T (V : T) return Integer;

   private

      subtype NameLengthT is Natural range 0 .. MaxNameLength;
      subtype NameI is Positive range 1 .. MaxNameLength;
      subtype NameTextT is String (NameI);

      type NameT is record
         Text   : NameTextT;
         Length : NameLengthT;
      end record;

      Type FilePtr is access all Natural;

      type T is record
         Name : NameT;
         Handle : FilePtr;
      end record;

      function Val_T (V : T) return Integer is (V.Name.Length);
   end P;

   package body P is
      --  A statically declared object is used here because
      --  currently gnat2goto does not support the "new" expression.
      Dummy : aliased Natural;
      function NullFile return T is
         NullFile_Ptr : FilePtr;
      begin
         Dummy := 0;
         NullFile_Ptr := Dummy'Access;
         return T'(NameT'( NameTextT'(others => ' '),
                   0),
                   NullFile_Ptr);
      end NullFile;
      end P;

   use P;

   V : T;
begin
   V  := NullFile;
   pragma Assert (Val_T (V) = 0);
   null;
end Forward_Dec;
