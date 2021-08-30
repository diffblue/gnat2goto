procedure Prove_False is

       type T;

      MaxNameLength : constant := 16;

      function NullFile return T;

      function Val_T (V : T) return Integer;
      procedure Set_T (N : Natural; V : in out T);

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

      procedure Set_T (N : Natural; V : in out T) is
      begin
         V.Name.Length := N;
      end Set_T;


   V : T;
begin
   V := NullFile;
   pragma Assert (Val_T (V) = 0);
   Set_T (1, V);
   pragma Assert (Val_T (V) = 1);
   Set_T (3, V);
   pragma Assert (Val_T (V) = 7);  -- cbmc fails invariant check no false case
   pragma Assert (Val_T (V) = 3);
   null;
end Prove_False;
