with Interfaces;  use type Interfaces.Unsigned_32;
with My_Intefaces; use type My_Intefaces.Byte;
procedure Subprog_Renaming is
   --  Note: function Interfaces.Shift_Left is an Intrinsic imported function
   --  and has no body defined in Interfaces and so will give a warning from
   --  cbmc that it has no body.
   FUNCTION sl (value : interfaces.unsigned_32; amount : natural)
                RETURN interfaces.unsigned_32
                RENAMES interfaces.shift_left;

   FUNCTION Do_It (n : interfaces.unsigned_32; bits : integer)
                         RETURN interfaces.unsigned_32 IS
      r : interfaces.unsigned_32 := n;-- result
   BEGIN
      r := sl (n, bits);
      RETURN r;
   END Do_It;

   function My_sl (Value : My_Intefaces.Byte; Amount : Natural)
                   return My_Intefaces.Byte
                   renames My_Intefaces.Shift_Left;

   function Id (X : Integer) return Integer is (X);

   function Ident  (X : Integer) return Integer;

   function Ident (X : Integer) return Integer renames Id;

   procedure Inc (X : in out Integer) is
   begin
      X := X + 1;
   end Inc;

   procedure Increment (Y : in out Integer) renames Inc;

   procedure Add_1 (X : in out Integer);

   procedure Add_1 (X : in out Integer) renames Inc;

   procedure Add_One (W : in out Integer) renames Increment;

   type Enum is (one, two, three);

   function Number_One return Enum renames one;
   
   --  A horrible thing to do but here it is used to
   --  check the renaming with an operator symbol.
   function "-" (Left, Right : Integer) return Integer
                 renames "+";
   
   E : Enum := Number_One;

   V : Integer := 0;

   U : Interfaces.Unsigned_32 := 2;

   W : My_Intefaces.Byte := 2;

begin
   Increment (V);
   pragma Assert (V = 1);
   Add_1 (V);
   pragma Assert (V = 2);
   Add_One (V);
   pragma Assert (V = 3);
   V := Ident (V);
   pragma Assert (V = 3);
   U := Do_It (U, 1);
   --  Fails because the body of renamed function Interface_Shift_Left
   --  is an Imported intrinsic function.
   pragma Assert (U = 4);
   W := My_sl (W, 1);
   pragma Assert (W = 4);
   pragma Assert (E = one);
   --  Not a renaming declaration but was causing an unsupported report
   --  and was raised by Do_Function_Call that needed changing to support
   --  subprogram renamin renaming so it has been fixed and this statement
   --  checks that the problem has been resolved.
   V := "+" (V, 1);
   pragma Assert (V = 4);
   --  Because "-" has been renamed as "+"
   pragma Assert (V - 1 = 5);
end Subprog_Renaming;
