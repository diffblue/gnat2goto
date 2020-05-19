--  This example is run with pragma Profile (Rational)
--  in the configuration file gnat.adc.
--
procedure Iss_131 is

   TYPE neighbours IS (tom, barbara, margo, jerry);
   TYPE neigh_array IS ARRAY (neighbours) OF boolean;
   FOR neigh_array'size use 4;

   Vna : neigh_array := (others => False);

   type r is record
      a, b, c, d, e, f, g, h : boolean;
      chr                    : character;
   end record;
   for r'size use 16;


   VR : r := (chr => 'c', others => False);

   Test : Integer;

begin
   pragma Assert (Vna'size = 4);
   pragma Assert (neigh_array'size = 4);
   pragma Assert (r'Size = 16);
   pragma Assert (Vr'Size = 16);

   --  The front-end does not process the component size correctly with
   --  Profile (Rational) it substitutes the component_size with constant
   --  literal 8 in the AST but when its value is printed out, somehow,
   --  the back-end has changed the value to 1.
   --  Gnat2goto cannot correct this as it never
   --  encounters the attribute Component_Size in the AST.
   --  Hence the following 2 assertions are satisfied in the executable code
   --  but fail in cbmc.
   pragma Assert (Vna'Component_size = 1);
   pragma Assert (neigh_array'component_size = 1);

   --  If one looks at the goto function generated for this procedure,
   --  it can be seen that the front-end is putting the value 8 in the tree.
   Test := Vna'Component_Size;
   Test := neigh_array'Component_Size;
--     Test := S'Size;

   --  The following 2 assertions give a warning from the gnat compiler
   --  (but not gnat2goto) that they will fail at run time, but
   --  succeed in cbmc.
   pragma Assert (Vna'Component_size = 8);
   pragma Assert (neigh_array'component_size = 8);


end Iss_131;
