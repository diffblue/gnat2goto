--  This example is run with no configuration pragmas
--
--  with Text_IO; use Text_IO;
procedure Iss_131 is
   type SI is range 0 .. 3;
   for SI'Size use 2;

   type OS is range 0 .. 6;
   for OS'Size use 16;

   VSI : SI := 1;
   VOS : OS := 5;

   VSIS : SI := 2;
   for VSIS'Size use 16;

   TYPE neighbours IS (tom, barbara, margo, jerry);
   TYPE neigh_array IS ARRAY (neighbours) OF boolean;
   pragma Pack (neigh_array);
   FOR neigh_array'size use 4;

   Vna : neigh_array := (others => False);

   TYPE neigh_array_NS IS ARRAY (neighbours) OF boolean;

   VnaNS : neigh_array_NS := (others => False);

   --  Declaration and initialisation causes cbmc to give a precondition
   --  failure.
--   S : constant String := "Hello World";
--     for S'Size use 88;

   type R1 is record
      A : Integer;
      B : Integer;
   end record;
   for R1'Size use 64;

   type R2 is record
      H : R1;
      X : Integer;
      Y : Integer;
      N : neigh_array;
   end record;
   pragma Pack (R2);
   for R2'Size use 132;

   type R3 is record
      H : R1;
      B1 : Boolean;
      B2 : Boolean;
   end record;
   pragma Pack (R3);
   for R3'Size use 66;

   VR1 : R1; pragma Warnings (Off, VR1);
   VR2 : R2; pragma Warnings (Off, VR2);
   VR3 : R3; pragma Warnings (Off, VR3);

   type R4 is record
      A : Integer;
      B : Integer;
   end record;

   type R5 is record
      H : R1;
      X : Integer;
      Y : Integer;
      N : neigh_array;
   end record;

   type R6 is record
      H : R1;
      B1 : Boolean;
      B2 : Boolean;
   end record;

   VR4 : R1; pragma Warnings (Off, VR4);
   VR5 : R2; pragma Warnings (Off, VR5);
   VR6 : R3; pragma Warnings (Off, VR6);

   type r is record
      a, b, c, d, e, f, g, h : boolean;
      chr                    : character;
   end record;
   pragma Pack (r);
   for r'size use 16;

   VR : r; pragma Warnings (Off, VR);

   type rNS is record
      a, b, c, d, e, f, g, h : boolean;
      chr                    : character;
   end record;

   VRNS : rNS; pragma Warnings (Off, VRNS);

   type V1 (B : Boolean) is record
      case B is
         when True =>
            I : Integer;
         when False =>
            null;
      end case;
   end record;
   pragma Pack (V1);

   subtype STV1 is V1 (True);
   for STV1'Value_Size use 40;

   subtype SFV1 is V1 (False);
   for SFV1'Value_Size use 1;

   VST : STV1; pragma Warnings (Off, VST);
   VSF : SFV1; pragma Warnings (Off, VSF);

   type V2 (B : Boolean := False) is record
      case B is
         when True =>
            N : Natural;
         when False =>
            null;
      end case;
   end record;
   pragma Pack (V2);
   for V2'Size use 39;

   --  Declaration of this variable causes cbmc to give a precondition failure.
--     VV2 : V2;

   type V3 (B : Boolean) is record
      case B is
         when True =>
            I : Integer;
         when False =>
            null;
      end case;
   end record;

   subtype STV3 is V3 (True);
   for STV3'Value_Size use 64;

   subtype SFV3 is V3 (False);
   for SFV3'Value_Size use 8;

   subtype STV3_No_Size_Rep is V3 (True);

   subtype SFV3_No_Size_Rep is V3 (False);

   VST3 : STV3; pragma Warnings (Off, VST3);
   VSF3 : SFV3; pragma Warnings (Off, VSF3);

   VST3_NS : STV3_No_Size_Rep; pragma Warnings (Off, VST3_NS);
   VSF3_NS : SFV3_No_Size_Rep; pragma Warnings (Off, VSF3_NS);

   type V4 (B : Boolean := False) is record
      case B is
         when True =>
            N : Natural;
         when False =>
            null;
      end case;
   end record;

   --  Declaration of this variable causes cbmc to give a precondition failure.
   --     VV4 : V4;

   type A_T is array (SI) of SI;
   pragma Pack (A_T);

   VA : A_T := (others => 0);
   for VA'Size use 8;

begin
--     Put_Line ("Boolean'Size =" & Integer'Image (Boolean'Size));
--     Put_Line ("neighbours'Size = " & Integer'Image (neighbours'Size));
--     Put_Line ("Natural size = " & Integer'Image (Natural'Size));
--     Put_Line ("VSI size " & Integer'Image (VSI'Size));
--     Put_Line ("VOS size " & Integer'Image (VOS'Size));
--     Put_Line ("VSIS size " & Integer'Image (VSIS'Size));
--     Put_Line ("neigh_array size" & Integer'Image (neigh_array'Size));
--     Put_Line ("na component size " & Integer'Image (neigh_array'Component_Size));
--     Put_Line ("Vna size " & Integer'Image (Vna'Size));
--     Put_Line ("Vna component  size " & Integer'Image (Vna'Component_Size));
--     Put_Line ("Vna (barbara) size " & Integer'Image (Vna (barbara)'Size));
--     Put_Line ("Vna (barbara..margo)'Size " &
--               Integer'Image (Vna (barbara .. margo)'Size));
--     Put_Line ("Vna (barbara..margo)'Component_Size " &
--               Integer'Image (Vna (barbara .. margo)'Component_Size));
--     Put_Line ("neigh_array_NS " & Integer'Image (neigh_array_NS'Size));
--     Put_Line ("naNS component size " & Integer'Image (neigh_array_NS'Component_Size));
--     Put_Line ("VnaNS size " & Integer'Image (VnaNS'Size));
--     Put_Line ("VnaNS component  size " & Integer'Image (VnaNS'Component_Size));
--     Put_Line ("VnaNS (barbara) size " & Integer'Image (VnaNS (barbara)'Size));
--     Put_Line ("VnaNS (barbara..margo)'Size " &
--               Integer'Image (VnaNS (barbara .. margo)'Size));
--     Put_Line ("VnaNS (barbara..margo)'Component_Size " &
--               Integer'Image (VnaNS (barbara .. margo)'Component_Size));
--  --     Put_Line ("S size " & Integer'Image (S'Size));
--
--     Put_Line ("R1'Size " & Integer'Image (R1'Size));
--     Put_Line ("R2'Size " & Integer'Image (R2'Size));
--     Put_Line ("R3'Size " & Integer'Image (R3'Size));
--     Put_Line ("VR1'Size " & Integer'Image (VR1'Size));
--     Put_Line ("VR2'Size " & Integer'Image (VR2'Size));
--     Put_Line ("VR3'Size " & Integer'Image (VR3'Size));
--     Put_Line ("R4'Size " & Integer'Image (R4'Size));
--     Put_Line ("R5'Size " & Integer'Image (R5'Size));
--     Put_Line ("R6'Size " & Integer'Image (R6'Size));
--     Put_Line ("VR4'Size " & Integer'Image (VR4'Size));
--     Put_Line ("VR5'Size " & Integer'Image (VR5'Size));
--     Put_Line ("VR6'Size " & Integer'Image (VR6'Size));
--
--     Put_Line ("r'Size " & Integer'Image (r'Size));
--     Put_Line ("Vr'Size " & Integer'Image (Vr'Size));
--     Put_Line ("rNS'Size " & Integer'Image (rNS'Size));
--     Put_Line ("VrNS'Size " & Integer'Image (VrNS'Size));
--
--     Put_Line ("V1'Size " & Integer'Image (V1'Size));
--     Put_Line ("V2'Size " & Integer'Image (V2'Size));
--     Put_Line ("STV1'Size " & Integer'Image (STV1'Size));
--     Put_Line ("SFV1'Size " & Integer'Image (SFV1'Size));
--     Put_Line ("VST'Size " & Integer'Image (VST'Size));
--     Put_Line ("VSF'Size " & Integer'Image (VSF'Size));
--  --     Put_Line ("VV2'Size " & Integer'Image (VV2'Size));
--     Put_Line ("V3'Size " & Integer'Image (V3'Size));
--     Put_Line ("V4'Size " & Integer'Image (V4'Size));
--     Put_Line ("STV3'Size " & Integer'Image (STV3'Size));
--     Put_Line ("SFV3'Size " & Integer'Image (SFV3'Size));
--     Put_Line ("VST3'Size " & Integer'Image (VST3'Size));
--     Put_Line ("VSF3'Size " & Integer'Image (VSF3'Size));
--     Put_Line ("STV3_No_Size_Rep'Size " & Integer'Image (STV3_No_Size_Rep'Size));
--     Put_Line ("SFV3_No_Size_Rep'Size" & Integer'Image (SFV3_No_Size_Rep'Size));
--     Put_Line ("VST3_NS'Size " & Integer'Image (VST3_NS'Size));
--     Put_Line ("VSF3_NS'Size " & Integer'Image (VSF3_NS'Size));
--     --     Put_Line ("VV4'Size " & Integer'Image (VV4'Size));
--
--     Put_Line ("A_T'Size = " & Integer'Image (A_T'Size));
--     Put_Line ("VA'Size = " & Integer'Image (VA'Size));
--     Put_Line ("VA (1)'Size = " & Integer'Image (VA (1)'Size));
--     Put_Line ("VA (0 .. 2)'Size = " & Integer'Image (VA (0 .. 2)'Size));

   pragma Assert (Boolean'Size = 1);
   pragma Assert (neighbours'Size = 2);
   pragma Assert (Natural'Size = 31);
   pragma Assert (VSI'Size = 8);
   pragma Assert (VOS'size = 16);
   pragma Assert (VSIS'size = 16);
   pragma Assert (neigh_array'size = 4);
   pragma Assert (Vna'size = 8);
   pragma Assert (Vna (barbara)'size = 1);
   pragma Assert (Vna (barbara .. margo)'Size = 8);
   pragma Assert (Vna (barbara .. margo)'Component_Size = 1);
   pragma Assert (neigh_array_NS'size = 32);
   pragma Assert (VnaNS'size = 32);
   pragma Assert (VnaNS (barbara)'Size = 8);
   pragma Assert (VnaNS (barbara .. margo)'Size = 16);
   pragma Assert (VnaNS (barbara .. margo)'Component_Size = 8);
--     pragma Assert (S'Size = 88);

   pragma Assert (R1'Size = 64);
   pragma Assert (R2'Size = 132);
   pragma Assert (R3'Size = 66);
   pragma Assert (VR1'Size = 64);
   pragma Assert (VR2'Size = 136);
   pragma Assert (VR3'Size = 72);
   pragma Assert (r'Size = 16);
   pragma Assert (Vr'Size = 16);

   --  The following 3 assertions fail because the size is not known
   --  by the front-end (attribute_size has the value 0).
   --  A unsupported message is reported by gnat2goto.
   pragma Assert (R4'Size = 64);
   pragma Assert (R5'Size = 132);
   pragma Assert (R6'Size = 80);

   pragma Assert (VR4'Size = 64);
   pragma Assert (VR5'Size = 136);
   pragma Assert (VR6'Size = 72);

   --  The following 2 assertions fail because the size is not known
   --  by the front-end (attribute_size has the value 0).
   --  A unsupported message is reported by gnat2goto.
   pragma Assert (rNS'Size = 72);
   pragma Assert (VrNS'Size = 72);

   --  The following assertion fails because the size_attibute is applied
   --  to an indefinite type (without a size representation clause).
   --  The Ada RM states that the results of this are implementation
   --  dependent.  The front-end returns 0.
   --  Gnat2goto reports an unhandled node.
   pragma Assert (V1'Size = 40);

   pragma Assert (V2'Size = 39);
   pragma Assert (STV1'Size = 40);
   pragma Assert (SFV1'Size = 1);
   pragma Assert (VST'Size = 40);
   pragma Assert (VSF'Size = 8);
   --   pragma Assert (VV2'Size = 64);

   --  The following assertion fails because the size_attibute is applied
   --  to an indefinite type (without a size representation clause).
   --  The Ada RM states that the results of this are implementation
   --  dependent.  The front-end returns 0.
   --  Gnat2goto reports an unhandled node.
   pragma Assert (V3'Size = 64);

   --  The following assertion fails because the size is not known
   --  by the front-end (attribute_size has the value 0).
   --  A unsupported message is reported by gnat2goto.
   pragma Assert (V4'Size = 64);

   pragma Assert (STV3'Size = 64);
   pragma Assert (SFV3'Size = 8);
   pragma Assert (VST3'Size = 64);

   --  Not sure why the back end makes the VSF3'Size = 32 because
   --  its subtype has an attribute_value_size = 8
   --  which the compiler accepts.
   --  Gnat2goto has no choice but to use the attribute_value_size.
   pragma Assert (VSF3'Size = 32);
   --  The is the expected result follows:
   pragma Assert (VSF3'Size = 8);

   --  The following 4 assertions fail because the size is not known
   --  by the front-end (attribute_size has the value 0).
   --  A unsupported message is reported by gnat2goto.
   pragma Assert (STV3_No_Size_Rep'Size = 64);
   pragma Assert (SFV3_No_Size_Rep'Size = 8);
   pragma Assert (VST3_NS'Size = 64);
   pragma Assert (VSF3_NS'Size = 32);
   --   pragma Assert (VV2'Size = 64);

   pragma Assert (A_T'Size = 8);
   pragma Assert (VA'Size = 8);
   pragma Assert (VA (1)'Size = 2);
   pragma Assert (VA (0 .. 2)'Size = 8);

   pragma Assert (Vna'Component_size = 1);
   pragma Assert (neigh_array'component_size = 1);

 end Iss_131;
