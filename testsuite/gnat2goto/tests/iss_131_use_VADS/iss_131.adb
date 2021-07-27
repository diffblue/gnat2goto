--  This example is run with the configuration pragma Use_VADS_Size.
--
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

   type A_T is array (SI) of SI;
   pragma Pack (A_T);

   VA : A_T := (others => 0);
   for VA'Size use 8;

begin
   pragma Assert (Boolean'Size = 8);
   pragma Assert (neighbours'Size = 8);
   pragma Assert (Natural'Size = 32);
   pragma Assert (VSI'Size = 2);
   pragma Assert (VOS'size = 16);
   pragma Assert (VSIS'size = 16);
   pragma Assert (neigh_array'size = 4);
   pragma Assert (Vna'size = 4);
   pragma Assert (Vna (barbara)'size = 1);
   --  The following 2 assertions fail with this configuration.
   --  The Slice'Size gives the unpacked result of 16.
   --  The Slice'Component_Size gives the upacked result of 8.
   --  The frontend does this despite pragma Implicit_Packing being in force.
   --  If pragma Pack to neigh_array is applied then these assertions succeed.
   pragma Assert (Vna (barbara .. margo)'Size = 2);
   pragma Assert (Vna (barbara .. margo)'Component_Size = 1);

   pragma Assert (neigh_array_NS'size = 32);
   pragma Assert (VnaNS'size = 32);
   pragma Assert (VnaNS (barbara)'Size = 8);
   pragma Assert (VnaNS (barbara .. margo)'Size = 16);
   pragma Assert (VnaNS (barbara .. margo)'Component_Size = 8);

   pragma Assert (R1'Size = 64);
   pragma Assert (R2'Size = 132);
   pragma Assert (R3'Size = 66);
   pragma Assert (VR1'Size = 64);
   pragma Assert (VR2'Size = 132);
   pragma Assert (VR3'Size = 66);
   pragma Assert (r'Size = 16);
   pragma Assert (Vr'Size = 16);

   pragma Assert (R4'Size = 64);
   
   --  The following assertion fails because ASVAT does not support
   --  the packing of arrays within records, so the unpacked size is 160.
   pragma Assert (R5'Size = 132);
   pragma Assert (R5'Size = 160);
   
   pragma Assert (R6'Size = 80);

   pragma Assert (VR4'Size = 64);
   pragma Assert (VR5'Size = 132);
   pragma Assert (VR6'Size = 66);

   pragma Assert (rNS'Size = 72);
   pragma Assert (VrNS'Size = 72);

   --  The following assertion fails because the size_attibute is applied
   --  to an indefinite type (without a size representation clause).
   --  The Ada RM states that the results of this are implementation
   --  dependent.
   --  Gnat2goto reports an unhandled node.
   pragma Assert (V1'Size = 40);

   pragma Assert (V2'Size = 39);
   pragma Assert (STV1'Size = 40);
   pragma Assert (SFV1'Size = 1);
   pragma Assert (VST'Size = 40);
   pragma Assert (VSF'Size = 1);
 
   --  The following assertion fails because the size_attibute is applied
   --  to an indefinite type (without a size representation clause).
   --  The Ada RM states that the results of this are implementation
   --  dependent.
   --  Gnat2goto reports an unhandled node.
   pragma Assert (V3'Size = 64);

   pragma Assert (V4'Size = 40);
   pragma Assert (STV3'Size = 64);
   pragma Assert (SFV3'Size = 8);
   pragma Assert (VST3'Size = 64);
   pragma Assert (VSF3'Size = 8);

   pragma Assert (STV3_No_Size_Rep'Size = 40);
   
   --  The following assertion fails because ASVAT does not support
   --  the packing of record types. The unpacked size is 40.
   pragma Assert (SFV3_No_Size_Rep'Size = 8);
   pragma Assert (SFV3_No_Size_Rep'Size = 40);
   
   pragma Assert (VST3_NS'Size = 40);

   --  The following assertion fails because ASVAT does not support
   --  the packing of record types. The unpacked size is 40.
   pragma Assert (VSF3_NS'Size = 8);
   pragma Assert (VSF3_NS'Size = 40);
 
   pragma Assert (A_T'Size = 8);
   pragma Assert (VA'Size = 8);
   pragma Assert (VA (1)'Size = 2);
   pragma Assert (VA (0 .. 2)'Size = 6);

   pragma Assert (Vna'Component_size = 1);
   pragma Assert (neigh_array'component_size = 1);

 end Iss_131;
