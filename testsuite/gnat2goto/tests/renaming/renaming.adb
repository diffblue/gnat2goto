with Renamed;
with Use_Types;
procedure Renaming is
   package P is
      function F (X : Integer) return Integer;
   end P;

   package body P is
      function F (X : Integer) return Integer is (X);
   end P;

   VP : Integer := P.F (7);

   package Q renames P;

   VQ : Integer := Q.F (3);

   use Q;

   VQ1 : Integer := F (5);

   use P;
   VP1 : Integer := F (VP);

   V : Integer;
   V_Renamed : Integer renames V;

   VG : Integer := Renamed.G (2);
   use Renamed;

   VG1 : Integer := G (1);

   use type Use_Types.My_Int;
   MV  : Use_Types.My_Int := 1;
   MV1 : Use_Types.My_Int := MV * 2;

   use Use_Types;

   MV2 : My_Int := MV + MV1;
begin
   pragma Assert (MV1 = 2);
   pragma Assert (MV2 = 3);
   V := 17;
   pragma Assert (V_Renamed = 17);
   V_Renamed := 19;
   --  This next check should fail as V==V_Renamed.
   pragma Assert (V = 17);
end Renaming;
