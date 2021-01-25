procedure Priv_Typ_Aggregate is
   package P is
      type Priv is private;

      type Lim_Priv is limited private;

      type Priv_Arr is private;
      type Lim_Arr is limited private;

      function Priv_Const return Priv;

      function Get_I_Priv (P : Priv) return Integer;
      function Get_J_Lim (P : Lim_Priv) return Integer;
      procedure Set_Lim_Priv (P : out Lim_Priv);
      function Const_Arr return Priv_Arr;
      procedure Init_Lim (L : out Lim_Arr);
      function Index (A : Priv_Arr; I : Integer) return Integer;
      function Index (A : Lim_Arr; I : Integer) return Integer;

   private

      type Priv is record
         I, J : Integer;
      end record;

      type Lim_Priv is record
         I, J : Integer;
      end record;

      type Priv_Arr is array (1 .. 5) of Integer;

      type Lim_Arr is array (1 .. 5) of Integer;

   end P;

   package body P is
      function Priv_Const return Priv is (3, 5);

      function Get_I_Priv (P : Priv) return Integer is (P.I);
      function Get_J_Lim (P : Lim_Priv) return Integer is (P.J);
      procedure Set_Lim_Priv (P : out Lim_Priv) is
      begin
         P := (7, 11);
      end Set_Lim_Priv;

      function Const_Arr return Priv_Arr is (1, 2, 3, 4, 5);
      procedure Init_Lim (L : out Lim_Arr) is
      begin
         L := (1, 2, 3, 4, 5);
      end Init_Lim;

      function Index (A : Priv_Arr; I : Integer) return Integer is (A (I));
      function Index (A : Lim_Arr; I : Integer) return Integer is (A (I));
   end P;
   use P;

   V_P : Priv := Priv_Const;
   V_L : Lim_Priv;

   A_P : Priv_Arr := Const_Arr;
   A_L : Lim_Arr;

begin
   Set_Lim_Priv (V_L);
   pragma Assert (Get_I_Priv (V_P) = 3);
   pragma Assert (Get_J_Lim (V_L) = 11);

   Init_Lim (A_L);
   pragma Assert (Index (A_P, 3) = 3);
   pragma Assert (Index (A_L, 5) = 5);
   null;
end Priv_Typ_Aggregate;
