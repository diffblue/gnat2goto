procedure Array_Constraints is
   type Enum is (One, Two, Three, Four, Five, Six);
   subtype S_Enum is Enum range One .. Five;
   subtype S_Enum_No_Constraint is S_Enum;
   subtype My_Bool is Boolean;

   type My_Mod is Mod 2**8;
   subtype S_Mod_Constrained is My_Mod range 1 .. 127;
   subtype S_Mod_No_Constraint is My_Mod;

   subtype S_Constrained is Integer range 1 .. 10;
   subtype S_No_Constraint is S_Constrained;
   subtype S_No_Constraint_2 is S_No_Constraint;

   --  The following subtype declaration causes gnat2goto to report unsupported
   --  lower and upper range kinds and so has been coomented and placed in a
   --  separate test program
   --  subtype S_Char_Constrained is Character range 'a' .. 'z';
   subtype S_Char_No_Constraint is Character;

   type Constrained_Array_1 is array (Integer range 1 .. 10) of Integer;
   type Constrained_Array_2 is array (S_Constrained) of Integer;
   type Constrained_Array_3 is array (S_No_Constraint) of Integer;
   type Constrained_Array_4 is array (S_No_Constraint_2) of Integer;

   type Unconstrained_Array is array (Integer range <>) of Integer;
   type U_A_Enum is array (Enum range <>) of Integer;
   type U_A_Bool is array (Boolean range <>) of Integer;
   type U_A_Char is array (Character range <>) of Integer;
   type U_A_Mod  is array (My_Mod range <>) of Integer;

   subtype UA1 is Unconstrained_Array (S_Constrained);
   subtype UA2 is Unconstrained_Array (S_No_Constraint);
   subtype UA3 is Unconstrained_Array (S_No_Constraint_2);
   subtype UA4 is Unconstrained_Array (S_Constrained range 2 .. 9);

   subtype UA_Enum_1 is U_A_Enum (One .. Two);
   subtype UA_Enum_2 is U_A_Enum (Enum);
   subtype UA_Enum_3 is U_A_Enum (S_Enum);
   subtype UA_Enum_4 is U_A_Enum (S_Enum_No_Constraint);
   subtype UA_Enum_5 is U_A_Enum (Enum range Two .. Six);

   subtype UA_Bool_1 is U_A_Bool (False .. True);
   subtype UA_Bool_2 is U_A_Bool (Boolean);
   subtype UA_Bool_3 is U_A_Bool (My_Bool);
   subtype UA_Bool_4 is U_A_Bool (My_Bool range True .. True);

   subtype UA_Char_1 is U_A_Char ('A' .. 'Z');
   --   subtype UA_Char_2 is U_A_Char (S_Char_Constrained);
   subtype UA_Char_3 is U_A_Char (S_Char_No_Constraint);
   subtype UA_Char_4 is U_A_Char (Character);
   subtype UA_Char_5 is U_A_Char (Character range '0' .. '9');

   subtype UA_Mod_1 is U_A_Mod (My_Mod range 1 .. 10);
   subtype UA_Mod_2 is U_A_Mod (S_Mod_Constrained);
   subtype UA_Mod_3 is U_A_Mod (S_Mod_No_Constraint);
   subtype UA_Mod_4 is U_A_Mod (My_Mod);

   VUA1 : UA1;
   VUA2 : UA2;
   VUA3 : UA3;
   VUA4 : Unconstrained_Array (S_Constrained);
   VUA5 : Unconstrained_Array (S_No_Constraint);
   VUA6 : Unconstrained_Array (S_No_Constraint_2);
   VUA7 : Unconstrained_Array (1 .. 10);
   VUA8 : UA4;

   VCA1 : Constrained_Array_1;
   VCA2 : Constrained_Array_2;
   VCA3 : Constrained_Array_3;
   VCA4 : Constrained_Array_4;

   VEA1 : UA_Enum_1;
   VEA2 : UA_Enum_2;
   VEA3 : U_A_Enum (One .. Three);
   VEA4 : U_A_Enum (Enum);
   VEA5 : UA_Enum_4;
   VEA6 : UA_Enum_5;


   VBA1 : UA_Bool_1;
   VBA2 : UA_Bool_2;
   VBA3 : U_A_Bool (False .. True);
   VBA4 : U_A_Bool (Boolean);
   VBA5 : UA_Bool_3;
   VBA6 : UA_Bool_4;

   VAC1 : UA_Char_1;
   --  VAC2 : UA_Char_2;
   VAC3 : UA_Char_3;
   VAC4 : UA_Char_4;
   VAC5 : UA_Char_5;
   VAC6 : U_A_Char ('x' .. 'z');
   --  VAC7 : U_A_Char (S_Char_Constrained);
   VAC8 : U_A_Char (S_Char_No_Constraint);
   VAC9 : U_A_Char (S_Char_No_Constraint range 'a' .. 'f');

   VAM1 : UA_Mod_1;
   VAM2 : UA_Mod_2;
   VAM3 : UA_Mod_3;
   VAM4 : UA_Mod_4;
   VAM5 : U_A_Mod (My_Mod range 1 .. 10);
   VAM6 : U_A_Mod (S_Mod_Constrained);
   VAM7 : U_A_Mod (S_Mod_No_Constraint);
   VAM8 : U_A_Mod (My_Mod);
   VAM9 : U_A_Mod (1 .. 20);

begin
   VUA1 (1) := 1;
   VUA2 (2) := 2;
   VUA3 (3) := 3;
   VUA4 (4) := 4;
   VUA5 (5) := 5;
   VUA6 (6) := 6;
   VUA7 (7) := 7;
   VUA8 (8) := 8;
   pragma Assert (VUA1 (1) +
                    VUA2 (2) +
                    VUA3 (3) +
                    VUA4 (4) +
                    VUA5 (5) +
                    VUA6 (6) +
                    VUA7 (7) +
                    VUA8 (8) =
                    36);

   VCA1 (VCA1'Last) := 1;
   VCA2 (VCA2'Last - 1) := 2;
   VCA3 (VCA3'Last - 2) := 3;
   VCA4 (Constrained_Array_4'Last - 3) := 4;
   pragma Assert (VCA1 (VCA1'Last) +
                    VCA2 (VCA2'Last - 1) +
                    VCA3 (VCA3'Last - 2) +
                    VCA4 (Constrained_Array_4'Last - 3) =
                    10);

   VEA1 (One) := 1;
   VEA2 (Two) := 2;
   VEA3 (Three) := 3;
   VEA4 (Four) := 4;
   VEA5 (Five) := 5;
   VEA6 (Six) := 6;
   pragma Assert (VEA1 (One) + VEA2 (Two) + VEA3 (Three) +
                    VEA4 (Four) + VEA5 (Five) + VEA6 (Six) = 21);

   VBA1 (False) := 0;
   VBA2 (True) := 1;

   VBA3 (False) := 0;
   VBA4 (True) := 1;

   VBA5 (False) := 2;
   VBA6 (True) := 3;
   pragma Assert (VBA1 (False) + VBA2 (True) + VBA3 (False) + VBA4 (True) +
                    VBA5 (False) + VBA6 (True) = 7);

   VAC1 ('A') := 1;
   --  VAC2 ('a') := 2;
   VAC3 ('b') := 3;
   VAC4 ('c') := 4;
   VAC5 ('0') := 5;
   VAC6 ('x') := 6;
   --  VAC7 ('d') := 7;
   VAC8 ('e') := 8;
   VAC9 ('f') := 9;
   pragma Assert (VAC1 ('A') + --  VAC2 ('a') +
                    VAC3 ('b') + VAC4 ('c') +
                    VAC5 ('0') + VAC6 ('x') + -- VAC7 ('d') +
                    VAC8 ('e') + VAC9 ('f') = 36);

   VAM1 (1) := 1;
   VAM2 (2) := 2;
   VAM3 (3) := 3;
   VAM4 (4) := 4;
   VAM5 (5) := 5;
   VAM6 (6) := 6;
   VAM7 (7) := 7;
   VAM8 (8) := 8;
   VAM9 (9) := 9;
   pragma Assert (VAM1 (1) + VAM2 (2) + VAM3 (3) + VAM4 (4) +
                    VAM5 (5) + VAM6 (6) + VAM7 (7) + VAM8 (8) + VAM9 (9) = 45);

end Array_Constraints
;
