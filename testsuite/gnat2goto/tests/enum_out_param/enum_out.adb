procedure Enum_Out is
   type Enum is (One, Two, Three);

   procedure P_Out (E : out Enum) is
   begin
      E := Two;
   end P_Out;

   Var_E : Enum;

begin
   P_Out (Var_E);
   pragma Assert (Var_E = Two);
end Enum_Out;
