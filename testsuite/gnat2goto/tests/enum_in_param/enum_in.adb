procedure Enum_In is
   type Enum is (One, Two, Three, Four, Five, Six);

   subtype More_Than_Two is Enum range Three .. Six;

   type Number_Array_Type is array (1 .. 6) of Enum;

   Number_Array : constant Number_Array_Type :=
     (1 => One,
      2 => Two,
      3 => Three,
      4 => Four,
      5 => Five,
      6 => Six);

   procedure P_Enum_In (E : Enum) is
   begin
      if E > Two then
         pragma Assert (E > Two);
         pragma Assert (E in More_Than_Two);
      else
         pragma Assert (E <= Two);
         pragma Assert (E not in More_Than_Two);
      end if;
   end P_Enum_In;

begin
   pragma Assert (Number_Array (4) = Four);
   pragma Assert (Number_Array (2) = Two);
   P_Enum_In (Number_Array (4));
   P_Enum_In (Number_Array (2));
end Enum_In;

