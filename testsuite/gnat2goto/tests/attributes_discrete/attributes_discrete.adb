procedure Attributes_Discrete is
   --  Only for discrete types
   --  integers, modular, enums, etc.
   I: Integer := -30;
   type My_Enum is (Enum1, Enum2, Enum3);
begin
   pragma Assert (My_Enum'Pos(Enum1) = 2);  -- Wrong
   pragma Assert (My_Enum'Pos(Enum1) = 0);  -- Right
   pragma Assert (My_Enum'Pos(Enum3) = 2);

   pragma Assert (My_Enum'Val(0) = Enum1);  -- OK
   pragma Assert (My_Enum'Val(1) = Enum2);  -- OK
   pragma Assert (My_Enum'Val(2) = Enum3);  -- OK

   pragma Assert (Integer'Pos(I) = -30);
   pragma Assert (Integer'Val(I) = -30);
   pragma Assert (Integer'Pred(I) = -31);
   pragma Assert (Integer'Succ(I) = -29);
end Attributes_Discrete;
