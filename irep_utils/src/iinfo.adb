package body Iinfo is

   package body Trivial is

      function Trivial_Irep (Value : String) return Irep is
         ToRet : Irep;
      begin
         ToRet.Id := To_Unbounded_String (Value);
         return ToRet;
      end Trivial_Irep;

      function Trivial_Irep (Value : Integer) return Irep is
         ToRet : Irep;
      begin
         --  'Image will leave a space for positive values
         ToRet.Id := Trim (Source => To_Unbounded_String (Integer'Image (Value)),
                           Side   => Ada.Strings.Left);
         return ToRet;
      end Trivial_Irep;

      function Trivial_Irep (Value : Boolean) return Irep is
         ToRet : Irep;
      begin
         if Value then
            ToRet.Id := To_Unbounded_String ("1");
         else
            ToRet.Id := To_Unbounded_String ("0");
         end if;
         return ToRet;
      end Trivial_Irep;

   end Trivial;

   function Irep_To_Json (Ir : Irep) return JSON_Value is
      Ret_Sub : JSON_Array := Empty_Array;
      Ret_Named_Sub : JSON_Value := Create_Object;
      Ret_Comment : JSON_Value := Create_Object;
   begin
      for Sub of Ir.Sub loop
         Append (Ret_Sub, Irep_To_Json (Sub.All));
      end loop;
      for C in Ir.Named_Sub.Iterate loop
         Set_Field (Ret_Named_Sub, To_String (Irep_Maps.Key (C)), Irep_To_Json (Irep_Maps.Element (C).All));
      end loop;
      for C in Ir.Comment.Iterate loop
         Set_Field (Ret_Comment, To_String (Irep_Maps.Key (C)), Irep_To_Json (Irep_Maps.Element (C).All));
      end loop;

      return R : Json_Value := Create_Object do
        R.Set_Field ("id", Ir.Id);
        R.Set_Field ("sub", Ret_Sub);
        R.Set_Field ("namedSub", Ret_Named_Sub);
        R.Set_Field ("comment", Ret_Comment);
      end return;
   end Irep_To_Json;

   function Alloc_Clone (Ir : Irep) return Irep_Ptr is
   begin
      return new Irep'(Id =>        Ir.Id,
                       Sub =>       Ir.Sub,
                       Named_Sub => Ir.Named_Sub,
                       Comment =>   Ir.Comment);
   end Alloc_Clone;

end Iinfo;
