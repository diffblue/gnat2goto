package body Iinfo is

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

   function Irep_To_Json (Ir : Irep) return JSON_Value is
      ToRet : JSON_Value := Create_Object;
   begin
      return R : Json_Value := Create_Object do
         R.Set_Field ("id", Ir.Id);
         R.Set_Field ("sub", Ir.Sub);
         R.Set_Field ("namedSub", Ir.Named_Sub);
         R.Set_Field ("comment", Ir.Comment);
      end return;
   end Irep_To_Json;

end Iinfo;
