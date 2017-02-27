
package body Irep is
   
   function TrivialIrep(Value : String) return Irep is 
      ToRet : Irep;
   begin
      ToRet.Id := To_Unbounded_String(Value);
      return ToRet;
   end;
   
   function TrivialIrep(Value : Integer) return Irep is 
      ToRet : Irep;      
   begin
      ToRet.Id := To_Unbounded_String(Integer'Image(Value));
      return ToRet;
   end;
   
   function TrivialIrep(Value : Boolean) return Irep is 
      ToRet : Irep;            
   begin
      if Value then
	 ToRet.Id := To_Unbounded_String("1");
      else
	 ToRet.Id := To_Unbounded_String("0");
      end if;
      return ToRet;
   end;  
   
   function Irep2Json(Ir : Irep) return JSON_Value is
      ToRet : JSON_Value := Create_Object;
   begin
      ToRet.Set_Field("id", Ir.Id);
      ToRet.Set_Field("sub", Ir.Sub);
      ToRet.Set_Field("namedSub", Ir.NamedSub);
      ToRet.Set_Field("comment", Ir.Comment);
      return ToRet;
   end;
   
end Irep;
