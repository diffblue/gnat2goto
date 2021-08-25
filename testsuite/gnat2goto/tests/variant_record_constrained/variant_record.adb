procedure Variant_Record is
   type V1 (B : Boolean) is record
      case B is
         when True =>
            I : Integer;
         when False =>
            C : Character;
      end case;
   end record;

   VR1 : V1 (True);
begin
   VR1.I := 2;
   null;
end Variant_Record;
