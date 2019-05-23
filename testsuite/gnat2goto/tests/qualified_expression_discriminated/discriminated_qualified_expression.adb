procedure Discriminated_Qualified_Expression is
   type Variant_Record (Is_Integer : Boolean := True) is
      record
         case Is_Integer is
            when True  => Int  : Integer;
            when False => Bool : Boolean;
         end case;
      end record;
         
   subtype Int_Rec  is Variant_Record (Is_Integer => True);
   subtype Bool_Rec is Variant_Record (Is_Integer => False); 
   
   V1, V2, V3 : Variant_Record;
         
begin
   V1 := (True, 0);
   V2 := Int_Rec'(V1);  --  Should succeed
   V3 := Bool_Rec'(V1); --  Should fail
end Discriminated_Qualified_Expression;
   
