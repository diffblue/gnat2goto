
--  Example borrowed from https://en.wikibooks.org/wiki/Ada_Programming/Types/record
procedure Variant_Record is
   
   type Traffic_Light is (Red, Yellow, Green);

   type Variant_Record (Option : Traffic_Light := Red) is 
      record
	 Location : Natural;
	 case Option is
	    when Red =>
	       Flashing : Boolean := True;
	    when Yellow =>
	       Timeout  : Positive := 1;
	    when Green =>
	       Whatever : Positive := 1;
	 end case;
      end record;
   
   Mutable_Traffic_Light   : Variant_Record;
   Immutable_Traffic_Light : Variant_Record (Option => Yellow);
   
begin
   Mutable_Traffic_Light   := (Option => Yellow,
			       Location => 54,     
			       Timeout => 5);
end Variant_Record;
   
   
