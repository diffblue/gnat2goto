procedure Repro is
   type Colours is (RED, BLACK);

   type VR (C : Colours := RED) is
      record
	 case C is
	    when BLACK =>
	       X : Integer;
	       Y : Integer;
	    when RED =>
	       W : Natural;
	       Z : Natural;
	 end case;
      end record;

   S : VR ( C => BLACK) :=
     ( C => BLACK,
       X => 1,
       Y => 2 );

   Output : Integer;
begin
   Output := S.X;
end Repro;
