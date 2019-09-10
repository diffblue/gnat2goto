procedure Test is
   type Unsigned_16 is mod 2**16;
   NUMBER_ADC_CHANNELS : constant := 2 ** 12;
   subtype Data_Channel_Number is Unsigned_16 range
     0 .. NUMBER_ADC_CHANNELS - 1;

   type Peak_Record_Type is
      record
--         Centre_Channel             : Data_Channel_Number;
         Low_Channel  : Data_Channel_Number;
         High_Channel : Data_Channel_Number;
      end record;

   Peak_Boundaries : Peak_Record_Type;

   Int_Var : Unsigned_16 := 0;

   procedure Foo (Peaks : Peak_Record_Type) is
   begin
      for I in Data_Channel_Number range
        Peaks.Low_Channel ..
        Peaks.High_Channel + 4 loop

         Int_Var := Int_Var + I;
      end loop;

      pragma Assert (Int_Var = 15);

   end Foo;
begin
   Peak_Boundaries.Low_Channel := 2;
   Peak_Boundaries.High_Channel := 8;

   Foo (Peak_Boundaries);
end Test;
