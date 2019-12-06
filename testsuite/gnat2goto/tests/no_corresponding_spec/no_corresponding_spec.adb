package body No_Corresponding_Spec with SPARK_Mode
is

    procedure Write_To_Uart (Msg : Unsigned_8)
    -- I don't know why but this global attribute
    -- seems to cause there to be no Corresponding_Spec
    -- attribute for this procedure
     with Global => (Output => Some_Register)
    is
    begin
        Some_Register := Msg;
    end;

    function To_Byte (X : Unsigned_16) return Unsigned_8 is
      (Unsigned_8 (X mod 256));

    procedure Foo is
    begin
        Write_To_Uart (To_Byte (Some_Constant));
    end Foo;
end No_Corresponding_Spec;
