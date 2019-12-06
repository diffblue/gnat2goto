package No_Corresponding_Spec with SPARK_Mode
is
    type Unsigned_8 is mod 2**8;
    type Unsigned_16 is mod 2**16;
    Some_Register : Unsigned_8;

    Some_Constant : constant Unsigned_16 := 22_027;
    procedure Foo;
end No_Corresponding_Spec;
