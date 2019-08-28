package Other is
   type Unsigned_8 is mod 2 ** 3;

   package SubOther is
      type Unsigned_16 is mod 2 ** 4;
   end SubOther;
end Other;
