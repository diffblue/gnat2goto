with Urealp; use Urealp;

package Ureal_To_Binary is

   function Convert_Ureal_To_Binary_Fixed (Number : Ureal;
                                           Max_Digits : Positive := 30)
                                          return String;
   function Convert_Ureal_To_Binary_IEEE (Number : Ureal;
                                         Fraction_Bits : Positive;
                                         Exponent_Bits : Positive;
                                         Exponent_Bias : Positive)
     return String
     with Post => (Convert_Ureal_To_Binary_IEEE'Result'Length
                     = Fraction_Bits + Exponent_Bits + 1);

   Integer_Part_Too_Large : exception;
   Exponent_Too_Large : exception;
   Negative_Exponent : exception;
end Ureal_To_Binary;
