package body Uint_To_Binary is

   function Convert_Uint_To_Binary (Input : Uint; Width : Pos) return String is
      --  we use 2's complement so we can just treat negative numbers the same
      --  as positive numbers
      Normalized_Input : Uint :=
          (if Input < Uint_0
           then Input + Uint_2 ** Width
           else Input);
      Result : String (1 .. Integer (Width)) := (others => '0');
   begin
      --  the resultant value should fit into the value range of the bitvector
      pragma Assert (Normalized_Input < Uint_2 ** Width);
      for I in Result'Range loop
         declare
            Power : constant Uint := Uint_2 ** (Width - Int (I));
         begin
            if Normalized_Input >= Power then
               Result (I) := '1';
               Normalized_Input := Normalized_Input - Power;
            end if;
         end;
      end loop;
      return Result;
   end Convert_Uint_To_Binary;

end Uint_To_Binary;
