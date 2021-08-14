procedure Bug is
   X : Integer;

   type Array_Type is array (Integer range <>) of Integer;
   Var : Array_Type(1..1) := (others => 23);

   function F (V : Array_Type) return Integer is
   begin
      return V(1);
   end F;

begin
   X := F(Var);
   pragma Assert (X = 23);
end Bug;
