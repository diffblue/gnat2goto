package Private_Type is
   type Priv is private;
   Priv_Const : constant Priv;
private
   type Priv is new Integer range 1 .. 23;
   Priv_Const : constant Priv := 17;
end Private_Type;
