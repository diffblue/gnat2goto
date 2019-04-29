procedure Op_Mod_Expon is
  type Mod13 is mod 13;
  procedure Test (Six : Mod13) is
  begin
    pragma Assert (Six ** 1 = 6);
    pragma Assert (Six ** 2 = 10);
    pragma Assert (Six ** 3 = 8);
    pragma Assert (Six ** 4 = 9);
    pragma Assert (Six ** 5 = 2);
    pragma Assert (Six ** 6 = 12);
    pragma Assert (Six ** 7 = 7);
    pragma Assert (Six ** 8 = 3);
    pragma Assert (Six ** 9 = 5);
    pragma Assert (Six ** 10 = 4);
    pragma Assert (Six ** 11 = 11);
    pragma Assert (Six ** 12 = 1);
    pragma Assert (Six ** 13 = 6);
  end Test;
begin
  Test (6);
end Op_Mod_Expon;
