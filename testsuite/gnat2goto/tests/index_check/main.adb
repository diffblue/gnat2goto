procedure Main is
   type IndexArray is array(1..10) of Integer;
   AnArray : IndexArray := (others=>5);
begin
   for I in 1..12 loop
      AnArray(I):=6;
   end loop;

   pragma Assert(AnArray(3)=6);
end Main;
