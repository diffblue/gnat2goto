
with GNATCOLL.JSON;
with Text_IO;
with Irep;
procedure Test is 
   Ir : Irep.Irep;
begin
   Ir := Irep.TrivialIrep("example");
   Text_IO.Put_Line(Irep.Irep2Json(Ir).Write);
end;
