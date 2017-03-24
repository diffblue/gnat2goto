separate (Ireps)
function Trivial_Sloc (S : Source_Ptr) return JSON_Value
is
   pragma Unreferenced (S);
begin
   return V : constant JSON_Value := Create_Object do
      V.Set_Field ("id", "source_location");
      V.Set_Field ("sub", Empty_Array);
      V.Set_Field ("comment", Create_Object);
      declare
         N : constant JSON_Value := Create_Object;
      begin
         N.Set_Field ("file", Trivial_String ("todo"));
         --  N.Set_Field ("working_directory", Trivial_String (""));
         N.Set_Field ("line", Trivial_Integer (42));
         N.Set_Field ("column", Trivial_Integer (666));
         --  N.Set_Field ("function", Trivial_String (""));
         --  N.Set_Field ("property_id", Trivial_String (""));
         --  N.Set_Field ("property_class", Trivial_String (""));
         --  N.Set_Field ("comment", Trivial_String (""));
         --  N.Set_Field ("java_bytecode_index", Trivial_String (""));

         V.Set_Field ("namedSub", N);
      end;
   end return;
end Trivial_Sloc;
