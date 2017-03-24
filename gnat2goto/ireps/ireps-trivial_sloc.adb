with Sinput; use Sinput;

separate (Ireps)
function Trivial_Sloc (S : Source_Ptr) return JSON_Value
is
   Source_Location : constant JSON_Value := Create_Object;
   N               : constant JSON_Value := Create_Object;

   SI : constant Source_File_Index := Get_Source_File_Index (S);
   F  : constant File_Name_Type    := File_Name (SI);
begin
   Source_Location.Set_Field ("id", "source_location");

   if S /= No_Location and S > Standard_Location then
      N.Set_Field ("file", Trivial_String (Get_Name_String (F)));
      N.Set_Field ("line",
                   Trivial_Integer (Integer (Get_Logical_Line_Number (S))));
      N.Set_Field ("column",
                   Trivial_Integer (Integer (Get_Column_Number (S))));
      Source_Location.Set_Field ("namedSub", N);
   end if;

   return Source_Location;
end Trivial_Sloc;
