with External_Types;

--  A package renaming must occur within the declaration block of a subprocess
--  package ET renames External_Types;  --  this would be an error

procedure Declaration_Rename is
   A : Integer := 1;
   B : Integer;

   New_Integer_With_Complicated_Name : Integer := 11;
   Simply_Named_Int : Integer renames New_Integer_With_Complicated_Name;
   function My_Plus(Left, Right : Integer) return Integer renames "+";

   package ET renames External_Types;
   --  without a 'use' package clause (which would make the package renaming redundant)
   --  the operator must also be explicitly renamed
   --  (this operator is required for the 'pragma Assert'
   function "=" (Left, Right: ET.New_Integer) return Boolean renames ET."=";

   --  currently unsupported --
   --  Var : ET.New_Integer renames ET.External_Var;

   C : ET.New_Integer := 4;
begin
   A := A + 1;
   B := A + 1;

   pragma Assert (B=3);
   pragma Assert (Simply_Named_Int=11);
   pragma Assert (My_Plus(A,B)=5);
   pragma Assert (C=4);
   
   --  currently unsupported --
   --  Var := 5;
   --  pragma Assert (Var=5);
end Declaration_Rename;
