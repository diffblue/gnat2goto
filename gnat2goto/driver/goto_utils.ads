with Ireps; use Ireps;

package GOTO_Utils is

   --  Utility routines for high-level GOTO AST construction

   Pointer_Type_Width : constant Positive := 64;
   --  ??? this should be queried at runtime from GNAT

   function Make_Int_Type (Width : Positive) return Irep;

   function Make_Pointer_Type (Base : Irep) return Irep;

   function Make_Address_Of (Base : Irep) return Irep;

end GOTO_Utils;
