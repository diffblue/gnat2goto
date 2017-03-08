
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Iinfo; use Iinfo;
with Irep_Schemata; use Irep_Schemata;

package body Irep_Helpers is

   function To_Code_Block (Input : Irep_Code) return Irep_Code_Block is
      -- TODO: auto-synthesise getters
      Statement_Irep : constant Irep :=
        Irep_Maps.Element (Input.Named_Sub, To_Unbounded_String ("statement")).all;
      Input_Statement : constant Unbounded_String := Statement_Irep.Id;
   begin
      if (Input_Statement = "block") then
	 return Irep_Code_Block (Input);
      else
	 declare
	    Ret : Irep_Code_Block := Make_Irep_Code_Block;
	 begin
	    Add_Op (Ret, Irep (Input));
	    return Ret;
	 end;
      end if;
   end;
   
end Irep_Helpers;
