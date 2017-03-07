
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Iinfo; use Iinfo;

package body Irep_Helpers is
   
   function To_Code_Block (Input : Irep_Code) return Irep_Code_Block is
      -- TODO: auto-synthesise getters
      Input_Statement : constant String := Get (Get (Input.Named_Sub, "statement"), "id");
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
