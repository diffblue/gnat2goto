
with Irep_Schemata; use Irep_Schemata;
with Symbol_Table_Info; use Symbol_Table_Info;

package Irep_Helpers is

   function To_Code_Block (Input : Irep_Code) return Irep_Code_Block;
   function Follow_Symbol_Type (Ty : Irep_Type; Symtab : Symbol_Table) return Irep_Type;

end Irep_Helpers;
