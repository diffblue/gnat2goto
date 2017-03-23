with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Follow is
   function Follow_Symbol_Type (I : Irep; ST : Symbol_Table) return Irep is
      Next : Irep := I;
   begin
      while (Kind (Next) = I_Symbol_Type) loop
         Next := ST (To_Unbounded_String (Get_Identifier (Next))).SymType;
      end loop;
      return Next;
   end;
end Follow;
