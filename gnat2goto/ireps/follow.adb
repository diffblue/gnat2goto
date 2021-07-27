--  with Types;
--  with Tree_Walk;     use Tree_Walk;
package body Follow is
   use Symbol_Maps;
   function Follow_Symbol_Type (I : Irep; ST : Symbol_Table) return Irep is
      Next : Irep := I;
   begin
      while Next /= 0 and then Kind (Next) = I_Symbol_Type loop
         declare
            Next_Sym_Id : constant Symbol_Id := Intern (Get_Identifier (Next));
            Next_Cursor : constant Cursor := ST.Find (Next_Sym_Id);
         begin
            if Next_Cursor /= No_Element then
               Next := Element (Next_Cursor).SymType;
            else
               --  This error is currently handled by
               --  Report_Unhandled_Node, but it is probably not
               --  the most satisfactory approach.
               --  In fact it causes a circularity.
               --  Needs fixing.
--                 Report_Unhandled_Node_Empty
--                   (N        => Types.Empty,
--                    Fun_Name => "Follow_Symbol_Type",
--                    Message  => "Symbol not found, "
--                   & Get_Identifier (Next));
               Next := 0;
            end if;
         end;
      end loop;
      return Next;
   end Follow_Symbol_Type;
end Follow;
