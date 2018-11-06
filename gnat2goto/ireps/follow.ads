with Ireps; use Ireps;
with Symbol_Table_Info; use Symbol_Table_Info;

package Follow is
   function Follow_Symbol_Type (I : Irep; ST : Symbol_Table) return Irep
     with Pre => ST.Contains (Intern (Get_Identifier (I)));
end Follow;
