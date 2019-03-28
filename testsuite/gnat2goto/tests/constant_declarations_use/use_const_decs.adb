with Const_Decs; use Const_Decs;
procedure Use_Const_Decs is
   Loc : Integer := 40;
begin
   Loc := Loc + Static_Const;
   Var := Read_Only_Var + 1;
end Use_Const_Decs;
