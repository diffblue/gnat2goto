procedure Qualified_Expr is
  type Int10 is range 0..10;
  subtype Int25 is Int10 range 2..5;

  procedure Do_Loop (LB : Int10; UB: Int10) is
  begin
    for J in Int25'(LB) .. Int25'(UB) loop
      null;
    end loop;
  end Do_Loop;
begin
  --  We expect one of the lower bound to fail
  --  the range assertion check because of the
  --  qualified type.
  Do_Loop (1, 4);
end Qualified_Expr;
