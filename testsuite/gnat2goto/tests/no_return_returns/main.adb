procedure Main is
  procedure Does_Not_Return (X : Integer) is
  begin
    loop
      if X < 10 then
        return;
      end if;
    end loop;
  end Does_Not_Return;
  pragma No_Return (Does_Not_Return);
begin
  Does_Not_Return (5);
end Main;
