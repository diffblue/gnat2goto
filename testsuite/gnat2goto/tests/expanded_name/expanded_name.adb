with Withed_Package;
procedure Expanded_Name is
   N : Integer := 0;
begin
   Withed_Package.X := 1;
   case Withed_Package.X is
      when 1 =>
         N := Withed_Package.X + 1;
      when others =>
         N := 1;
   end case;
   pragma Assert ((if Withed_Package.X = 1 then N = 2 else N = 1));
end Expanded_Name;
