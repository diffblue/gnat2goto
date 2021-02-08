with System.Storage_Elements;
procedure Rep_Clauses is

   type Small is range 1 .. 16;
   I : Integer;
   for I use at System.Storage_Elements.To_Address (16#FFF0#);

   type R is record
      S : Small;
   end record;
   for R use record
      S at 0 range 0 .. 3;
   end record;

   type Mix_Code is (ADD, SUB, MUL, LDA, STA, STZ);
   for Mix_Code use
     (ADD => 1, SUB => 2, MUL => 3, LDA => 8, STA => 24, STZ =>33);

begin
   null;
end Rep_Clauses;

