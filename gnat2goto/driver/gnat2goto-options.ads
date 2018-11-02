package GNAT2GOTO.Options is
   --  we want initialisation to happen and the specification needs to indicate
   --  that a package can have a body if the rest of the specification doesn't
   --  REQUIRE there to be one
   pragma Elaborate_Body;
   No_Dump_Statement_AST_On_Error_Option : constant String :=
     "no-dump-statement-ast-on-error";
   Dump_Statement_AST_On_Error : Boolean;
end GNAT2GOTO.Options;
