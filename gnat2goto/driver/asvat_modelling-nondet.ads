private package ASVAT_Modelling.Nondet is
   type Bound_Sort is (Lower, Highier);

   procedure Append_Nondet_Var (Var_Name : String;
                                Var_Type : Node_Id;
                                Block    : Irep;
                                Model    : Model_Sorts;
                                E        : Entity_Id);
   --  Appends a Nondet assignment to the given variable
   --  and if Model is Nondet_In_Type adds assumptions that the variable
   --  is in type.

   function Do_Nondet_Var (Var_Name, Var_Type : String;
                           E : Entity_Id) return Irep;
   --  Nondets the given variable.

   function Do_Var_In_Type (Var_Name, Var_Type  : String;
                            Var_Irep, Type_Irep : Irep;
                            E : Entity_Id;
                            Bound : Bound_Sort) return Irep;
   --  Marks as in type the given discrete variable.

   procedure Make_Selector_Names (Unique_Object_Name : String;
                                  Root_Irep : Irep;
                                  Block : Irep;
                                  Root_Type : Node_Id;
                                  E : Entity_Id;
                                  Loc : Irep);
   --  A provisional subprogram which recurses any non-discriminated record
   --  and marks its discrete components in type.
   --  The procedure will be replaced with a more general one which
   --  handles discriminated records and arrays.

end ASVAT_Modelling.Nondet;
