--  ASVAT Modelling
--  This package implements the features for ASVAT modelling described
--  in the presentation "ASVAT Modelling".
--  There are currently 3 sorts of model:
--  Nondet, Nondet_In_Type and Represents.
--  Nondet and Nondet_In_Type apply to subprograms and Represents is used to
--  model non-visible varaiables (and their type if this is also not visible).
--  Applying Nondet to a subprogram indicates that all of its outputs have
--  a nondeterministic value, including its return value if it is a function.
--  Applying Nondet_In_Type to a subprogam also indicates that all of its
--  outputs have a nondeterministic value but additionally each of the outputs
--  have a value which is within the values permitted for its type.
--  If the subprogram has outputs which are global variables these must be
--  specified using the SPARK 2014 Global aspect or Global pragma as either
--  mode In_Out or Out.
--  If the subprogram has no global outputs then it should have Global
--  aspect or pragma with Null or only Mode In variables.
--  Generally modelling subprograms will not have a body as they represent
--  features of the system that are not being directly analysed by ASVAT,
--  the very reason that a model of their behaviour is requred.
--  If the model is applied via an Import pragma the subprogram cannot
--  have a body (An Ada rule).  If the model is applied by an aspect then
--  then usually Import => True to the aspect specification.
--  Unless this is added to the aspect specification the subprogram Ada
--  requires the subprogram to have a body but it will be ignored by ASVAT.
--
--  Applying Represents to a declaration of a local variable indicates that the
--  variable represents a more global variable that is not visible in the
--  context of the local declaration.  Furthermore, if the type of the
--  non-visible variable a local type declaration can be used to represent
--  this too.
--  The local declaration of the variable (and its type, if the type of
--  the non-visible variable is also not visible) must exactly match the
--  declaration of the non-visible variable (and its type if necessary).
--  The purpose of these declarations is to represent global variables of
--  the subprogram which are indirectly updated by the subprogram but not
--  directly visible to it.
--
--  The models can be applied in one of two ways, by an ASVAT annotation or,
--  by using an Import pragma.  The ASVAT annotation is the preferred method.
--
--  Using the ASVAT annotation:
--  type My_Int is range 1 .. 100;
--
--  X : Integer;  --  A normal, visible variable.
--
--  I : Integer   --  Represents a variable that is not visible.
--  with Annotate => (ASVAT, Represents, "Hidden_Vars.I");
--
--  J : My_Int    --  Represents a non-visible variable whose type is also
--                -- not visible.
--  with Annotate => (ASVAT, Represents,
--                           "Hidden_Vars.J",      --  The non-visible variable
--                           "Hidden_Type.My_Int"  --  Its non-visible type
--                   );
--
--  The name of a modelling subprogram can be any legal Ada subprogram name
--  except operator names.
--
--  procedure Update
--  with Global   => (In_Out => (I, J, X)),
--       Annotate => (ASVAT, Nondet_In_Type),
--       Import   => True;
--
--  procedure Read
--  with Global   => (Out => I),
--       Annotate => (ASVAT, Nondet),
--       Import   => True;
--
--  function Nondet_Integer return Integer
--  with Global   => null,
--       Annotate => (ASVAT, Nondet);
--
--  Using pragma Import - the convention Ada indicates it is an ASVAT model:
--  type My_Int is range 1 .. 100;
--
--  X : Integer;  --  A normal, visible variable
--
--  I : Integer;  --  Represents a variable that is not visible.
--  pragma Import (Ada, I, "Represents", "Hidden_Vars.I");
--
--  J : My_Int;   --  Represents a non-visible variable whose type is also
--                -- not visible.
--  pragma Import (Convention    => Ada,
--                 Entity        => J,
--                 External_Name => "Represents",
--                 Link_Name     => "Hidden_Vars.J:Hidden_Type.My_Int");
--  --  Notice the hidden type follows the colon. No spaces allowed.
--  with Annotate => (ASVAT, Represents,
--                           "Hidden_Vars.J",      --  The non-visible variable
--                           "Hidden_Type.My_Int"  --  Its non-visible type
--                   );
--
--  The name of a modelling subprogram can be any legal Ada subprogram name
--  except operator names.
--
--  procedure Update
--  pragma Global ((In_Out => (I, J, X));
--  pragma Import (Ada, Update, "Nondet_In_Type");
--
--  procedure Read
--  pragma Global (Out => I);
--  pragma Import (Ada, Read, "Nondet");
--
--  function Nondet_Integer return Integer
--  pragma Global (null);
--  pragma Import (Ada, Non_Det_Integer, "Nondet");
--
--  Current limitations:
--  Can only mark as "In_Type" discrete variables and non-discriminated
--  records. Real variables and arrays are not handled.
--

with Types;                   use Types;
with Atree;                   use Atree;
with Sinfo;                   use Sinfo;
with Sem_Util;                use Sem_Util;
with Snames;                  use Snames;
with Ireps;                   use Ireps;
package ASVAT_Modelling is
   type Model_Sorts is
     (Not_A_Model, Nondet, Nondet_In_Type, Represents, Memcpy);
   subtype Valid_Model is Model_Sorts range Nondet .. Model_Sorts'Last;

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

   function Get_Annotation_Name (N : Node_Id) return String
   with Pre => Nkind (N) = N_Pragma and then
               Get_Pragma_Id (N) = Pragma_Annotate;

   function Get_Import_Convention (N : Node_Id) return String
   with Pre => Nkind (N) = N_Pragma and then
               Get_Pragma_Id (N) = Pragma_Import;

   function Get_Import_External_Name (N : Node_Id) return String
   with Pre => Nkind (N) = N_Pragma and then
               Get_Pragma_Id (N) = Pragma_Import;
   --  Returns null string if the External_Name parameter is not present.

   function Get_Import_Link_Name (N : Node_Id) return String
   with Pre => Nkind (N) = N_Pragma and then
               Get_Pragma_Id (N) = Pragma_Import;
   --  Returns null string if the Link_Name parameter is not present.

   function Get_Model_From_Anno (N : Node_Id) return Model_Sorts
   with Pre => Nkind (N) = N_Aspect_Specification;

   function Get_Model_From_Import (N : Node_Id) return Model_Sorts
   with Pre => Nkind (N) = N_Pragma and then Get_Pragma_Id (N) = Pragma_Import;

   function Get_Model_Sort (E : Entity_Id) return Model_Sorts;

   function Is_Model (Model : Model_Sorts) return Boolean is
     (Model /= Not_A_Model);

   procedure Make_Model (E : Entity_Id; Model : Model_Sorts);

   procedure Make_Nondet_Function (Fun_Name, Result_Type : String;
                                   Statements : Irep;
                                   E : Entity_Id);
   --  The Result_Type must be the "Unique_Name" of a declared type.
end ASVAT_Modelling;
