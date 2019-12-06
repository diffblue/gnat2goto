procedure Subpackage_Local_Initialisers is
  package Subpackage is
    function Get_Value return Integer;
  end Subpackage;

  package body Subpackage is
    X : Integer := 42;
    function Get_Value return Integer is (X);
  end Subpackage;

  --  Alibi function to prevent naive constant folding
  function Double (X : Integer) return Integer is (X * 2);

begin
  pragma Assert (Subpackage.Get_Value = Double (21));
end Subpackage_Local_Initialisers;
