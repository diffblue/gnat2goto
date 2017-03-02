#!/usr/bin/env python

import goto_json
import sys

if len(sys.argv) != 3:
    print >>sys.stderr, "Usage: irep_specs_to_ada.py output_file_prefix"
    sys.exit(1)

gj = goto_json.goto_json(sys.argv[1])
outsource = open(sys.argv[2] + ".ads", "w")
outbody = open(sys.argv[2] + ".adb", "w")

outsource.write("with Iinfo; use Iinfo;\n")
outsource.write("package Irep_Schemata is\n\n")
outbody.write("with GNATCOLL.JSON;         use GNATCOLL.JSON;\n\n")
outbody.write("package body Irep_Schemata is\n\n")

def toCamelCase(s):
    s = s[0].upper() + s[1:]
    idx = s.find("_")
    while idx != -1:
        s = s[:idx+1] + s[idx+1].upper() + s[idx+2:]
        idx = s.find("_", idx + 1)
    return s

def ada_type_from_schema(schema):
    if "type" in schema:
        if schema["type"] == "string":
            return "Unbounded_String"
        elif schema["type"] == "integer":
            return "Integer"
        elif schema["type"] == "boolean":
            return "Boolean"
        else:
            raise Exception("Unexpected type")
    else:
        return "Irep"

def ada_argument_conversion_from_schema(schema, value):
    if "type" in schema:
        return "Trivial_Irep(%s)" % value
    else:
        return value

def ada_from_schema(schema_name, schema):
    if "sub" in schema:
        for (i, sub) in enumerate(schema["sub"]):
            try:
                subname = sub["friendly_name"]
                if isinstance(subname, list):
                    subnames = subname
                else:
                    subnames = [subname]
            except KeyError:
                continue

            for subname in subnames:
                signature = "procedure Set_%s (Irep_To_Modify : %s, Value : %s)" % \
                            (toCamelCase(subname), schema_name, ada_type_from_schema(sub))
                outsource.write("%s;\n" % signature)
                outbody.write("%s is begin\n" % signature)
                outbody.write("  Set_Element(Irep_To_Modify.Sub, %d, Irep_To_Json(%s))\n" % \
                              (i + 1, ada_argument_conversion_from_schema(sub, "Value")))
                outbody.write("end;\n")

                if "number" in sub:
                    assert sub["number"] == "*"
                    signature = "procedure Set_%s (Irep_To_Modify : %s, Value : JSON_Array)" % \
                                (toCamelCase(subname) + "s", schema_name)
                    outsource.write("%s;\n" % signature)
                    outbody.write("%s is begin\n" % signature)
                    outbody.write("  Irep_To_Modify.Sub := Value;\n")
                    outbody.write("end;\n")

    if "parent" in schema:
        ada_from_schema(schema_name, schema["parent"])        

for (schema_name, schema) in gj.schemata.iteritems():

    schema_name = toCamelCase(schema_name)
    outsource.write("type %s is new Irep;\n" % schema_name)
    outsource.write("function Make_%s return %s;" % (schema_name, schema_name))
    outbody.write("function Make_%s return %s is\n  Ret : %s;\nbegin\n" % \
                  (schema_name, schema_name, schema_name));
    schema_id = gj.find_schema_id(schema)
    outbody.write("  Ret.id := Unbounded_String(\"%s\")\n" % schema_name)
    n_required_operands = gj.count_required_positional_operands(schema)
    outbody.write("  --  Add null values for required operands:\n")
    for i in range(n_required_operands):
        outbody.write("  Append (Ret.Sub, Irep_To_Json(Trivial_Irep(\"\")))\n")
    outbody.write("end;")
    
    ada_from_schema(schema_name, schema)

outsource.write("end Irep_Schemata;")
outbody.write("end Irep_Schemata;")
