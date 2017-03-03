#!/usr/bin/env python

import goto_json
import sys

if len(sys.argv) != 3:
    print >>sys.stderr, "Usage: irep_specs_to_ada.py output_file_prefix"
    sys.exit(1)

indent = "   "

gj = goto_json.goto_json(sys.argv[1])
outspec = open(sys.argv[2] + ".ads", "w")
outbody = open(sys.argv[2] + ".adb", "w")

outspec.write("with Iinfo;         use Iinfo;\n")
outspec.write("with GNATCOLL.JSON; use GNATCOLL.JSON;\n")
outspec.write("\n")
outspec.write("package Irep_Schemata is\n")

outbody.write("with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;\n")
outbody.write("\n")
outbody.write("package body Irep_Schemata is\n")

def to_ada_identifier(s):

    if any(c != "_" and not c.isalpha() and not c.isdigit() for c in s):
        if s == "<":
            return "LT"
        elif s == ">":
            return "GT"
        elif s == "<=":
            return "LE"
        elif s == ">=":
            return "GE"
        elif s == "=":
            return "Equal"
        elif s == "!=":
            return "NotEqual"
        elif s == "+":
            return "Plus"
        elif s == "-":
            return "Minus"
        elif s == "*":
            return "Mult"
        elif s == "**":
            return "Exponentiate" 
        elif s == "/":
            return "Div"
        elif s == "unary-":
            return "UnaryMinus"
        else:
            raise Exception("Unhandled non-alphanum id '%s'" % s)
    
    s = s[0].upper() + s[1:]
    idx = s.find("_")
    while idx != -1:
        s = s[:idx+1] + s[idx+1].upper() + s[idx+2:]
        idx = s.find("_", idx + 1)
    return s

def ada_type_from_schema(schema):
    if "type" in schema:
        if schema["type"] == "string":
            return "String"
        elif schema["type"] == "integer":
            return "Integer"
        elif schema["type"] == "bool":
            return "Boolean"
        else:
            raise Exception("Unexpected type '%s'" % schema["type"])
    else:
        return "Irep"

def ada_argument_conversion_from_schema(schema, value):
    if "type" in schema:
        return "Trivial.Trivial_Irep(%s)" % value
    else:
        return value

def write_mutator_method(property_name, irep_type, value_type, body):
    
    signature = indent + \
                ("procedure Set_%s (Irep_To_Modify : in out %s; Value : %s)" % \
                 (to_ada_identifier(property_name), irep_type, value_type))

    outspec.write("%s;\n" % signature)

    outbody.write("\n%s is\n" % signature)
    outbody.write(indent + "begin\n")
    outbody.write(indent + indent + body + "\n")
    outbody.write(indent + "end;\n")
    
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
                body = "Set_Element (Irep_To_Modify.Sub, %d, Irep_To_Json (%s));" % \
                       (i + 1, ada_argument_conversion_from_schema(sub, "Value"))
                write_mutator_method(subname, schema_name, ada_type_from_schema(sub), body)

                if "number" in sub:
                    assert sub["number"] == "*"
                    body = "Irep_To_Modify.Sub := Value;"
                    write_mutator_method(subname + "s", schema_name, "JSON_Array", body)

    if "namedSub" in schema:
        for (propname, propschema) in schema["namedSub"].iteritems():
            if "constant" in propschema:
                continue
            body = "Set_Field (Irep_To_Modify.Named_Sub, \"%s\", Irep_To_Json (%s));" % \
                   (propname, ada_argument_conversion_from_schema(propschema, "Value"))
            write_mutator_method(propname, schema_name, ada_type_from_schema(propschema), body)
    if "comment" in schema:
        for (propname, propschema) in schema["comment"].iteritems():
            if "constant" in propschema:
                continue
            body = "Set_Field (Irep_To_Modify.Comment, \"%s\", Irep_To_Json (%s));" % \
                   (propname, ada_argument_conversion_from_schema(propschema, "Value"))
            write_mutator_method(propname, schema_name, ada_type_from_schema(propschema), body)

    if "parent" in schema:
        ada_from_schema(schema_name, schema["parent"])

def get_constant_assignments(schema):
    ret = []

    if "namedSub" in schema:
        for (propname, propschema) in schema["namedSub"].iteritems():
            if "constant" in propschema:
                ret.append("Set_Field (Ret.Named_Sub, \"%s\", Irep_To_Json (Trivial.Trivial_Irep(%s)));\n" % \
                           (propname, "\"%s\"" % propschema["constant"]))

    if "comment" in schema:
        for (propname, propschema) in schema["comment"].iteritems():
            if "constant" in propschema:
                ret.append("Set_Field (Ret.Comment, \"%s\", Irep_To_Json (Trivial.Trivial_Irep(%s)));\n" % \
                           (propname, "\"%s\"" % propschema["constant"]))
                           
    if "parent" in schema:
        ret.extend(get_constant_assignments(schema["parent"]))
    return ret

for (schema_name, schema) in gj.schemata.iteritems():

    schema_name = "Irep_" + to_ada_identifier(schema_name)

    outspec.write("\n")
    outspec.write(indent + ("type %s is new Irep;\n" % schema_name))
    outspec.write(indent + "function Make_%s return %s;\n" % (schema_name, schema_name))

    outbody.write("\n")
    outbody.write(indent + ("function Make_%s return %s is\n" % (schema_name, schema_name)))
    outbody.write(indent + indent + "Ret : %s;\n" % schema_name)
    outbody.write(indent + "begin\n")
    schema_id = gj.find_schema_id(schema)
    outbody.write(indent + indent + ("Ret.Id := To_Unbounded_String(\"%s\");\n" % schema_id))
    n_required_operands = gj.count_required_positional_operands(schema)
    
    if n_required_operands != 0:
        outbody.write(indent + indent + "--  Add null values for required operands\n")
    for i in range(n_required_operands):
        outbody.write(indent + indent + "Append (Ret.Sub, Irep_To_Json (Trivial.Trivial_Irep(\"\")));\n")
    constant_assignments = get_constant_assignments(schema)
    if len(constant_assignments) != 0:
        outbody.write(indent + indent + "--  Set constant members:\n")
    for c in constant_assignments:
        outbody.write(indent + indent + c)
    outbody.write(indent + indent + "return Ret;\n")
        
    outbody.write(indent + "end;\n")
    
    ada_from_schema(schema_name, schema)

outspec.write("\nend Irep_Schemata;")
outbody.write("\nend Irep_Schemata;")
