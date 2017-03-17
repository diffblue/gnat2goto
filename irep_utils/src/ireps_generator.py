#!/usr/bin/env python

"""
   This is Florian's attempt at making an sinfo-like ireps package based
   on GNAT.Tables. It is work in progress and until it's done we will keep
   using irep_specs_to_ada.
"""

# Special todo:
# * code_ifthenelse we need to deal with the "optional" flag
# * recognize and remove Argument_List

import os
import sys
import json

from pprint import pprint
from glob import glob
from copy import copy

def get_schema_files():
    try:
        assert len(sys.argv) == 2
        CBMC_ROOT = sys.argv[1]
        assert os.path.isdir(CBMC_ROOT)
        assert os.path.isfile(os.path.join(CBMC_ROOT, "README.md"))
        return glob(os.path.join(CBMC_ROOT, "src", "util", "irep_specs",
                                 "*.json"))
    except:
        print "Usage: %s <PATH_TO_CBMC_ROOT>" % sys.argv[0]
        sys.exit(1)

def ada_casing(s):
    rv = ""
    seen_underscore = True
    for c in s:
        if c == "_":
            seen_underscore = True
            rv += c
        elif seen_underscore:
            seen_underscore = False
            rv += c.upper()
        else:
            rv += c.lower()
    return rv

def ada_setter_name(name, is_list):
    if is_list:
        return "%s_Append" % ada_casing(name)
    else:
        return "Set_%s" % ada_casing(name)

def ada_component_name(layout_kind, layout_id):
    return "%s_%u" % ({"str" : "String",
                       "int" : "Int",
                       "bool" : "Bool"}[layout_kind],
                      layout_id)

def write_file(fn, instructions):
    indent = 0
    with open(fn, "w") as fd:
        for i in instructions:
            if i["kind"] == "indent":
                indent += 1
            elif i["kind"] == "outdent":
                indent -= 1
            else:
                assert i["kind"] == "text"
                txt = "   " * indent + i["text"]
                fd.write(txt.rstrip() + "\n")
    assert indent == 0

def indent(f):
    f.append({"kind" : "indent"})

def outdent(f):
    f.append({"kind" : "outdent"})

def write(f, txt):
    f.append({"kind" : "text",
              "text" : str(txt)})

def current_indent(f):
    indent = 0
    for v in f:
        if v["kind"] == "indent":
            indent += 1
        elif v["kind"] == "outdent":
            indent -= 1
    return indent

def continuation(f):
    # Merge the last two lines if it doesn't make them too long
    indent = current_indent(f)
    tmp = f[-2]["text"] + " " + f[-1]["text"].strip()
    if 3 * indent + len(tmp) < 80:
        del f[-1]
        f[-1]["text"] = tmp

def mk_prefixed_lines(prefix, lines):
    # Prefix the first line with prefix, and everything else by a suitable
    # number of spaces.
    assert len(lines) > 0
    rv = [prefix + lines[0]]
    for line in lines[1:]:
        rv.append(" " * len(prefix) + line)
    return rv

def main():
    special_names = {"+"      : "op_add",
                     "-"      : "op_sub",
                     "*"      : "op_mul",
                     "/"      : "op_div",
                     "**"     : "op_exp",
                     "="      : "op_eq",
                     ">"      : "op_gt",
                     "<"      : "op_lt",
                     ">="     : "op_geq",
                     "<="     : "op_leq",
                     "unary-" : "op_neg",
                 }

    schemata = {}
    for schema_fn in get_schema_files():
        with open(schema_fn, "rU") as fd:
            sn = os.path.splitext(os.path.basename(schema_fn))[0]
            # print "Loading %s" % sn
            schemata[sn] = json.load(fd)
            schemata[sn]["subclasses"] = set()
            schemata[sn]["ada_name"] = ada_casing("i_" + sn)

    # Schemata will contain the following. Things in <> we synthesise below.
    #   id           : string constant that identifies the kind
    #   parent       : name of parent class
    #   sub          : list of irep
    #   namedSub     : dict of string -> irep
    #   comment      : dict of string -> irep
    #   <subclasses> : set of classes where parent refers to this class
    #   <ada_name>   : name of irep_kind enum
    #   <used>       : true if node we actually want to produce


    # Subs are dicts:
    #   friendly_name : string; or list of strings
    #   schema        : what kind of irep to expect here
    #   number        : [optional] '*' if set the entire sub acts as a list

    # NS is a dict
    #   type : 'integer', 'string'
    # or
    #   sub : list of subs

    # Comment
    # class -> constant -> 1

    # Restore full hierarchy
    for sn, schema in schemata.iteritems():
        parent = schema.get("parent", None)
        if parent is None and sn != "irep":
            parent = "irep"
            schema["parent"] = parent
        if parent is not None:
            schemata[parent]["subclasses"].add(sn)

    # Expand trivial subclasses
    to_add = {}
    for sn, schema in schemata.iteritems():
        for sc_id in schema.get("trivial_subclass_ids", []):
            if sc_id in special_names:
                sc_name = special_names[sc_id]
                ada_name = ada_casing("i_" + sc_name)
            else:
                sc_name = sc_id
                ada_name = ada_casing("i_op_" + sc_name)
            new_schema = copy(schema)
            new_schema["id"] = sc_id
            new_schema["ada_name"] = ada_name
            del new_schema["trivial_subclass_ids"]
            new_schema["parent"] = sn
            new_schema["subclasses"] = set()
            schema["subclasses"].add(sc_name)
            to_add[sc_name] = new_schema
        if "trivial_subclass_ids" in schema:
            del schema["trivial_subclass_ids"]
    schemata.update(to_add)

    # Flag nodes that will be supported
    for sn, schema in schemata.iteritems():
        schema["used"] = (len(schema["subclasses"]) == 0 or
                          sn in ("struct_type", "pointer_type"))
        if sn == "source_location":
            # We will be using the GNAT ones instead
            schema["used"] = False

    def all_used_subclasses(sn):
        """ return the set of all subclasses of sn (and itself) """
        rv = set()
        for sc in schemata[sn]["subclasses"]:
            rv |= all_used_subclasses(sc)
        if schemata[sn]["used"]:
            rv.add(sn)
        return rv

    # Debug output of hierarchy
    with open("tree.dot", "w") as fd:
        fd.write("digraph G {\n")
        fd.write("graph [rankdir=LR,ranksep=3];\n")
        for sn in sorted(schemata):
            atr = []
            lbl = schemata[sn].get("id", None)
            if lbl is None or lbl == "":
                lbl = sn
            if lbl != sn:
                atr.append('label="%s"' % lbl)
            if not schemata[sn]["used"]:
                atr.append("fontcolor=red")
                atr.append("shape=none")
            fd.write(sn)
            if len(atr) > 0:
                fd.write(' [%s];' % ",".join(atr))
            fd.write("\n")
        for sn, schema in schemata.iteritems():
            for sc in schema["subclasses"]:
                fd.write('%s -> %s;\n' % (sn, sc))
        fd.write("}\n")
    os.system("dot tree.dot -Tpdf > tree.pdf")

    # Emit spec and body file
    s = []
    b = []
    write(s, "with Types; use Types;")  # Source_Ptr
    write(s, "")

    write(b, "with Table;")
    write(b, "with Alloc;   use Alloc;") # for Nodes_Initial
    write(b, "with Stringt; use Stringt;") # String_Id
    write(b, "")

    write(s, "package Ireps is")
    write(s, "")
    indent(s)

    write(b, "package body Ireps is")
    write(b, "")
    indent(b)

    ##########################################################################
    # Types and subtypes

    write(s, "type Irep is range 0 .. Integer'Last;")
    write(s, "Empty : constant Irep := 0;")
    write(s, "")

    write(s, "type Irep_List is range Integer'First .. 0;");
    # write(s, "Empty : constant Irep_List := 0;")
    write(s, "")

    # Emit kind enum
    top_sorted_sn = []
    prefix = "type Irep_Kind is ("
    prefix_len = len(prefix)
    write(s, prefix + "I_Empty, --  For the Empty Irep")
    def rec(sn, depth=0):
        if schemata[sn]["used"]:
            write(s, " " * prefix_len + schemata[sn]["ada_name"] + ",")
            top_sorted_sn.append(sn)
        for sc in sorted(schemata[sn]["subclasses"]):
            rec(sc, depth+1)
    rec("irep")
    s[-1]["text"] = s[-1]["text"].rstrip(",") + ");"
    write(s, "")

    write(s, "subtype Valid_Irep_Kind is Irep_Kind")
    write(s, "  range Irep_Kind'Succ (Irep_Kind'First) .. Irep_Kind'Last;")
    write(s, "")

    # Emit subclasses for the enum
    summary_classes = {}
    def make_class(root):
        name = schemata[root]["ada_name"].replace("I_", "Class_")
        subc = sorted(schemata[root]["subclasses"])
        first = subc[0]
        last = subc[-1]
        while len(schemata[last]["subclasses"]) >= 1:
            subc = sorted(schemata[last]["subclasses"])
            last = subc[-1]
        write(s, "subtype %s is Irep_Kind" % name)
        write(s, "  range %s .. %s;" % (schemata[first]["ada_name"],
                                        schemata[last]["ada_name"]))
        continuation(s)
        schemata[root]["subclass_ada_name"] = name
        summary_classes[name] =\
          set(top_sorted_sn[top_sorted_sn.index(first) :
                            top_sorted_sn.index(last) + 1])
    make_class("unary_expr")
    make_class("binary_expr")
    make_class("nary_expr")
    make_class("code")
    make_class("bitvector_type")
    make_class("expr")
    make_class("type")
    write(s, "")

    def mk_precondition_in(param_name, kinds):
        todo = set(kinds)
        groups = []
        def rec(kind, todo):
            group_name = schemata[kind].get("subclass_ada_name", None)
            if group_name is not None:
                if summary_classes[group_name] <= todo:
                    todo -= summary_classes[group_name]
                    groups.append(group_name)
            for sc in schemata[kind]["subclasses"]:
                rec(sc, todo)
        rec("irep", todo)
        things = sorted(groups + [schemata[x]["ada_name"] for x in todo])
        assert len(things) >= 1
        if len(things) == 1 and things[0].startswith("I_"):
            rv = ["Kind (%s) = %s" % (param_name, things[0])]
        else:
            prefix = "Kind (%s) in " % param_name
            prefix_len = len(prefix) - 2
            rv = [prefix + things[0]]
            for thing in things[1:]:
                rv.append(" " * prefix_len + "| " + thing)
        return rv

    # Collect and consolidate setters (subs, named and comment)

    sub_setters = {} # setter_name -> value|list -> {schema: (op_id, type)}
    def register_sub_setter(root_schema,
                            op_id, friendly_name,
                            value_schema,
                            is_list):
        if type(friendly_name) is list:
            assert len(friendly_name) == 2
            assert friendly_name[0] == "op%u" % op_id
            friendly_name = friendly_name[1]
        if friendly_name == "op" and root_schema == "replication_expr":
            # Hack to resolve some name clash
            friendly_name = "to_replicate"

        schema = schemata[root_schema]
        setter_kind = "list" if is_list else "value"

        if friendly_name not in sub_setters:
            sub_setters[friendly_name] = {}
        if setter_kind not in sub_setters[friendly_name]:
            sub_setters[friendly_name][setter_kind] = {}
        sub_setters[friendly_name][setter_kind][root_schema] = (op_id,
                                                                value_schema)

        # Also apply to all children
        for sc in schema.get("subclasses", None):
            register_sub_setter(sc, op_id, friendly_name, value_schema, is_list)

    named_setters = {}
    # setter_name -> value|list|trivial -> {schema: (is_comment, type)}
    def register_named_setter(root_schema,
                              kind,
                              friendly_name, value_type,
                              is_comment):
        schema = schemata[root_schema]
        assert kind in ("trivial", "irep", "list")
        assert not kind == "trivial" or value_type in ("bool",
                                                       "string",
                                                       "integer")
        assert not kind != "trivial" or value_type in schemata

        actual_kind = kind
        actual_type = value_type
        if kind == "irep" and value_type == "source_location":
            # We magically map GNAT source locations to CPROVER source
            # locations
            actual_kind = "trivial"
            actual_type = "gnat:sloc"

        if friendly_name not in named_setters:
            named_setters[friendly_name] = {}
        if actual_kind not in named_setters[friendly_name]:
            named_setters[friendly_name][actual_kind] = {}
        named_setters[friendly_name][actual_kind][root_schema] = (is_comment,
                                                                  actual_type)

        # Also apply to all children
        for sc in schema.get("subclasses", None):
            register_named_setter(sc,
                                  kind,
                                  friendly_name, value_type,
                                  is_comment)

    const = {}
    # cnst ::= schema -> id|namedSub|comment -> {name: value}
    def register_constant(root_schema, kind, friendly_name, string_value):
        if root_schema not in const:
            const[str(root_schema)] = {}
        if kind not in const[root_schema]:
            const[root_schema][kind] = {}
        const[root_schema][kind][friendly_name] = string_value

        # Also apply to all children
        for sc in schemata[root_schema].get("subclasses", None):
            register_constant(sc,
                              kind,
                              friendly_name, string_value)

    def rec(sn):
        if sn == "source_location":
            return

        schema = schemata[sn]

        tmp = copy(schema)

        del tmp["used"]
        del tmp["ada_name"]
        del tmp["subclasses"]
        if "parent" in schema:
            del tmp["parent"]
        if "subclass_ada_name" in schema:
            del tmp["subclass_ada_name"]

        if "id" in schema:
            del tmp["id"]
            register_constant(sn, "id", "id", schema["id"])

        if "sub" in schema:
            del tmp["sub"]
        for i, sub in enumerate(schema.get("sub", [])):
            if "sub" in sub:
                # Op_i is a list
                assert type(sub["sub"]) is list
                assert len(sub["sub"]) == 1
                list_schema = sub["sub"][0]
                assert list_schema.get("number", None) == "*"
                assert "schema" in list_schema

                friendly_name = list_schema["friendly_name"]
                element_type  = list_schema["schema"]
                register_sub_setter(sn, i, friendly_name, element_type, True)

            elif "friendly_name" in sub:
                friendly_name = sub["friendly_name"]
                register_sub_setter(sn,
                                    i, friendly_name,
                                    sub["schema"],
                                    sub.get("number", None) == "*")

            elif "number" in sub:
                assert sub["number"] == "*"
                friendly_name = "elmt" # TODO: should have a nicer name
                element_type  = sub["schema"]
                register_sub_setter(sn, i, friendly_name, element_type, True)

        for fld in ("namedSub", "comment"):
            if fld in schema:
                del tmp[fld]
            for friendly_name, data in schema.get(fld, {}).iteritems():
                if "constant" in data:
                    # A specific string constant that must be set by the
                    # constructor
                    assert len(data) == 1 or (len(data) == 2 and
                                              data["type"] == "string")
                    const_value = data["constant"]
                    register_constant(sn,
                                      fld,
                                      friendly_name, const_value)

                elif data.get("type", None) in ("string", "integer", "bool"):
                    # Trivial field
                    assert len(data) == 1
                    register_named_setter(sn,
                                          "trivial",
                                          friendly_name, data["type"],
                                          fld == "comment")

                elif "schema" in data:
                    # Irep of some type
                    assert data["schema"] in schemata
                    assert len(data) == 1
                    value_type = data["schema"]
                    register_named_setter(sn,
                                          "irep",
                                          friendly_name, value_type,
                                          fld == "comment")

                elif "sub" in data:
                    # A list
                    assert len(data) == 1
                    data = data["sub"]
                    assert len(data) == 1
                    data = data[0]
                    assert len(data) == 3
                    assert data["number"] == "*"
                    friendly_name = data["friendly_name"]
                    list_type     = data["schema"]
                    register_named_setter(sn,
                                          "list",
                                          friendly_name, list_type,
                                          fld == "comment")

                else:
                    assert False

        # cnst ::= schema -> id|namedSub|comment -> {name: value}
        # namd ::= setter_name -> value|list|trivial -> {schema: (is_comment, type)}
        # Delete setters for which we have a constant
        for kind in ("namedSub", "comment"):
            data = const.get(sn, {}).get(kind, {})
            for friendly_name, const_value in data.iteritems():
                if (friendly_name in named_setters and
                    "trivial" in named_setters[friendly_name] and
                    sn in named_setters[friendly_name]["trivial"]):
                    del named_setters[friendly_name]["trivial"][sn]

        if len(tmp) > 0:
            print "error: unconsumed data for %s:" % sn
            for item, data in tmp.iteritems():
                print "   %s: %s" % (item, data)

        for sc in schema.get("subclasses", None):
            rec(sc)

    rec("irep")

    # Delete setters that only touch non-used classes (maybe we removed
    # some because they are always constant)
    setters_to_kill = []
    for setter_name in named_setters:
        kinds_to_kill = []
        for kind in named_setters[setter_name]:
            all_unused = True
            for sn in named_setters[setter_name][kind]:
                if schemata[sn]["used"]:
                    all_unused = False
                    break
            if all_unused:
                kinds_to_kill.append(kind)
        for kind in kinds_to_kill:
            del named_setters[setter_name][kind]
        if len(named_setters[setter_name]) == 0:
            setters_to_kill.append(setter_name)
    for setter_name in setters_to_kill:
        del named_setters[setter_name]

    ##########################################################################
    # Diagnostics after parsing schemata

    for setter_name, data in sub_setters.iteritems():
        if len(data) > 1:
            print "sub setter", setter_name, "conflicting kinds"
            pprint(data)

    for setter_name in (set(sub_setters) & set(named_setters)):
        print "both a sub and named:", setter_name
        print "> sub in  :", ", ".join(set(list(sub_setters[setter_name].itervalues())[0]))
        print "> named in:", ", ".join(set(list(named_setters[setter_name].itervalues())[0]))

    ##########################################################################
    # Layout

    op_counts = {}
    # schema -> int|str|bool
    #    where int includes irep, list, trivial integer

    layout = {}
    # schema -> friendly_name -> (str|int|bool|sloc, index)

    for sn in top_sorted_sn:
        op_counts[sn] = {"int"  : 0,
                         "str"  : 0,
                         "bool" : 0}
        layout[sn] = {}

        for setter_name, data in sub_setters.iteritems():
            assert len(data) == 1
            assert "value" in data or "list" in data
            for kind, variants in data.iteritems():
                if sn in variants:
                    layout[sn][setter_name] = ("int", op_counts[sn]["int"])
                    op_counts[sn]["int"] += 1

        for setter_name, setter_kinds in named_setters.iteritems():
            for kind in setter_kinds:
                if sn in setter_kinds[kind]:
                    is_comment, typ = setter_kinds[kind][sn]
                    if kind in ("irep", "list") or typ == "integer":
                        layout[sn][setter_name] = ("int", op_counts[sn]["int"])
                        op_counts[sn]["int"] += 1
                    elif typ == "string":
                        layout[sn][setter_name] = ("str", op_counts[sn]["str"])
                        op_counts[sn]["str"] += 1
                    elif typ == "bool":
                        layout[sn][setter_name] = ("bool", op_counts[sn]["bool"])
                        op_counts[sn]["bool"] += 1
                    elif typ == "gnat:sloc":
                        pass
                    else:
                        print sn, setter_name, kind, typ
                        assert False

    ##########################################################################
    # Documentation

    for sn in top_sorted_sn:
        schema = schemata[sn]
        assert schema["used"]

        write(s, "--  %s" % schema["ada_name"])

        # sub_setters ::= setter_name -> value|list -> {schema: (op_id, type)}
        # subs        ::= op_id -> (setter_name, type)
        subs = {}
        for setter_name, data in sub_setters.iteritems():
            assert len(data) == 1
            for typ, variants in data.iteritems():
                actual_type = {"value" : "irep",
                               "list"  : "list"}[typ]
                if sn in variants:
                    subs[variants[sn][0]] = (ada_casing(setter_name),
                                             actual_type)
        if len(subs):
            write(s, "--  subs")
            for op in xrange(len(subs)):
                assert op in subs
                write(s, "--    %s (op%u, %s)" % (subs[op][0],
                                                  op,
                                                  subs[op][1]))

        nams = {}
        coms = {}
        for setter_name, setter_kinds in named_setters.iteritems():
            for kind in setter_kinds:
                if sn in setter_kinds[kind]:
                    is_comment, typ = setter_kinds[kind][sn]
                    d = coms if is_comment else nams
                    if kind == "trivial":
                        d[setter_name] = typ
                    else:
                        d[setter_name] = kind
        if len(nams):
            write(s, "--  namedSubs")
            for setter_name in sorted(nams):
                write(s, "--    %s (%s)" % (ada_casing(setter_name),
                                            nams[setter_name]))
        if len(coms):
            write(s, "--  comment")
            for setter_name in sorted(coms):
                write(s, "--    %s (%s)" % (ada_casing(setter_name),
                                            coms[setter_name]))

        # cnst ::= schema -> id|namedSub|comment -> {name: value}
        cons = {}
        for kind in const.get(sn, {}):
            for const_name, const_value in const[sn][kind].iteritems():
                tmp = "constant %s: %s" % (ada_casing(const_name), const_value)
                if kind != "id":
                    tmp += " (%s)" % kind
                write(s, "--  %s" % tmp)

        write(s, "")

    write(s, "")

    ##########################################################################
    # Datastructure

    write(b, "type Irep_Node is record")
    components = [("Kind", "Valid_Irep_Kind", None),
                  ("Sloc", "Source_Ptr", "No_Location")]
    for i in xrange(max(x["int"] for x in op_counts.itervalues())):
        components.append(("Int_%u" % i, "Integer", "0"))
    for i in xrange(max(x["str"] for x in op_counts.itervalues())):
        components.append(("String_%u" % i, "String_Id", "Null_String_Id"))
    for i in xrange(max(x["bool"] for x in op_counts.itervalues())):
        components.append(("Bool_%u" % i, "Boolean", "False"))
    indent(b)
    max_len = max(map(len, (x[0] for x in components)))
    for cname, ctyp, default in components:
        if default is None:
            write(b, "%-*s : %-15s;" % (max_len, cname, ctyp))
        else:
            write(b, "%-*s : %-15s := %s;" % (max_len, cname, ctyp, default))
    outdent(b)
    write(b, "end record;")
    write(b, "pragma Pack (Irep_Node);")
    write(b, "")

    # write(b, "subtype Valid_Irep is Irep range 1 .. Irep'Last")
    # write(b, "")

    write(b, "package Irep_Table is new Table.Table")
    write(b, "  (Table_Component_Type => Irep_Node,")
    write(b, "   Table_Index_Type     => Irep,")
    write(b, "   Table_Low_Bound      => 1,")
    write(b, "   Table_Initial        => Nodes_Initial, --  seems like a good guess")
    write(b, "   Table_Name           => \"Irep_Table\");")
    write(b, "")


    ##########################################################################
    # API

    write(s, "function Kind (I : Irep) return Irep_Kind;")
    write(s, "")

    write(b, "function Kind (I : Irep) return Irep_Kind")
    write(b, "is")
    write(b, "begin")
    indent(b)
    write(b, "if I = Empty then")
    indent(b)
    write(b, "return I_Empty;")
    outdent(b)
    write(b, "else")
    indent(b)
    write(b, "return Irep_Table.Table (I).Kind;")
    outdent(b)
    write(b, "end if;")
    outdent(b)
    write(b, "end Kind;")
    write(b, "")

    write(s, "function New_Irep (Kind : Valid_Irep_Kind) return Irep;")
    write(s, "")

    write(b, "function New_Irep (Kind : Valid_Irep_Kind) return Irep")
    write(b, "is")
    indent(b)
    write(b, "I : Irep_Node;")
    outdent(b)
    write(b, "begin")
    indent(b)
    write(b, "I.Kind := Kind;")
    # write(b, "case Kind is")
    # indent(b)
    # for sn in top_sorted_sn:
    #     schema = schemata[sn]
    #     assert schema["used"]
    #     write(b, "when %s =>" % schema["ada_name"])
    #     indent(b)
    #     write(b, "null;")
    #     outdent(b)
    # outdent(b)
    # write(b, "end case;")
    write(b, "Irep_Table.Append (I);")
    write(b, "return Irep_Table.Last;")
    outdent(b)
    write(b, "end New_Irep;")
    write(b, "")

    write(b, "-" * 70)
    write(b, "--  sub setters")
    write(b, "-" * 70)
    write(b, "")

    # Print all setters for 'subs'
    # sub ::= setter_name -> value|list         -> {schema: (op_id, type)}
    for setter_name in sorted(sub_setters):
        assert len(sub_setters[setter_name]) == 1
        assert ("value" in sub_setters[setter_name] or
                "list"  in sub_setters[setter_name])
        is_list = "list" in sub_setters[setter_name]
        data    = list(sub_setters[setter_name].itervalues())[0]

        all_the_same = len(set(x[1] for x in data.itervalues())) == 1

        name = ada_setter_name(setter_name, is_list)

        write(s, "procedure %s (I : Irep; Value : Irep)" % name)

        precon = []

        i_kinds = set()
        for kind in data:
            i_kinds |= all_used_subclasses(kind)
        precon += mk_precondition_in("I", i_kinds)

        if all_the_same:
            v_kinds = set()
            for op_id, kind in data.itervalues():
                v_kinds |= all_used_subclasses(kind)
            precon[-1] += " and then"
            precon += mk_precondition_in("Value", v_kinds)

        precon[-1] += ";"
        for l in mk_prefixed_lines("with Pre => ", precon):
            write(s, l)

        if not all_the_same:
            write(s, "--  TODO: precondition for Value")

        # layout ::= schema -> friendly_name -> (str|int|bool|sloc, index)
        write(b, "procedure %s (I : Irep; Value : Irep)" % name)
        write(b, "is")
        write(b, "begin")
        indent(b)
        write(b, "if I = Empty then")
        indent(b)
        write(b, "raise Program_Error;")
        outdent(b)
        write(b, "end if;")
        write(b, "")
        write(b, "case Irep_Table.Table (I).Kind is")
        indent(b)
        for sn in sorted(i_kinds):
            write(b, "when %s =>" % ada_casing(schemata[sn]["ada_name"]))
            indent(b)
            layout_kind, layout_index = layout[sn][setter_name]
            if is_list:
                write(b, "--  TODO: setting of list")
                write(b, "null;")
            else:
                write(b, "Irep_Table.Table (I).%s := Integer (Value);" %
                         ada_component_name(layout_kind, layout_index))
            outdent(b)
        write(b, "when others =>")
        indent(b)
        write(b, "raise Program_Error;")
        outdent(b)
        outdent(b)
        write(b, "end case;")
        outdent(b)
        write(b, "end %s;" % name)

        write(s, "")
        write(b, "")

    write(b, "-" * 70)
    write(b, "--  namedSub and comment setters")
    write(b, "-" * 70)
    write(b, "")

    # Print all setters for 'named' and 'comment'
    # nam ::= setter_name -> value|list|trivial -> {schema: (is_comment, type)}
    for setter_name in sorted(named_setters):
        assert len(named_setters[setter_name]) >= 1
        assert set(named_setters[setter_name]) <= set(["trivial",
                                                       "irep",
                                                       "list"])
        for kind in named_setters[setter_name]:
            data = named_setters[setter_name][kind]
            all_the_same = len(set(x[1] for x in data.itervalues())) == 1
            is_list = kind == "list"
            name = ada_setter_name(setter_name, is_list)
            assert all_the_same

            if kind == "trivial":
                tmp = list(x[1] for x in data.itervalues())[0]
                value_ada_typ = {"integer"   : "Integer",
                                 "bool"      : "Boolean",
                                 "string"    : "String",
                                 "gnat:sloc" : "Source_Ptr"}[tmp]
            else:
                value_ada_typ = "Irep"

            write(s, "procedure %s (I : Irep; Value : %s)" % (name,
                                                              value_ada_typ))

            precon = []

            i_kinds = set()
            for cls in data:
                i_kinds |= all_used_subclasses(cls)
            precon += mk_precondition_in("I", i_kinds)

            if all_the_same and value_ada_typ == "Irep":
                v_kinds = set()
                for is_constant, cls in data.itervalues():
                    v_kinds |= all_used_subclasses(cls)
                precon[-1] += " and then"
                precon += mk_precondition_in("Value", v_kinds)

            precon[-1] += ";"
            for l in mk_prefixed_lines("with Pre => ", precon):
                write(s, l)

            if not all_the_same:
                write(s, "--  TODO: precondition for Value")

            # layout ::= schema -> friendly_name -> (str|int|bool|sloc, index)
            write(b, "procedure %s (I : Irep; Value : %s)" % (name,
                                                              value_ada_typ))
            write(b, "is")
            write(b, "begin")
            indent(b)
            write(b, "if I = Empty then")
            indent(b)
            write(b, "raise Program_Error;")
            outdent(b)
            write(b, "end if;")
            write(b, "")
            if setter_name == "source_location":
                write(b, "Irep_Table.Table (I).Sloc := Value;")
            else:
                write(b, "case Irep_Table.Table (I).Kind is")
                indent(b)
                for sn in sorted(i_kinds):
                    write(b, "when %s =>" % ada_casing(schemata[sn]["ada_name"]))
                    indent(b)
                    layout_kind, layout_index = layout[sn][setter_name]
                    if is_list:
                        write(b, "--  TODO: setting of list")
                        write(b, "null;")
                    elif layout_kind == "str":
                        write(b, "Start_String;")
                        write(b, "Store_String_Chars (Value);")
                        write(b, "Irep_Table.Table (I).%s := End_String;" %
                              ada_component_name(layout_kind, layout_index))
                    elif layout_kind == "sloc":
                        write(b, "Irep_Table.Table (I).%s := Value;")
                    elif kind == "trivial":
                        write(b, "Irep_Table.Table (I).%s := Value;" %
                              ada_component_name(layout_kind, layout_index))
                    else:
                        write(b, "Irep_Table.Table (I).%s := Integer (Value);" %
                              ada_component_name(layout_kind, layout_index))
                    outdent(b)
                write(b, "when others =>")
                indent(b)
                write(b, "raise Program_Error;")
                outdent(b)
                outdent(b)
                write(b, "end case;")
            outdent(b)
            write(b, "end %s;" % name)


            write(s, "")
            write(b, "")

    ##########################################################################
    # Initialisation

    # write(s, "procedure Init;")
    # write(s, "--  Must be called before this package is used")
    # write(s, "")

    # write(b, "procedure Init is")
    # write(b, "begin")
    # indent(b)
    # outdent(b)
    # write(b, "end Init;")
    # write(b, "")

    outdent(s)
    write(s, "end Ireps;")
    write_file("ireps.ads", s)

    outdent(b)
    write(b, "end Ireps;")
    write_file("ireps.adb", b)

    #pprint(schemata)

if __name__ == "__main__":
    main()
