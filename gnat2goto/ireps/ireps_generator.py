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
        return "Append_%s" % ada_casing(name)
    else:
        return "Set_%s" % ada_casing(name)

def ada_component_name(layout_kind, layout_id=None):
    rv = "%s_%%u" % ({"str" : "String",
                      "int" : "Int",
                      "bool" : "Bool"}[layout_kind])
    if layout_id is not None:
        rv = rv % layout_id
    return rv

def write_file(f):
    indent = 0
    instructions = f["content"]
    with open(f["name"], "w") as fd:
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

def manual_indent(f):
    f["content"].append({"kind" : "indent"})
    f["indent"] += 1

def manual_outdent(f):
    f["content"].append({"kind" : "outdent"})
    f["indent"] -= 1

def write(f, txt):
    if len(txt) + 3*f["indent"] < 80:
        f["content"].append({"kind" : "text",
                             "text" : str(txt)})
    else:
        bs = []
        last_space = None
        for pos, c in enumerate(str(txt)):
            actual_pos = 3 * f["indent"] + pos
            if actual_pos < 80:
                if c == '(':
                    bs.append(pos)
                elif c == ')':
                    del bs[-1]
                elif c == ' ':
                    last_space = pos
            else:
                assert len(bs) > 0
                assert last_space is not None
                break
        a = txt[:last_space].rstrip()
        b = " " * bs[-1] + txt[last_space:].strip()
        assert len(a) + f["indent"] * 3 < 80
        assert len(b) + f["indent"] * 3 < 80
        f["content"].append({"kind" : "text",
                             "text" : a})
        f["content"].append({"kind" : "text",
                             "text" : b})

    # Finally, we update the bracketing
    for pos, c in enumerate(str(txt)):
        if c == '(':
            f["bracket_stack"].append(pos)
        elif c == ")":
            del f["bracket_stack"][-1]

def continuation(f):
    assert f["content"][-2]["kind"] == "text"
    assert f["content"][-1]["kind"] == "text"
    len_before = len(f["content"])
    # Merge the last two lines if it doesn't make them too long
    tmp = f["content"][-2]["text"].rstrip() + " " + f["content"][-1]["text"].strip()
    if 3 * f["indent"] + len(tmp) < 80:
        del f["content"][-1]
        f["content"][-1]["text"] = tmp
    assert len(f["content"]) in (len_before, len_before - 1)

def new_file(name):
    return {"name"          : name,
            "indent"        : 0,
            "bracket_stack" : [],
            "content"       : []}

class indent(object):
    def __init__(self, the_file):
        self.the_file = the_file

    def __exit__(self, type, value, traceback):
        manual_outdent(self.the_file)

    def __enter__(self):
        manual_indent(self.the_file)


def write_comment_block(f, txt):
    write(f, "-" * (len(txt) + 6))
    write(f, "-- " + txt + " --")
    write(f, "-" * (len(txt) + 6))
    write(f, "")

def mk_prefixed_lines(prefix, lines, join=""):
    # Prefix the first line with prefix, and everything else by a suitable
    # number of spaces.
    assert len(lines) > 0
    rv = [prefix + lines[0]]
    empty_prefix = " " * (len(prefix) - len(join)) + join
    for line in lines[1:]:
        rv.append(empty_prefix + line)
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
                          sn in ("struct_type",
                                 "pointer_type",
                                 "signedbv_type"))
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
    s = new_file("ireps.ads")
    b = new_file("ireps.adb")
    write(s, "with Types;         use Types;")  # Source_Ptr
    write(s, "")
    write(s, "with GNATCOLL.JSON; use GNATCOLL.JSON;")  # JSON
    write(s, "")

    write(b, "with Table;")
    write(b, "with Alloc;   use Alloc;") # for Nodes_Initial
    write(b, "with Namet;   use Namet;") # Name_Buffer
    write(b, "with Output;  use Output;") # for Debug IO
    write(b, "with Stringt; use Stringt;") # String_Id
    write(b, "")

    write(s, "package Ireps is")
    write(s, "")
    manual_indent(s)

    write(b, "package body Ireps is")
    write(b, "")
    manual_indent(b)

    ##########################################################################
    # Types and subtypes

    write(s, "type Irep is range 0 .. Integer'Last;")
    write(s, "Empty : constant Irep := 0;")
    write(s, "")

    write(s, "type Irep_List is private")
    write(s, "with Iterable => (First       => List_First,")
    write(s, "                  Next        => List_Next,")
    write(s, "                  Has_Element => List_Has_Element,")
    write(s, "                  Element     => List_Element);")
    write(s, "")

    write(s, "type List_Cursor is private;")
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
    s["content"][-1]["text"] = s["content"][-1]["text"].rstrip(",") + ");"
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
    # schema -> friendly_name -> (str|int|bool|sloc, index, irep|list|trivial)

    for sn in top_sorted_sn:
        op_counts[sn] = {"int"  : 0,
                         "str"  : 0,
                         "bool" : 0}
        layout[sn] = {}

        for setter_name, data in sub_setters.iteritems():
            assert len(data) == 1
            assert "value" in data or "list" in data
            typ = "list" if "list" in data else "irep"
            for kind, variants in data.iteritems():
                if sn in variants:
                    layout[sn][setter_name] = ("int", op_counts[sn]["int"], typ)
                    op_counts[sn]["int"] += 1

        for setter_name, setter_kinds in named_setters.iteritems():
            for kind in setter_kinds:
                if sn in setter_kinds[kind]:
                    is_comment, typ = setter_kinds[kind][sn]
                    if kind in ("irep", "list") or typ == "integer":
                        l_typ = "trivial" if typ == "integer" else kind
                        layout[sn][setter_name] = ("int",
                                                   op_counts[sn]["int"],
                                                   l_typ)
                        op_counts[sn]["int"] += 1
                    elif typ == "string":
                        layout[sn][setter_name] = ("str",
                                                   op_counts[sn]["str"],
                                                   "trivial")
                        op_counts[sn]["str"] += 1
                    elif typ == "bool":
                        layout[sn][setter_name] = ("bool",
                                                   op_counts[sn]["bool"],
                                                   "trivial")
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

    ##########################################################################
    # Datastructure

    write(b, "function To_String (S : String_Id) return String;")
    write(b, "")

    write(b, "function Is_Irep (I : Integer) return Boolean")
    write(b, "is (I >= Integer (Irep'First));")
    continuation(b)
    write(b, "")

    write(b, "function Is_List (I : Integer) return Boolean")
    write(b, "is (I <= Integer (Irep_List'Last));")
    continuation(b)
    write(b, "")

    write(b, "function To_Internal_List (L : Irep_List) return Internal_Irep_List")
    write(b, "is (Internal_Irep_List (-L));")
    continuation(b)
    write(b, "")

    write(b, "function To_List (L : Internal_Irep_List) return Irep_List")
    write(b, "is (Irep_List (-L));")
    continuation(b)
    write(b, "")

    write(b, "type Irep_Node is record")
    components = [("Kind", "Valid_Irep_Kind", None),
                  ("Sloc", "Source_Ptr", "No_Location")]
    for i in xrange(max(x["int"] for x in op_counts.itervalues())):
        components.append(("Int_%u" % i, "Integer", "0"))
    for i in xrange(max(x["str"] for x in op_counts.itervalues())):
        components.append(("String_%u" % i, "String_Id", "Null_String_Id"))
    for i in xrange(max(x["bool"] for x in op_counts.itervalues())):
        components.append(("Bool_%u" % i, "Boolean", "False"))
    with indent(b):
        max_len = max(map(len, (x[0] for x in components)))
        for cname, ctyp, default in components:
            if default is None:
                write(b, "%-*s : %-15s;" % (max_len, cname, ctyp))
            else:
                write(b, "%-*s : %-15s := %s;" % (max_len,
                                                  cname,
                                                  ctyp,
                                                  default))
    write(b, "end record;")
    write(b, "pragma Pack (Irep_Node);")
    write(b, "")

    write(b, "type Irep_List_Node is record")
    with indent(b):
        write(b, "A       : Integer;            "
              "--  Element [or pointer to first list link]")
        write(b, "B       : Internal_Irep_List; "
              "--  Next [or pointer to last list link]")
        write(b, "Is_Node : Boolean;")
    write(b, "end record with Dynamic_Predicate =>")
    with indent(b):
        write(b, "(if Is_Node")
        write(b, " then Is_Irep (A)")
        continuation(b)
        write(b, " else Is_List (A));")
        continuation(b)
    write(b, "pragma Pack (Irep_List_Node);")
    write(b, "")

    write(b, "package Irep_Table is new Table.Table")
    write(b, "  (Table_Component_Type => Irep_Node,")
    write(b, "   Table_Index_Type     => Irep,")
    write(b, "   Table_Low_Bound      => 1,")
    write(b, "   Table_Initial        => Nodes_Initial, --  seems like a good guess")
    write(b, "   Table_Increment      => Nodes_Increment,")
    write(b, "   Table_Name           => \"Irep_Table\");")
    write(b, "")

    write(b, "package Irep_List_Table is new Table.Table")
    write(b, "  (Table_Component_Type => Irep_List_Node,")
    write(b, "   Table_Index_Type     => Internal_Irep_List,")
    write(b, "   Table_Low_Bound      => 1,")
    write(b, "   Table_Initial        => Elists_Initial, --  seems like a good guess")
    write(b, "   Table_Increment      => Elists_Increment,")
    write(b, "   Table_Name           => \"Irep_List_Table\");")
    write(b, "")

    ##########################################################################
    # List API

    write(b, "function New_List return Irep_List;")
    write(b, "")

    write(b, "procedure Append (L : Irep_List; I : Irep)")
    write(b, "with Pre => L /= 0;")
    write(b, "")

    write_comment_block(b, "New_List")
    write(b, "function New_List return Irep_List")
    write(b, "is")
    continuation(b)
    with indent(b):
        write(b, "N : constant Irep_List_Node := (Is_Node => False,")
        write(b, "                                A       => 0,")
        write(b, "                                B       => 0);")
    write(b, "begin")
    with indent(b):
        write(b, "Irep_List_Table.Append (N);")
        write(b, "return To_List (Irep_List_Table.Last);")
    write(b, "end New_List;")
    write(b, "")

    write_comment_block(b, "Append")
    write(b, "procedure Append (L : Irep_List; I : Irep)")
    write(b, "is separate;")
    continuation(b)
    write(b, "")

    ##########################################################################
    # API

    write(s, "function Kind (I : Irep) return Irep_Kind;")
    write(s, "")

    write_comment_block(b, "Kind")
    write(b, "function Kind (I : Irep) return Irep_Kind")
    write(b, "is")
    continuation(b)
    write(b, "begin")
    with indent(b):
        write(b, "if I = Empty then")
        with indent(b):
            write(b, "return I_Empty;")
        write(b, "else")
        with indent(b):
            write(b, "return Irep_Table.Table (I).Kind;")
        write(b, "end if;")
    write(b, "end Kind;")
    write(b, "")

    write(s, "function Id (I : Irep) return String;")
    write(s, "")

    write_comment_block(b, "Id")
    write(b, "function Id (I : Irep) return String")
    write(b, "is")
    continuation(b)
    write(b, "begin")
    with indent(b):
        write(b, "if I not in 1 .. Irep_Table.Last then")
        with indent(b):
            write(b, 'return "";')
        write(b, "end if;")
        write(b, "")
        write(b, "case Irep_Table.Table (I).Kind is")
        with indent(b):
            for sn in top_sorted_sn:
                if sn in const and "id" in const[sn]:
                    write(b, "when %s =>" % schemata[sn]["ada_name"])
                    write(b, '   return "%s";' % const[sn]["id"]["id"])
                    continuation(b)
            write(b, 'when others => return "";')
        write(b, "end case;")
    write(b, "end Id;")
    write(b, "")


    write(s, "function New_Irep (Kind : Valid_Irep_Kind) return Irep;")
    write(s, "")

    write_comment_block(b, "New_Irep")
    write(b, "function New_Irep (Kind : Valid_Irep_Kind) return Irep")
    write(b, "is")
    continuation(b)
    with indent(b):
        write(b, "I : Irep_Node;")
    write(b, "begin")
    with indent(b):
        write(b, "I.Kind := Kind;")
        write(b, "Irep_Table.Append (I);")
        write(b, "return Irep_Table.Last;")
    write(b, "end New_Irep;")
    write(b, "")

    def emit_getter(fn_name,
                    fn_kind,    # irep|trivial|list
                    value_type, # irep|bool|integer|string|gnat:sloc
                    inputs):    # map sn -> sn|None (if value_type != irep)
        assert fn_kind in ("irep", "trivial", "list")
        assert fn_kind != "irep" or value_type == "irep"
        assert fn_kind != "trivial" or value_type in ("bool",
                                                          "integer",
                                                          "string",
                                                          "gnat:sloc")
        assert fn_kind != "list" or value_type == "irep"
        is_list = fn_kind == "list"
        name = "Get_" + ada_casing(fn_name)
        all_the_same = len(set(x for x in inputs.itervalues())) == 1

        if is_list:
            ada_value_type = "Irep_List"
        else:
            ada_value_type = {"irep"      : "Irep",
                              "bool"      : "Boolean",
                              "integer"   : "Integer",
                              "string"    : "String",
                              "gnat:sloc" : "Source_Ptr"}[value_type]

        precon = []
        i_kinds = set()
        for kind in inputs:
            i_kinds |= all_used_subclasses(kind)
        precon += mk_precondition_in("I", i_kinds)
        precon[-1] += ";"

        write(s, "function %s (I : Irep) return %s " % (name,
                                                        ada_value_type))
        for l in mk_prefixed_lines("with Pre => ", precon):
            write(s, l)
        write(s, "")

        kind_slot_map = {}
        if fn_name == "source_location":
            field = "Irep_Table.Table (I).Sloc"
        else:
            for sn in sorted(i_kinds):
                layout_kind, layout_index, layout_typ = layout[sn][fn_name]
                if layout_index not in kind_slot_map:
                    kind_slot_map[layout_index] = []
                kind_slot_map[layout_index].append(sn)
            field = "Irep_Table.Table (I)." + ada_component_name(layout_kind)

        write_comment_block(b, name)
        write(b, "function %s (I : Irep) return %s" % (name,
                                                       ada_value_type))
        write(b, "is")
        continuation(b)
        write(b, "begin")
        manual_indent(b)

        write(b, "if I = Empty then")
        with indent(b):
            write(b, "raise Program_Error;")
        write(b, "end if;")
        write(b, "")

        if fn_kind == "irep":
            get_conversion = "Irep (%s)"
        elif fn_kind == "list":
            get_conversion = "Irep_List (%s)"
        elif value_type in ("bool", "integer", "gnat:sloc"):
            get_conversion = "%s"
        elif value_type == "string":
            get_conversion = "To_String (%s)"
        else:
            assert False

        retval = get_conversion % field

        if len(kind_slot_map) == 0:
            assert fn_name == "source_location"
            write(b, "return %s;" % retval)
        elif len(kind_slot_map) == 1:
            the_slot = list(kind_slot_map)[0]
            write(b, "return %s;" % (retval % the_slot))
        else:
            write(b, "case Irep_Table.Table (I).Kind is")
            manual_indent(b)
            for layout_index, i_kinds in kind_slot_map.iteritems():
                if len(i_kinds) == 1:
                    write(b, "when %s =>" %
                          ada_casing(schemata[i_kinds[0]]["ada_name"]))
                else:
                    for l in mk_prefixed_lines("when ",
                                               [schemata[x]["ada_name"]
                                                for x in i_kinds],
                                               "| "):
                        write(b, l)
                    write(b, "=>")
                with indent(b):
                    write(b, "return %s;" % (retval % layout_index))
                write(b, "")
            write(b, "when others =>")
            with indent(b):
                write(b, "raise Program_Error;")
            manual_outdent(b)
            write(b, "end case;")

        manual_outdent(b)
        write(b, "end %s;" % name)
        write(b, "")

    def emit_setter(fn_name,
                    fn_kind,    # irep|trivial|list
                    value_type, # irep|bool|integer|string|gnat:sloc
                    inputs):    # map sn -> sn|None (if value_type != irep)
        assert fn_kind in ("irep", "trivial", "list")
        assert fn_kind != "irep" or value_type == "irep"
        assert fn_kind != "trivial" or value_type in ("bool",
                                                          "integer",
                                                          "string",
                                                          "gnat:sloc")
        assert fn_kind != "list" or value_type == "irep"
        is_list = fn_kind == "list"
        name = ada_setter_name(fn_name, is_list)
        all_the_same = len(set(x for x in inputs.itervalues())) == 1

        ada_value_type = {"irep"      : "Irep",
                          "bool"      : "Boolean",
                          "integer"   : "Integer",
                          "string"    : "String",
                          "gnat:sloc" : "Source_Ptr"}[value_type]

        precon = []
        i_kinds = set()
        for kind in inputs:
            i_kinds |= all_used_subclasses(kind)
        precon += mk_precondition_in("I", i_kinds)
        if all_the_same:
            v_kinds = set()
            for kind in inputs.itervalues():
                if kind is not None:
                    v_kinds |= all_used_subclasses(kind)
            if len(v_kinds) > 0:
                precon[-1] += " and then"
                precon += mk_precondition_in("Value", v_kinds)
        precon[-1] += ";"

        write(s, "procedure %s (I : Irep; Value : %s)" % (name,
                                                          ada_value_type))
        for l in mk_prefixed_lines("with Pre => ", precon):
            write(s, l)
        if not all_the_same:
            write(s, "--  TODO: precondition for Value")
        write(s, "")

        kind_slot_map = {}
        if fn_name == "source_location":
            asn_lhs = "Irep_Table.Table (I).Sloc"
        else:
            for sn in sorted(i_kinds):
                layout_kind, layout_index, layout_typ = layout[sn][fn_name]
                if layout_index not in kind_slot_map:
                    kind_slot_map[layout_index] = []
                kind_slot_map[layout_index].append(sn)
            asn_lhs = "Irep_Table.Table (I)." + ada_component_name(layout_kind)

        write_comment_block(b, name)
        write(b, "procedure %s (I : Irep; Value : %s)" % (name,
                                                          ada_value_type))
        write(b, "is")
        continuation(b)
        write(b, "begin")
        manual_indent(b)
        write(b, "if I = Empty then")
        with indent(b):
            write(b, "raise Program_Error;")
        write(b, "end if;")
        write(b, "")

        if fn_kind in ("irep", "list"):
            assert value_type == "irep"
            asn_rhs = "Integer (Value)"
        elif value_type in ("bool", "integer", "gnat:sloc"):
            asn_rhs = "Value"
        elif value_type == "string":
            write(b, "Start_String;")
            write(b, "Store_String_Chars (Value);")
            asn_rhs = "End_String"
        else:
            assert False

        if len(kind_slot_map) == 0:
            assert fn_name == "source_location"
            write(b, asn_lhs + " := " + asn_rhs + ";")
        elif len(kind_slot_map) == 1:
            the_slot = list(kind_slot_map)[0]
            if is_list:
                write(b, "if %s = 0 then" % (asn_lhs % the_slot))
                with indent(b):
                    write(b, "%s := Integer (New_List);" % (asn_lhs % the_slot))
                write(b, "end if;")
                write(b, "Append (Irep_List (%s), Value);" % (asn_lhs %
                                                              the_slot))
            else:
                write(b,
                      asn_lhs % the_slot + " := " + asn_rhs + ";")
        else:
            write(b, "case Irep_Table.Table (I).Kind is")
            manual_indent(b)
            for layout_index, i_kinds in kind_slot_map.iteritems():
                if len(i_kinds) == 1:
                    write(b, "when %s =>" %
                          ada_casing(schemata[i_kinds[0]]["ada_name"]))
                else:
                    for l in mk_prefixed_lines("when ",
                                               [schemata[x]["ada_name"]
                                                for x in i_kinds],
                                               "| "):
                        write(b, l)
                    write(b, "=>")

                with indent(b):
                    if is_list:
                        write(b, "if %s = 0 then" % (asn_lhs % layout_index))
                        with indent(b):
                            write(b, "%s := Integer (New_List);" %
                                  (asn_lhs % layout_index))
                        write(b, "end if;")
                        write(b, "Append (Irep_List (%s), Value);" %
                              (asn_lhs % layout_index))
                    else:
                        write(b,
                              asn_lhs % layout_index + " := " + asn_rhs + ";")

                write(b, "")
            write(b, "when others =>")
            with indent(b):
                write(b, "raise Program_Error;")
            manual_outdent(b)
            write(b, "end case;")

        manual_outdent(b)
        write(b, "end %s;" % name)
        write(b, "")

    for fn, category in [(emit_getter, "getters"),
                         (emit_setter, "setters")]:
        write(b, "-" * 70)
        write(b, "--  sub %s" % category)
        write(b, "-" * 70)
        write(b, "")

        # Print all setters for 'subs'
        # sub ::= setter_name -> value|list         -> {schema: (op_id, type)}
        for fn_name in sorted(sub_setters):
            assert len(sub_setters[fn_name]) == 1
            assert ("value" in sub_setters[fn_name] or
                    "list"  in sub_setters[fn_name])
            is_list = "list" in sub_setters[fn_name]
            data    = list(sub_setters[fn_name].itervalues())[0]

            fn(fn_name    = fn_name,
               fn_kind    = ("irep"
                             if "value" in sub_setters[fn_name]
                             else "list"),
               value_type = "irep",
               inputs     = dict((sn, data[sn][1]) for sn in data))

        write(b, "-" * 70)
        write(b, "--  namedSub and comment %s" % category)
        write(b, "-" * 70)
        write(b, "")

        # Print all setters for 'named' and 'comment'
        # nam ::= setter_name -> value|list|trivial ->
        #           {schema: (is_comment, type)}
        for fn_name in sorted(named_setters):
            assert len(named_setters[fn_name]) >= 1
            assert set(named_setters[fn_name]) <= set(["trivial",
                                                           "irep",
                                                           "list"])
            for kind in named_setters[fn_name]:
                data = named_setters[fn_name][kind]

                if kind == "trivial":
                    vt = list(x[1] for x in data.itervalues())[0]
                    inputs = dict((cls, None) for cls in data)
                else:
                    vt = "irep"
                    inputs = dict((cls, data[cls][1]) for cls in data)
                fn(fn_name    = fn_name,
                   fn_kind    = kind,
                   value_type = vt,
                   inputs     = inputs)

    ##########################################################################
    # Traversal

    write(b, "-" * 70)
    write(b, "--  Traversal")
    write(b, "-" * 70)
    write(b, "")

    write(s, "type Irep_Traversal is (T_Continue, T_Skip, T_Abort);")
    write(s, "--  T_Continue : continue")
    write(s, "--  T_Skip     : skip all children of this node")
    write(s, "--  T_Abort    : abort traversal")
    write(s, "")

    write(s, "procedure Walk_Irep_Tree")
    write(s, "  (Root    : Irep;")
    write(s, "   Visitor : not null access")
    write(s, "     function (I : Irep) return Irep_Traversal);")
    write(s, "")

    write(s, "procedure Walk_Irep_Tree")
    write(s, "  (Root    : Irep;")
    write(s, "   Visitor : not null access")
    write(s, "     procedure (I : Irep));")
    write(s, "")

    write_comment_block(b, "Walk_Irep_Tree")

    write(b, "procedure Walk_Irep_Tree")
    write(b, "  (Root    : Irep;")
    write(b, "   Visitor : not null access")
    write(b, "     function (I : Irep) return Irep_Traversal)")
    write(b, "is")
    manual_indent(b)
    write(b, "Abort_Issued : Boolean := False;")
    write(b, "--  Set to True if we are instructed to abort.")
    write(b, "")

    write(b, "procedure Recurse (L : Irep_List);")
    write(b, "")

    write(b, "procedure Recurse (I : Irep);")
    write(b, "")

    write_comment_block(b, "Recurse")

    write(b, "procedure Recurse (L : Irep_List)")
    write(b, "is")
    with indent(b):
        write(b, "Cursor : List_Cursor;")
    write(b, "begin")
    with indent(b):
        write(b, "--  Check if we should do nothing.")
        write(b, "if Abort_Issued or else L = 0 then")
        with indent(b):
            write(b, "return;")
        write(b, "end if;")
        write(b, "")

        write(b, "Cursor := List_First (L);")
        write(b, "while not Abort_Issued and then List_Has_Element (L, Cursor) loop")
        with indent(b):
            write(b, "Recurse (List_Element (L, Cursor));")
            write(b, "Cursor := List_Next (L, Cursor);")
        write(b, "end loop;")
    write(b, "end Recurse;")
    write(b, "")

    write(b, "procedure Recurse (I : Irep)")
    write(b, "is")
    write(b, "begin")
    with indent(b):
        write(b, "--  Check if we should do nothing.")
        write(b, "if Abort_Issued or else I = 0 then")
        with indent(b):
            write(b, "return;")
        write(b, "end if;")
        write(b, "")

        write(b, "--  Visit this node to see if we should continue")
        write(b, "case Visitor (I) is")
        with indent(b):
            write(b, "when T_Continue =>")
            with indent(b):
                write(b, "null;")
            write(b, "when T_Skip =>")
            with indent(b):
                write(b, "return;")
            write(b, "when T_Abort =>")
            with indent(b):
                write(b, "Abort_Issued := True;")
                write(b, "return;")
        write(b, "end case;")
        write(b, "")

        # lo ::= schema -> friendly_name -> (str|int|bool|sloc,
        #                                    index,
        #                                    irep|list|trivial)

        write(b, "--  Visit children")
        write(b, "case Irep_Table.Table (I).Kind is")
        manual_indent(b)
        for sn in top_sorted_sn:
            write(b, "when %s =>" % schemata[sn]["ada_name"])
            with indent(b):
                null_required = True
                for friendly_name in layout[sn]:
                    lo_kind, lo_idx, lo_typ = layout[sn][friendly_name]
                    fld = "Irep_Table.Table (I).%s" %\
                          ada_component_name(lo_kind, lo_idx)
                    if lo_typ == "irep":
                        null_required = False
                        write(b, "Recurse (Irep (%s));" % fld)
                    elif lo_typ == "list":
                        null_required = False
                        write(b, "Recurse (Irep_List (%s));" % fld)
                if null_required:
                    write(b, "null;")
        manual_outdent(b)
        write(b, "end case;")
    write(b, "end Recurse;")
    manual_outdent(b)
    write(b, "begin")
    with indent(b):
        write(b, "Recurse (Root);")
    write(b, "end Walk_Irep_Tree;")
    write(b, "")

    write(b, "procedure Walk_Irep_Tree")
    write(b, "  (Root    : Irep;")
    write(b, "   Visitor : not null access")
    write(b, "     procedure (I : Irep))")
    write(b, "is")
    with indent(b):
        write(b, "function F (I : Irep) return Irep_Traversal;")
        write(b, "")
        write_comment_block(b, "Visitor")
        write(b, "function F (I : Irep) return Irep_Traversal")
        write(b, "is")
        continuation(b)
        write(b, "begin")
        with indent(b):
            write(b, "Visitor (I);")
            write(b, "return T_Continue;")
        write(b, "end F;")
        write(b, "")
    write(b, "begin")
    with indent(b):
        write(b, "Walk_Irep_Tree (Root, F'Access);")
    write(b, "end Walk_Irep_Tree;")
    write(b, "")

    ##########################################################################
    # Serialisation to JSON

    write(b, "-" * 70)
    write(b, "--  Serialisation to JSON")
    write(b, "-" * 70)
    write(b, "")

    write(s, "function To_JSON (I : Irep) return JSON_Value;")
    write(s, "--  Serialise to JSON")
    write(s, "")

    write(b, "function To_JSON (L : Irep_List) return JSON_Array;")
    write(b, "--  Serialise list to JSON")
    write(b, "")

    write(b, "function Trivial_Boolean (B : Boolean) return JSON_Value;")
    write(b, "--  Create a trivial irep with id = B")
    write(b, "")

    write(b, "function Trivial_Integer (I : Integer) return JSON_Value;")
    write(b, "--  Create a trivial irep with id = I (base 10)")
    write(b, "")

    write(b, "function Trivial_String (S : String) return JSON_Value;")
    write(b, "--  Create a trivial irep with id = S")
    write(b, "")

    write(b, "function Trivial_String (S : String_Id) return JSON_Value;")
    write(b, "--  Create a trivial irep with id = S")
    write(b, "")

    write(b, "function Trivial_Sloc (S : Source_Ptr) return JSON_Value;")
    write(b, "--  Create a source_location from S")
    write(b, "")

    write(b, "function Trivial_List (L : Irep_List; Name : String)")
    write(b, "                      return JSON_Value;")
    continuation(b)
    write(b, "--  Create naked irep with id = Name and subs containing L")
    write(b, "")

    # lo ::= schema -> friendly_name -> (str|int|bool|sloc,
    #                                    index,
    #                                    irep|list|trivial)
    # subs ::= setter_name -> value|list -> {schema: (op_id, type)}
    # nams ::= setter_name -> value|list|trivial -> {schema: (is_comment, type)}
    # cnst ::= schema -> id|namedSub|comment -> {name: value}

    write(b, "function To_JSON (L : Irep_List) return JSON_Array")
    write(b, "is separate;")
    continuation(b)
    write(b, "")

    write_comment_block(b, "Trivial_Boolean")
    write(b, "function Trivial_Boolean (B : Boolean) return JSON_Value")
    write(b, "is")
    continuation(b)
    with indent(b):
        write(b, "As_String : constant array (Boolean) of String (1 .. 1) :=")
        write(b, '  (False => "0", True => "1");')
    write(b, "begin")
    with indent(b):
        write(b, "return V : constant JSON_Value := Create_Object do")
        with indent(b):
            write(b, 'V.Set_Field ("id",       As_String (B));')
            write(b, 'V.Set_Field ("sub",      Empty_Array);')
            write(b, 'V.Set_Field ("namedSub", Create_Object);')
            write(b, 'V.Set_Field ("comment",  Create_Object);')
        write(b, "end return;")
    write(b, "end Trivial_Boolean;")
    write(b, "")

    write_comment_block(b, "Trivial_Integer")
    write(b, "function Trivial_Integer (I : Integer) return JSON_Value")
    write(b, "is")
    continuation(b)
    with indent(b):
        write(b, "S : constant String := Integer'Image (I);")
    write(b, "begin")
    with indent(b):
        write(b, "return V : constant JSON_Value := Create_Object do")
        with indent(b):
            write(b, "if I >= 0 then")
            with indent(b):
                write(b, 'V.Set_Field ("id", S (2 .. S\'Last));')
            write(b, "else")
            with indent(b):
                write(b, 'V.Set_Field ("id", S);')
            write(b, "end if;")
            write(b, 'V.Set_Field ("sub",      Empty_Array);')
            write(b, 'V.Set_Field ("namedSub", Create_Object);')
            write(b, 'V.Set_Field ("comment",  Create_Object);')
        write(b, "end return;")
    write(b, "end Trivial_Integer;")
    write(b, "")

    write_comment_block(b, "Trivial_String")
    write(b, "function Trivial_String (S : String_Id) return JSON_Value")
    write(b, "is")
    continuation(b)
    write(b, "begin")
    with indent(b):
        write(b, "String_To_Name_Buffer (S);")
        write(b, "return V : constant JSON_Value := Create_Object do")
        with indent(b):
            write(b, 'V.Set_Field ("id",       Name_Buffer (1 .. Name_Len));')
            write(b, 'V.Set_Field ("sub",      Empty_Array);')
            write(b, 'V.Set_Field ("namedSub", Create_Object);')
            write(b, 'V.Set_Field ("comment",  Create_Object);')
        write(b, "end return;")
    write(b, "end Trivial_String;")
    write(b, "")

    write(b, "function Trivial_String (S : String) return JSON_Value")
    write(b, "is")
    continuation(b)
    write(b, "begin")
    with indent(b):
        write(b, "return V : constant JSON_Value := Create_Object do")
        with indent(b):
            write(b, 'V.Set_Field ("id",       S);')
            write(b, 'V.Set_Field ("sub",      Empty_Array);')
            write(b, 'V.Set_Field ("namedSub", Create_Object);')
            write(b, 'V.Set_Field ("comment",  Create_Object);')
        write(b, "end return;")
    write(b, "end Trivial_String;")
    write(b, "")

    write_comment_block(b, "Trivial_Sloc")
    write(b, "function Trivial_Sloc (S : Source_Ptr) return JSON_Value")
    write(b, "is separate;")
    continuation(b)
    write(b, "")

    write_comment_block(b, "Trivial_List")
    write(b, "function Trivial_List (L : Irep_List; Name : String)")
    write(b, "                      return JSON_Value")
    continuation(b)
    write(b, "is")
    write(b, "begin")
    with indent(b):
        write(b, "return Naked_Irep : constant JSON_Value := Create_Object do")
        with indent(b):
            write(b, 'Naked_Irep.Set_Field ("id",       Name);')
            write(b, 'Naked_Irep.Set_Field ("sub",      To_JSON (L));')
            write(b, 'Naked_Irep.Set_Field ("namedSub", Create_Object);')
            write(b, 'Naked_Irep.Set_Field ("comment",  Create_Object);')
        write(b, "end return;")
    write(b, "end Trivial_List;")
    write(b, "")

    write_comment_block(b, "To_JSON")
    write(b, "function To_JSON (I : Irep) return JSON_Value")
    write(b, "is")
    continuation(b)
    write(b, "begin")
    manual_indent(b)
    write(b, "if I = 0 then")
    with indent(b):
        write(b, "return JSON_Null;")
    write(b, "end if;")
    write(b, "")
    write(b, "declare")
    with indent(b):
        write(b, "N : Irep_Node renames Irep_Table.Table (I);")
        write(b, "")
        write(b, "Sub       :          JSON_Array := Empty_Array;")
        write(b, "Named_Sub : constant JSON_Value := Create_Object;")
        write(b, "Comment   : constant JSON_Value := Create_Object;")
    write(b, "begin")
    manual_indent(b)
    write(b, "return V : constant JSON_Value := Create_Object do")
    manual_indent(b)
    write(b, 'V.Set_Field ("id", Id (I));')
    write(b, "case N.Kind is")
    for sn in top_sorted_sn:
        schema = schemata[sn]
        with indent(b):
            write(b, "when %s =>" % schema["ada_name"])
            with indent(b):
                needs_null = True
                needs_sloc = False

                # Set all subs
                subs = {}
                for setter_name in sub_setters:
                    for kind in sub_setters[setter_name]:
                        assert kind in ("value", "list")
                        if sn in sub_setters[setter_name][kind]:
                            op_id = sub_setters[setter_name][kind][sn][0]
                            subs[op_id] = (setter_name, kind == "list")
                for i in xrange(len(subs)):
                    needs_null = False
                    setter_name, is_list = subs[i]
                    layout_kind, layout_index, layout_typ =\
                      layout[sn][setter_name]
                    tbl_field = "N." + ada_component_name(layout_kind,
                                                          layout_index)
                    if is_list:
                        write(b, "Append (Sub, Trivial_List (Irep_List (%s)," % tbl_field)
                        write(b, "                           \"%s\"));" % setter_name)
                        continuation(b)
                    else:
                        write(b, "Append (Sub, To_JSON (Irep (%s)));" %
                              tbl_field)

                # Set all namedSub and comments
                for setter_name in named_setters:
                    for kind in named_setters[setter_name]:
                        assert kind in ("irep", "list", "trivial")
                        if sn in named_setters[setter_name][kind]:
                            needs_null = False
                            if setter_name == "source_location":
                                needs_sloc = True
                                continue
                            is_comment, value_type =\
                              named_setters[setter_name][kind][sn]
                            layout_kind, layout_index, layout_typ =\
                              layout[sn][setter_name]
                            tbl_field = "N." + ada_component_name(layout_kind,
                                                                  layout_index)

                            obj = "Comment" if is_comment else "Named_Sub"
                            if kind == "irep":
                                val = "To_JSON (Irep (%s))" % tbl_field
                            elif kind == "list":
                                val = "Trivial_List (Irep_List (%s), \"%s\")" \
                                      % (tbl_field, setter_name)
                            elif layout_kind == "str":
                                val = "Trivial_String (%s)" % tbl_field
                            elif layout_kind == "int":
                                val = "Trivial_Integer (%s)" % tbl_field
                            elif layout_kind == "bool":
                                val = "Trivial_Boolean (%s)" % tbl_field
                            else:
                                assert False

                            tmp = "%s.Set_Field (" % obj
                            write(b, tmp + '"' + setter_name + '",')
                            write(b, " " * len(tmp) + val + ");")
                            continuation(b)


                # Set all constants
                for kind, data in const.get(sn, {}).iteritems():
                    if kind == "id":
                        continue
                    elif kind == "namedSub":
                        obj = "Named_Sub"
                    elif kind == "comment":
                        obj = "Comment"
                    else:
                        print sn, kind, const[sn]
                        assert False
                    for const_name, const_value in data.iteritems():
                        needs_null = False
                        tmp = "%s.Set_Field (" % obj
                        write(b, tmp + '"%s"' % const_name + ",")
                        write(b, " " * len(tmp) + 'Trivial_String ("%s"));'
                              % const_value)
                        continuation(b)

                # Set SLOC
                if needs_sloc:
                    write(b, 'Comment.Set_Field ("source_location",')
                    write(b, '                   Trivial_Sloc (N.Sloc));')
                    continuation(b)

                if needs_null:
                    write(b, "null;")
                write(b, "")
    write(b, "end case;")
    write(b, "")
    write(b, 'V.Set_Field ("sub",      Sub);')
    write(b, 'V.Set_Field ("namedSub", Named_Sub);')
    write(b, 'V.Set_Field ("comment",  Comment);')
    manual_outdent(b)
    write(b, "end return;")
    manual_outdent(b)
    write(b, "end;")
    manual_outdent(b)
    write(b, "end To_JSON;")
    write(b, "")


    ##########################################################################
    # Debug output

    write(b, "-" * 70)
    write(b, "--  Debug")
    write(b, "-" * 70)
    write(b, "")

    write(s, "procedure Print_Irep (I : Irep);")
    write(s, "--  Debug procedure to print the given Irep to standard output")
    write(s, "")

    write(b, "function To_String (K : Irep_Kind) return String;")
    write(b, "")
    write(b, "procedure PI_Irep (I : Irep);")
    write(b, "--  Print one-line irep description:")
    write(b, "--    e.g. I_Member_Expr (Irep=2) (Id=member)")
    write(b, "--    e.g. <Empty>")
    write(b, "")
    write(b, "procedure PI_List (L : Irep_List; Name : String);")
    write(b, "--  Print one-line list description using Name")
    write(b, "--    e.g. List #argument (Irep_List=-1)")
    write(b, "--    e.g. <Empty_List> (Irep_List=0)")
    write(b, "")
    write(b, "procedure PI_String (S : String_Id);")
    write(b, "--  Print one-line string description")
    write(b, "--    e.g. \"wibble\" (String_Id=400000001)")
    write(b, "")
    write(b, "procedure PI_Bool (B : Boolean);")
    write(b, "--  Print one-line boolean description")
    write(b, "--    e.g. True")
    write(b, "")
    write(b, "procedure PS_List (L : Irep_List; Name : String);")
    write(b, "--  Print short summary of list")
    write(b, "")

    write_comment_block(b, "To_String")
    write(b, "function To_String (K : Irep_Kind) return String")
    write(b, "is")
    continuation(b)
    write(b, "begin")
    with indent(b):
        write(b, "case K is")
        with indent(b):
            write(b, "when I_Empty => return \"I_Empty\";")
            for sn in top_sorted_sn:
                write(b, "when %s =>" % schemata[sn]["ada_name"])
                write(b, "   return \"%s\";" % schemata[sn]["ada_name"])
                continuation(b)
        write(b, "end case;")
    write(b, "end To_String;")
    write(b, "")

    write(b, "function To_String (S : String_Id) return String")
    write(b, "is")
    continuation(b)
    write(b, "begin")
    with indent(b):
        write(b, "String_To_Name_Buffer (S);")
        write(b, "return Name_Buffer (1 .. Name_Len);")
    write(b, "end To_String;")
    write(b, "")

    write_comment_block(b, "PI_Irep")
    write(b, "procedure PI_Irep (I : Irep)")
    write(b, "is")
    continuation(b)
    with indent(b):
        write(b, "Iid : constant String := Id (I);")
    write(b, "begin")
    with indent(b):
        write(b, "if I = Empty then")
        with indent(b):
            write(b, 'Write_Str ("<Empty>");')
        write(b, "elsif I > Irep_Table.Last then")
        with indent(b):
            write(b, 'Write_Str ("<Invalid>");')
        write(b, "else")
        with indent(b):
            write(b, 'Write_Str (To_String (Irep_Table.Table (I).Kind) '
                  '& " (Irep=");')
            write(b, "Write_Int (Int (I));")
            write(b, "Write_Char (')');")
            write(b, "if Iid'Length > 0 then")
            with indent(b):
                write(b, 'Write_Str (" (Id=" & Iid & ")");')
            write(b, "end if;")
        write(b, "end if;")
        write(b, "Write_Eol;")
    write(b, "end PI_Irep;")
    write(b, "")


    write_comment_block(b, "PI_List")
    write(b, "procedure PI_List (L : Irep_List; Name : String)")
    write(b, "is")
    continuation(b)
    write(b, "begin")
    with indent(b):
        write(b, "if L = 0 then")
        with indent(b):
            write(b, 'Write_Str ("<Empty_List>");')
        write(b, "else")
        with indent(b):
            write(b, 'Write_Str ("List #" & Name);')
        write(b, "end if;")
        write(b, 'Write_Str (" (Irep_List=");')
        write(b, 'Write_Int (Int (L));')
        write(b, "Write_Char (')');")
        write(b, "Write_Eol;")
    write(b, "end PI_List;")
    write(b, "")


    write_comment_block(b, "PI_String")
    write(b, "procedure PI_String (S : String_Id)")
    write(b, "is")
    continuation(b)
    write(b, "begin")
    with indent(b):
        write(b, "String_To_Name_Buffer (S);")
        write(b, "Write_Char ('\"');")
        write(b, "Write_Str (Name_Buffer (1 .. Name_Len));")
        write(b, "Write_Char ('\"');")
        write(b, 'Write_Str (" (String_Id=");')
        write(b, "Write_Int (Int (S));")
        write(b, "Write_Char (')');")
        write(b, "Write_Eol;")
    write(b, "end PI_String;")
    write(b, "")

    write_comment_block(b, "PI_Bool")
    write(b, "procedure PI_Bool (B : Boolean)")
    write(b, "is")
    continuation(b)
    write(b, "begin")
    with indent(b):
        write(b, "if B then")
        with indent(b):
            write(b, 'Write_Line ("True");')
        write(b, "else")
        with indent(b):
            write(b, 'Write_Line ("False");')
        write(b, "end if;")
    write(b, "end PI_Bool;")
    write(b, "")

    write_comment_block(b, "PS_List")
    write(b, "procedure PS_List (L : Irep_List; Name : String)")
    write(b, "is separate;")
    continuation(b)
    write(b, "")

    write_comment_block(b, "Print_Irep")
    write(b, "procedure Print_Irep (I : Irep)")
    write(b, "is")
    continuation(b)
    write(b, "begin")
    manual_indent(b)

    write(b, "PI_Irep (I);")
    write(b, "")

    write(b, "if I not in 1 .. Irep_Table.Last then")
    with indent(b):
        write(b, "return;")
    write(b, "end if;")
    write(b, "")

    write(b, "declare")
    with indent(b):
        write(b, "N : Irep_Node renames Irep_Table.Table (I);")
    write(b, "begin")
    manual_indent(b)
    write(b, "Indent;")
    write(b, 'Write_Str ("Source_Location = ");')
    write(b, 'Write_Int (Int (N.Sloc));')
    write(b, 'Write_Eol;')
    write(b, "case N.Kind is")
    manual_indent(b)
    for sn in top_sorted_sn:
        write(b, "when %s =>" % schemata[sn]["ada_name"])
        manual_indent(b)
        needs_null = True
        post = []
        for friendly_name in sorted(layout[sn]):
            needs_null = False
            layout_kind, layout_index, layout_typ = layout[sn][friendly_name]
            cn = ada_component_name(layout_kind, layout_index)
            write(b, 'Write_Str ("%s = ");' % ada_casing(friendly_name))
            if layout_kind == "str":
                assert layout_typ == "trivial"
                write(b, "PI_String (N.%s);" % cn)
            elif layout_kind == "bool":
                assert layout_typ == "trivial"
                write(b, 'PI_Bool (N.%s);' % cn)
            else:
                assert layout_kind == "int"
                if layout_typ == "irep":
                    write(b, "PI_Irep (Irep (N.%s));" % cn)
                elif layout_typ == "trivial":
                    write(b, 'Write_Int (Int (N.%s));' % cn)
                    write(b, "Write_Eol;")
                else:
                    assert layout_typ == "list"
                    write(b, "PI_List (Irep_List (N.%s), \"%s\");" %
                          (cn, friendly_name))
                    post.append((friendly_name, "N.%s" % cn))
        for friendly_name, node_field in post:
            write(b, "if %s /= 0 then" % node_field)
            with indent(b):
                write(b, "Write_Eol;")
                write(b, "PS_List (Irep_List (%s), \"%s\");" % (node_field,
                                                                friendly_name))
            write(b, "end if;")
        if needs_null:
            write(b, "null;")

        manual_outdent(b)
    manual_outdent(b)
    write(b, "end case;")
    write(b, "Outdent;")
    manual_outdent(b)
    write(b, "end;")

    manual_outdent(b)
    write(b, "end Print_Irep;")
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

    ##########################################################################
    # List Iteration

    write(s, "function List_First (L : Irep_List) return List_Cursor;")
    write(s, "")

    write_comment_block(b, "List_First")
    write(b, "function List_First (L : Irep_List) return List_Cursor")
    write(b, "is")
    continuation(b)
    write(b, "begin")
    with indent(b):
        write(b, "return C : List_Cursor do")
        with indent(b):
            write(b, "C.L := L;")
            write(b, "if L = 0 then")
            with indent(b):
                write(b, "C.Pos := 0;")
            write(b, "else")
            with indent(b):
                write(b, "C.Pos := To_Internal_List (Irep_List")
                write(b, "           (Irep_List_Table.Table (To_Internal_List (L)).A));")
            write(b, "end if;")
        write(b, "end return;")
    write(b, "end List_First;")
    write(b, "")

    write(s, "function List_Next (L : Irep_List; C : List_Cursor)")
    write(s, "                   return List_Cursor;")
    continuation(s)
    write(s, "")

    write_comment_block(b, "List_Next")
    write(b, "function List_Next (L : Irep_List; C : List_Cursor)")
    write(b, "                   return List_Cursor")
    continuation(b)
    write(b, "is")
    with indent(b):
        write(b, "pragma Assert (L /= 0 and L = C.L);")
        write(b, "pragma Assert (C.Pos /= 0);")
    write(b, "begin")
    with indent(b):
        write(b, "return Next : List_Cursor := C do")
        with indent(b):
            write(b, "Next.Pos := Irep_List_Table.Table (C.Pos).B;")
        write(b, "end return;")
    write(b, "end List_Next;")
    write(b, "")

    write(s, "function List_Has_Element (L : Irep_List; C : List_Cursor)")
    write(s, "                          return Boolean;")
    continuation(s)
    write(s, "")

    write_comment_block(b, "List_Has_Element")
    write(b, "function List_Has_Element (L : Irep_List; C : List_Cursor)")
    write(b, "                          return Boolean")
    continuation(b)
    write(b, "is")
    with indent(b):
        write(b, "pragma Assert (L = C.L);")
    write(b, "begin")
    with indent(b):
        write(b, "return C.L /= 0 and then C.Pos /= 0;")
    write(b, "end List_Has_Element;")
    write(b, "")

    write(s, "function List_Element (L : Irep_List; C : List_Cursor)")
    write(s, "                      return Irep;")
    continuation(s)
    write(s, "")

    write_comment_block(b, "List_Element")
    write(b, "function List_Element (L : Irep_List; C : List_Cursor)")
    write(b, "                      return Irep")
    continuation(b)
    write(b, "is")
    with indent(b):
        write(b, "pragma Assert (L /= 0 and L = C.L);")
        write(b, "pragma Assert (C.Pos /= 0);")
    write(b, "begin")
    with indent(b):
        write(b, "return Irep (Irep_List_Table.Table (C.Pos).A);")
    write(b, "end List_Element;")
    write(b, "")

    ##########################################################################
    # Private part

    manual_outdent(s)
    write(s, "private")
    write(s, "")
    manual_indent(s)

    write(s, "type Irep_List is range Integer'First + 1 .. 0;")
    write(s, "")

    write(s, "type Internal_Irep_List is range 0 .. -Irep_List'First;")
    write(s, "")

    write(s, "type List_Cursor is record")
    with indent(s):
        write(s, "L   : Irep_List;")
        write(s, "Pos : Internal_Irep_List;")
    write(s, "end record;")
    write(s, "")

    manual_outdent(s)
    write(s, "end Ireps;")
    write_file(s)

    manual_outdent(b)
    write(b, "end Ireps;")
    write_file(b)

    #pprint(schemata)

if __name__ == "__main__":
    main()
