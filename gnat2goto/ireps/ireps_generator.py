#!/usr/bin/env python
##############################################################################
##                                                                          ##
##                           GNAT2GOTO COMPONENTS                           ##
##                                                                          ##
##                I R E P S   T A B L E    G E N E R A T O R                ##
##                                                                          ##
##                   Copyright (C) 2017, Altran UK Limited                  ##
##                                                                          ##
## gnat2goto is  free  software;  you can redistribute it and/or  modify it ##
## under terms of the  GNU General Public License as published  by the Free ##
## Software  Foundation;  either version 3,  or (at your option)  any later ##
## version.  gnat2goto is distributed  in the hope that it will be  useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHAN- ##
## TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public ##
## License for  more details.  You should have  received  a copy of the GNU ##
## General  Public License  distributed with gnat2goto;  see file COPYING3. ##
## If not,  go to  http://www.gnu.org/licenses  for a complete  copy of the ##
## license.                                                                 ##
##                                                                          ##
##############################################################################

# Features left TODO:
# * code_ifthenelse we need to deal with the "optional" flag
# * recognize and remove Argument_List?
# * do not serialise "sub", "comment" or "namedSub" if they are empty

import os
import os.path
import json
import argparse

from pprint import pprint
from glob import glob
from copy import copy


IREP_TO_ADA_TYPE = {
    "irep"      : "Irep",
    "bool"      : "Boolean",
    "integer"   : "Integer",
    "string"    : "String",
    "gnat:sloc" : "Source_Ptr"
}

OP_NAME = {
    "+"      : "op_add",
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


# Helper functions

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
    rv = "%s_%%u" % ({"str"  : "Int",
                      "int"  : "Int",
                      "sloc" : "Int",
                      "bool" : "Bool"}[layout_kind])
    if layout_id is not None:
        rv = rv % layout_id
    return rv

def write_file(f):
    indent = 0
    instructions = f["content"]
    old_content = None
    try:
        with open(f["name"], "r") as fd:
            old_content = fd.read()
    except:
        pass # For example, if the file doesn't exist, leave old_content = None
    to_write = ""
    for i in instructions:
        if i["kind"] == "indent":
            indent += 1
        elif i["kind"] == "outdent":
            indent -= 1
        else:
            assert i["kind"] == "text"
            txt = "   " * indent + i["text"]
            to_write += (txt.rstrip() + "\n")
    # Avoid altering the file's mtime if we would write an identical file.
    # Useful for make et al.
    if old_content != to_write:
        with open(f["name"], "w") as fd:
            fd.write(to_write)
    assert indent == 0

def manual_indent(f):
    f["content"].append({"kind" : "indent"})
    f["indent"] += 1

def manual_outdent(f):
    f["content"].append({"kind" : "outdent"})
    f["indent"] -= 1

def write_raw(f, txt):
    f["content"].append({"kind" : "text",
                         "text" : str(txt).rstrip()})

def write(f, txt):
    if len(txt.rstrip()) + 3*f["indent"] < 80:
        write_raw(f, txt)
    else:
        bs = []
        last_space = None
        for pos, c in enumerate(str(txt).rstrip()):
            actual_pos = 3 * f["indent"] + pos
            if actual_pos < 80:
                if c == '(':
                    bs.append(pos)
                elif c == ')' and len(bs) > 0:
                    del bs[-1]
                elif c == ' ':
                    last_space = pos
            else:
                assert last_space is not None
                break
        if len(bs) == 0:
            bs = copy(f["bracket_stack"])
        a = txt[:last_space].rstrip()
        b = " " * (bs[-1] + 1) + txt[last_space:].strip()
        assert len(a) + f["indent"] * 3 < 80
        assert len(b) + f["indent"] * 3 < 80
        write_raw(f, a)
        write_raw(f, b)

    # Finally, we update the bracketing
    for pos, c in enumerate(str(txt.strip())):
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

def escape_reserved_words(w):
    if w in ("type", "subtype", "function", "array", "access", "body"):
        return "i_" + w
    else:
        return w

def initialiser_constant(val, typename):
    if val is None:
        return ""
    if typename == "Irep":
        assert val == "nil"
        return "Ireps.Empty"
    elif typename == "Boolean":
        assert type(val) == bool
        return str(val)
    elif typename == "Integer":
        assert type(val) == int
        return str(val)
    elif typename == "String":
        assert type(val) in (str, unicode)
        return "\"%s\"" % val

def initialiser(val, typename):
    init_const = initialiser_constant(val, typename)
    if init_const == "":
        return init_const
    else:
        return " := " + init_const


class IrepsGenerator(object):
    def __init__(self):
        self.schemata = {}
        self.top_sorted_sn = []
        self.summary_classes = {}
        self.sub_setters = {}
        self.named_setters = {}
        self.const = {}
        self.layout = {}

    # Emit subclasses for the enum
    def make_class(self, root, fd):
        name = self.schemata[root]["ada_name"].replace("I_", "Class_")
        subc = sorted(self.schemata[root]["subclasses"])
        first = subc[0]
        last = subc[-1]

        while len(self.schemata[first]["subclasses"]) >= 1:
            subc = sorted(self.schemata[first]["subclasses"])
            first = subc[0]
        while len(self.schemata[last]["subclasses"]) >= 1:
            subc = sorted(self.schemata[last]["subclasses"])
            last = subc[-1]

        write(fd, "subtype %s is Irep_Kind" % name)
        write(fd, "  range %s .. %s;" % (self.schemata[first]["ada_name"],
                                        self.schemata[last]["ada_name"]))
        continuation(fd)
        self.schemata[root]["subclass_ada_name"] = name
        self.summary_classes[name] =\
            set(self.top_sorted_sn[self.top_sorted_sn.index(first) :
                                   self.top_sorted_sn.index(last) + 1])

    def register_subclasses(self, sn, s, prefix_len):
        if self.schemata[sn]["used"]:
            write(s, " " * prefix_len + self.schemata[sn]["ada_name"] + ",")
            self.top_sorted_sn.append(sn)
        for sc in sorted(self.schemata[sn]["subclasses"]):
            self.register_subclasses(sc, s, prefix_len)

    def register_summary_classes(self, kind, todo, group):
        group_name = self.schemata[kind].get("subclass_ada_name", None)
        if group_name is not None:
            if self.summary_classes[group_name] <= todo:
                todo -= self.summary_classes[group_name]
                group.append(group_name)
        for sc in self.schemata[kind]["subclasses"]:
            self.register_summary_classes(sc, todo, group)

    # const ::= schema -> id|namedSub|comment -> {name: value}
    def register_constant(self, root_schema, kind, friendly_name, string_value):
        if root_schema not in self.const:
            self.const[str(root_schema)] = {}
        if kind not in self.const[root_schema]:
            self.const[root_schema][kind] = {}
        self.const[root_schema][kind][friendly_name] = string_value

        # Also apply to all children
        for sc in self.schemata[root_schema].get("subclasses", None):
            self.register_constant(sc,
                                   kind,
                                   friendly_name,
                                   string_value)

    def register_schema(self, sn):
        if sn == "source_location":
            return

        schema = self.schemata[sn]
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
            self.register_constant(sn, "id", "id", schema["id"])

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
                self.register_sub_setter(sn, i, friendly_name, element_type, True, None)

            elif "friendly_name" in sub:
                friendly_name = sub["friendly_name"]
                self.register_sub_setter(sn,
                                    i, friendly_name,
                                    sub["schema"],
                                    sub.get("number", None) == "*",
                                    sub.get("default", None))

            elif "number" in sub:
                assert sub["number"] == "*"
                friendly_name = "elmt"  # TODO: should have a nicer name
                element_type  = sub["schema"]
                self.register_sub_setter(sn, i, friendly_name, element_type, True, None)

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
                    self.register_constant(sn,
                                           fld,
                                           friendly_name,
                                           const_value)

                elif data.get("type", None) in ("string", "integer", "bool"):
                    # Trivial field
                    assert set(data.keys()) <= set(("type", "default"))
                    self.register_named_setter(sn,
                                          "trivial",
                                          friendly_name, data["type"],
                                          fld == "comment",
                                          data.get("default", None))

                elif "schema" in data:
                    # Irep of some type
                    assert data["schema"] in self.schemata
                    assert set(data.keys()) <= set(("schema", "default"))
                    value_type = data["schema"]
                    self.register_named_setter(sn,
                                          "irep",
                                          friendly_name, value_type,
                                          fld == "comment",
                                          data.get("default", None))

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
                    self.register_named_setter(sn,
                                        "list",
                                        friendly_name, list_type,
                                        fld == "comment",
                                        None)

                else:
                    assert False

        # const ::= schema -> id|namedSub|comment -> {name: value}
        # namd ::= setter_name -> value|list|trivial -> {schema: (is_comment, type)}
        # Delete setters for which we have a constant
        for kind in ("namedSub", "comment"):
            data = self.const.get(sn, {}).get(kind, {})
            for friendly_name, const_value in data.iteritems():
                if (friendly_name in self.named_setters and
                    "trivial" in self.named_setters[friendly_name] and
                    sn in self.named_setters[friendly_name]["trivial"]):
                    del self.named_setters[friendly_name]["trivial"][sn]

        if len(tmp) > 0:
            print "error: unconsumed data for %s:" % sn
            for item, data in tmp.iteritems():
                print "   %s: %s" % (item, data)

        for sc in schema.get("subclasses", None):
            self.register_schema(sc)

    # setter_name -> value|list|trivial -> {schema: (is_comment, type)}
    def register_named_setter(self,
                              root_schema,
                              kind,
                              friendly_name, value_type,
                              is_comment,
                              default_value):
        schema = self.schemata[root_schema]
        assert kind in ("trivial", "irep", "list")
        assert not kind == "trivial" or value_type in ("bool",
                                                    "string",
                                                    "integer")
        assert not kind != "trivial" or value_type in self.schemata

        actual_kind = kind
        actual_type = value_type
        if kind == "irep" and value_type == "source_location":
            # We magically map GNAT source locations to CPROVER source
            # locations
            actual_kind = "trivial"
            actual_type = "gnat:sloc"

        if friendly_name not in self.named_setters:
            self.named_setters[friendly_name] = {}
        if actual_kind not in self.named_setters[friendly_name]:
            self.named_setters[friendly_name][actual_kind] = {}
        self.named_setters[friendly_name][actual_kind][root_schema] = (is_comment,
                                                                actual_type,
                                                                default_value)

        # Also apply to all children
        for sc in schema.get("subclasses", None):
            self.register_named_setter(sc,
                                kind,
                                friendly_name, value_type,
                                is_comment,
                                default_value)

    # setter_name -> value|list -> {schema: (op_id, type)}
    def register_sub_setter(self,
                            root_schema,
                            op_id,
                            friendly_name,
                            value_schema,
                            is_list,
                            default_value):
        if type(friendly_name) is list:
            assert len(friendly_name) == 2
            assert friendly_name[0] == "op%u" % op_id
            friendly_name = friendly_name[1]

        schema = self.schemata[root_schema]
        setter_kind = "list" if is_list else "value"

        if friendly_name not in self.sub_setters:
            self.sub_setters[friendly_name] = {}
        if setter_kind not in self.sub_setters[friendly_name]:
            self.sub_setters[friendly_name][setter_kind] = {}
        self.sub_setters[friendly_name][setter_kind][root_schema] = (op_id,
                                                                value_schema,
                                                                default_value)

        # Also apply to all children
        for sc in schema.get("subclasses", None):
            self.register_sub_setter(sc, op_id, friendly_name, value_schema, is_list, default_value)

    def mk_precondition_in(self, param_name, kinds):
        todo = set(kinds)
        groups = []
        self.register_summary_classes("irep", todo, groups)
        things = sorted(groups + [self.schemata[x]["ada_name"] for x in todo])
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

    def all_used_subclasses(self, sn):
        """ return the set of all subclasses of sn (and itself) """
        rv = set()
        for sc in self.schemata[sn]["subclasses"]:
            rv |= self.all_used_subclasses(sc)
        if self.schemata[sn]["used"]:
            rv.add(sn)
        return rv

    # Debug output of hierarchy
    def export_to_dot(self, filename):
        with open(filename + ".dot", "w") as fd:
            fd.write("digraph G {\n")
            fd.write("graph [rankdir=LR,ranksep=3];\n")
            for sn in sorted(self.schemata):
                atr = []
                lbl = self.schemata[sn].get("id", None)
                if lbl is None or lbl == "":
                    lbl = sn
                if lbl != sn:
                    atr.append('label="%s"' % lbl)
                if not self.schemata[sn]["used"]:
                    atr.append("fontcolor=red")
                    atr.append("shape=none")
                fd.write(sn)
                if len(atr) > 0:
                    fd.write(' [%s];' % ",".join(atr))
                fd.write("\n")
            for sn, schema in self.schemata.iteritems():
                for sc in schema["subclasses"]:
                    fd.write('%s -> %s;\n' % (sn, sc))
            fd.write("}\n")
        os.system("dot " + filename + ".dot -Tpdf > " + filename + ".pdf")

    def emit_getter(self,
                    fn_name,
                    fn_kind,    # irep|trivial|list
                    value_type, # irep|bool|integer|string|gnat:sloc
                    inputs,     # map sn -> sn|None (if value_type != irep)
                    s,
                    b):
        assert fn_kind in ("irep", "trivial", "list")
        assert fn_kind != "irep" or value_type == "irep"
        assert fn_kind != "trivial" or value_type in ("bool",
                                                      "integer",
                                                      "string",
                                                      "gnat:sloc")
        assert fn_kind != "list" or value_type == "irep"
        is_list = fn_kind == "list"
        name = "Get_" + ada_casing(fn_name)

        if is_list:
            ada_value_type = "Irep_List"
        else:
            ada_value_type = IREP_TO_ADA_TYPE[value_type]

        precon = []
        i_kinds = set()
        for kind in inputs:
            i_kinds |= self.all_used_subclasses(kind)
        precon += self.mk_precondition_in("I", i_kinds)
        precon[-1] += ";"

        write(s, "function %s (I : Irep) return %s " % (name,
                                                        ada_value_type))
        for l in mk_prefixed_lines("with Pre => ", precon):
            write(s, l)
        write(s, "")

        kind_slot_map = {}
        for sn in sorted(i_kinds):
            layout_kind, layout_index, _ = self.layout[sn][fn_name]
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
        elif value_type in ("bool", "integer"):
            get_conversion = "%s"
        elif value_type == "string":
            get_conversion = "To_String (String_Id (%s))"
        elif value_type == "gnat:sloc":
            get_conversion = "Source_Ptr (%s)"
        else:
            assert False

        retval = get_conversion % field

        if len(kind_slot_map) == 0:
            assert False
        elif len(kind_slot_map) == 1:
            the_slot = list(kind_slot_map)[0]
            write(b, "return %s;" % (retval % the_slot))
        else:
            write(b, "case Irep_Table.Table (I).Kind is")
            manual_indent(b)
            for layout_index, i_kinds in kind_slot_map.iteritems():
                if len(i_kinds) == 1:
                    write(b, "when %s =>" %
                        ada_casing(self.schemata[i_kinds[0]]["ada_name"]))
                else:
                    for l in mk_prefixed_lines("when ",
                                            [self.schemata[x]["ada_name"]
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

    def emit_setter(self,
                    fn_name,
                    fn_kind,    # irep|trivial|list
                    value_type, # irep|bool|integer|string|gnat:sloc
                    inputs,     # map sn -> sn|None (if value_type != irep)
                    s,
                    b):
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

        ada_value_type = IREP_TO_ADA_TYPE[value_type]

        precon = []
        i_kinds = set()
        for kind in inputs:
            i_kinds |= self.all_used_subclasses(kind)
        precon += self.mk_precondition_in("I", i_kinds)
        if all_the_same:
            v_kinds = set()
            for kind in inputs.itervalues():
                if kind is not None:
                    v_kinds |= self.all_used_subclasses(kind)
            if len(v_kinds) > 0:
                precon[-1] += " and then"
                precon += self.mk_precondition_in("Value", v_kinds)
        precon[-1] += ";"

        write(s, "procedure %s (I : Irep; Value : %s)" % (name,
                                                        ada_value_type))
        for l in mk_prefixed_lines("with Pre => ", precon):
            write(s, l)
        if not all_the_same:
            write(s, "--  TODO: precondition for Value")
        write(s, "")

        kind_slot_map = {}
        for sn in sorted(i_kinds):
            layout_kind, layout_index, _ = self.layout[sn][fn_name]
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
        elif value_type in ("bool", "integer"):
            asn_rhs = "Value"
        elif value_type == "gnat:sloc":
            asn_rhs = "Integer (Value)"
        elif value_type == "string":
            write(b, "Start_String;")
            write(b, "Store_String_Chars (Value);")
            asn_rhs = "Integer (End_String)"
        else:
            assert False

        if len(kind_slot_map) == 0:
            assert False
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
                          ada_casing(self.schemata[i_kinds[0]]["ada_name"]))
                else:
                    for l in mk_prefixed_lines("when ",
                                               [self.schemata[x]["ada_name"]
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

    def optimize_layout(self, max_int, max_bool):
        accessors = {
            "int"  : set(),
            "bool" : set(),
        }
        req_fld = {
            "int"  : max_int,
            "bool" : max_bool,
        }

        for sn in self.layout:
            for friendly_name in self.layout[sn]:
                lo_typ, _, lo_kind = self.layout[sn][friendly_name]
                if lo_kind in ("irep", "list"):
                    a_kind = "int"
                elif lo_typ in ("int", "str", "sloc"):
                    a_kind = "int"
                else:
                    a_kind = "bool"
                accessors[a_kind].add(friendly_name)
        assert len(accessors["int"] & accessors["bool"]) == 0
        for kind in accessors:
            accessors[kind] = sorted(accessors[kind])

        for fld_kind, all_acc in accessors.iteritems():
            n = req_fld[fld_kind]
            f = new_file("%s_%u.smt2" % (fld_kind, n))

            write(f, "(set-logic QF_LIA)")
            write(f, "(set-option :produce-models true)")

            # Accessors
            for acc in all_acc:
                write(f, "(declare-const acc_%s Int)" % acc)

            # Restrict to fields
            for acc in all_acc:
                write(f, "(assert (<= 0 acc_%s %u))" % (acc, n - 1))

            # required accessors must be disjoint
            for sn in self.layout:
                tmp = []
                for friendly_name in self.layout[sn]:
                    if friendly_name in all_acc:
                        tmp.append("acc_" + friendly_name)
                if len(tmp) >= 2:
                    write(f, ";; for %s" % sn)
                    write_raw(f, "(assert (distinct %s))" % " ".join(tmp))

            write(f, "(check-sat)")
            write(f, "(get-model)")
            write(f, "(exit)")
            write_file(f)

            os.system("cvc4 %s_%u.smt2 > %s_%u.out" % (fld_kind, n,
                                                       fld_kind, n))

            with open("%s_%u.out" % (fld_kind, n), "rU") as fd:
                tmp = fd.read().strip()
            assert tmp.startswith("sat")

            optimal_lo = {}
            for line in tmp.splitlines():
                if line.startswith("(define-fun acc_"):
                    _, nam, _, _, pos = line.split()
                    nam = nam.replace("acc_", "")
                    assert nam in all_acc
                    pos = int(pos.rstrip(")"))
                    assert 0 <= pos < n
                    optimal_lo[nam] = pos
            assert sorted(optimal_lo) == all_acc

            # lo ::= schema -> friendly_name -> (str|int|bool|sloc,
            #                                    index,
            #                                    irep|list|trivial)

            for sn in self.layout:
                for friendly_name in self.layout[sn]:
                    if friendly_name in optimal_lo:
                        lo_typ, lo_idx, lo_kind = self.layout[sn][friendly_name]
                        lo_idx = optimal_lo[friendly_name]
                        self.layout[sn][friendly_name] = (lo_typ, lo_idx, lo_kind)


        return req_fld["int"], req_fld["bool"]

    def generate_code(self, optimize, schema_file_names):
        self.schemata = {}
        for schema_fn in schema_file_names:
            with open(schema_fn, "rU") as fd:
                sn = os.path.splitext(os.path.basename(schema_fn))[0]
                # print "Loading %s" % sn
                self.schemata[sn] = json.load(fd)
                self.schemata[sn]["subclasses"] = set()
                self.schemata[sn]["ada_name"] = ada_casing("i_" + sn)

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
        for sn, schema in self.schemata.iteritems():
            parent = schema.get("parent", None)
            if parent is None and sn != "irep":
                parent = "irep"
                schema["parent"] = parent
            if parent is not None:
                self.schemata[parent]["subclasses"].add(sn)

        # Expand trivial subclasses
        to_add = {}
        for sn, schema in self.schemata.iteritems():
            for sc_id in schema.get("trivial_subclass_ids", []):
                if sc_id in OP_NAME:
                    sc_name = OP_NAME[sc_id]
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
        self.schemata.update(to_add)

        # Flag nodes that will be supported
        for sn, schema in self.schemata.iteritems():
            schema["used"] = (len(schema["subclasses"]) == 0 or
                              sn in ("struct_type",
                                     "pointer_type",
                                     "signedbv_type"))
            if sn == "source_location":
                # We will be using the GNAT ones instead
                schema["used"] = False

        self.export_to_dot("tree")

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
        self.top_sorted_sn = []
        prefix = "type Irep_Kind is ("
        write(s, prefix + "I_Empty, --  For the Empty Irep")
        self.register_subclasses("irep", s, len(prefix))
        s["content"][-1]["text"] = s["content"][-1]["text"].rstrip(",") + ");"
        write(s, "")

        write(s, "subtype Valid_Irep_Kind is Irep_Kind")
        write(s, "  range Irep_Kind'Succ (Irep_Kind'First) .. Irep_Kind'Last;")
        write(s, "")

        # Emit subclasses for the enum
        self.make_class("unary_expr", s)
        self.make_class("binary_expr", s)
        self.make_class("nary_expr", s)
        self.make_class("code", s)
        self.make_class("bitvector_type", s)
        self.make_class("expr", s)
        self.make_class("type", s)
        write(s, "")

        # Collect and consolidate setters (subs, named and comment)
        # setter_name -> value|list -> {schema: (op_id, type)}
        self.sub_setters = {}
        # setter_name -> value|list|trivial -> {schema: (is_comment, type)}
        self.named_setters = {}
        # const ::= schema -> id|namedSub|comment -> {name: value}
        self.const = {}
        self.register_schema("irep")

        # Delete setters that only touch non-used classes (maybe we removed
        # some because they are always constant)
        setters_to_kill = []
        for setter_name in self.named_setters:
            kinds_to_kill = []
            for kind in self.named_setters[setter_name]:
                all_unused = True
                for sn in self.named_setters[setter_name][kind]:
                    if self.schemata[sn]["used"]:
                        all_unused = False
                        break
                if all_unused:
                    kinds_to_kill.append(kind)
            for kind in kinds_to_kill:
                del self.named_setters[setter_name][kind]
            if len(self.named_setters[setter_name]) == 0:
                setters_to_kill.append(setter_name)
        for setter_name in setters_to_kill:
            del self.named_setters[setter_name]

        ##########################################################################
        # Diagnostics after parsing schemata

        for setter_name, data in self.sub_setters.iteritems():
            if len(data) > 1:
                print "sub setter", setter_name, "conflicting kinds"
                pprint(data)

        for setter_name in (set(self.sub_setters) & set(self.named_setters)):
            print "both a sub and named:", setter_name
            print "> sub in  :", ", ".join(set(list(self.sub_setters[setter_name].itervalues())[0]))
            print "> named in:", ", ".join(set(list(self.named_setters[setter_name].itervalues())[0]))

        ##########################################################################
        # Layout

        op_counts = {}
        # schema -> int|str|bool
        #    where int includes irep, list, trivial integer

        self.layout = {}
        # schema -> friendly_name -> (str|int|bool|sloc, index, irep|list|trivial)

        for sn in self.top_sorted_sn:
            op_counts[sn] = {"int"  : 0,
                            "bool" : 0}
            self.layout[sn] = {}

            for setter_name, data in self.sub_setters.iteritems():
                assert len(data) == 1
                assert "value" in data or "list" in data
                typ = "list" if "list" in data else "irep"
                for kind, variants in data.iteritems():
                    if sn in variants:
                        self.layout[sn][setter_name] = ("int", op_counts[sn]["int"], typ)
                        op_counts[sn]["int"] += 1

            for setter_name, setter_kinds in self.named_setters.iteritems():
                for kind in setter_kinds:
                    if sn in setter_kinds[kind]:
                        is_comment, typ, _ = setter_kinds[kind][sn]
                        if kind in ("irep", "list") or typ == "integer":
                            l_typ = "trivial" if typ == "integer" else kind
                            self.layout[sn][setter_name] = ("int",
                                                    op_counts[sn]["int"],
                                                    l_typ)
                            op_counts[sn]["int"] += 1
                        elif typ == "string":
                            self.layout[sn][setter_name] = ("str",
                                                    op_counts[sn]["int"],
                                                    "trivial")
                            op_counts[sn]["int"] += 1
                        elif typ == "bool":
                            self.layout[sn][setter_name] = ("bool",
                                                    op_counts[sn]["bool"],
                                                    "trivial")
                            op_counts[sn]["bool"] += 1
                        elif typ == "gnat:sloc":
                            self.layout[sn][setter_name] = ("sloc",
                                                    op_counts[sn]["int"],
                                                    "trivial")
                            op_counts[sn]["int"] += 1
                        else:
                            print sn, setter_name, kind, typ
                            assert False

        MAX_INTS  = max(x["int"] for x in op_counts.itervalues())
        MAX_BOOLS = max(x["bool"] for x in op_counts.itervalues())

        if optimize:
            MAX_INTS, MAX_BOOLS = self.optimize_layout(MAX_INTS, MAX_BOOLS)



        self.generate_documentation(s)
        ##########################################################################
        # Datastructure

        write(b, "function To_String (S : String_Id) return String;")
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
        components = []
        size = 0
        for i in xrange(MAX_INTS):
            components.append(("Int_%u" % i, "Integer", None))
            size += 32
        for i in xrange(MAX_BOOLS):
            components.append(("Bool_%u" % i, "Boolean", "False"))
            size += 1
        components.append(("Kind", "Valid_Irep_Kind", None))
        size += 8
        with indent(b):
            max_len = max(map(len, (x[0] for x in components)))
            for cname, ctyp, default in components:
                if default is None:
                    write(b, "%-*s : %s;" % (max_len, cname, ctyp))
                else:
                    write(b, "%-*s : %s := %s;" % (max_len,
                                                cname,
                                                ctyp,
                                                default))
        write(b, "end record with Pack, Size => %u;" % size)
        write(b, "")

        write(b, "type Node_Storage_Kind is (S_Unused,")
        write(b, "                           S_Irep,")
        continuation(b)
        write(b, "                           S_List,")
        continuation(b)
        write(b, "                           S_Int,")
        continuation(b)
        write(b, "                           S_Sloc,")
        continuation(b)
        write(b, "                           S_Str);")
        continuation(b)
        write(b, "")

        write(b, "Empty_Default : constant array (Node_Storage_Kind) of Integer :=")
        write(b, "  (S_Unused => Integer'First,")
        write(b, "   S_Irep   => Integer (Empty),")
        write(b, "   S_List   => Integer (0),")
        write(b, "   S_Int    => Integer'First,")
        write(b, "   S_Sloc   => Integer (No_Location),")
        write(b, "   S_Str    => Integer (No_String));")
        write(b, "")

        write(b, "type Node_Semantics is record")
        with indent(b):
            for i in xrange(MAX_INTS):
                write(b, "Int_%u : Node_Storage_Kind;" % i)
        write(b, "end record;")
        write(b, "")

        write(b, "type Semantics_T is array (Valid_Irep_Kind) of Node_Semantics;")
        write(b, "")

        # lo ::= schema -> friendly_name -> (str|int|bool|sloc,
        #                                    index,
        #                                    irep|list|trivial)
        # sem ::= sn -> index -> unused|irep|list|int|sloc|str
        semantics = {}
        for sn in self.layout:
            semantics[sn] = {}
            for friendly_name in self.layout[sn]:
                lo_kind, lo_idx, lo_typ = self.layout[sn][friendly_name]
                if lo_kind in ("str", "int", "sloc"):
                    if lo_typ == "irep":
                        semantics[sn][lo_idx] = "irep"
                    elif lo_typ == "list":
                        semantics[sn][lo_idx] = "list"
                    elif lo_kind == "int":
                        semantics[sn][lo_idx] = "int"
                    elif lo_kind == "str":
                        semantics[sn][lo_idx] = "str"
                    elif lo_kind == "sloc":
                        semantics[sn][lo_idx] = "sloc"
                    else:
                        assert False
                for i in xrange(MAX_INTS):
                    if i not in semantics[sn]:
                        semantics[sn][i] = "unused"
        for sn in self.top_sorted_sn:
            if sn not in semantics:
                semantics[sn] = {}
            if len(semantics[sn]) == 0:
                for i in xrange(MAX_INTS):
                    semantics[sn][i] = "unused"

        max_len = max(map(len, (self.schemata[sn]["ada_name"] for sn in self.top_sorted_sn)))
        content = []
        for sn in self.top_sorted_sn:
            tmp = []
            for i in sorted(semantics[sn]):
                tmp.append("S_" + ada_casing(semantics[sn][i]))
            for i in xrange(len(tmp) - 1):
                tmp[i] += ","
            tmp[-1] += ")"
            content += mk_prefixed_lines("%-*s => (" % (max_len, self.schemata[sn]["ada_name"]),
                                        tmp)
            if sn != self.top_sorted_sn[-1]:
                content[-1] += ","
        content[-1] += ");"

        write(b, "Semantics : constant Semantics_T :=")
        for l in mk_prefixed_lines("(", content):
            write(b, "  " + l)
        write(b, "")

        write(b, "type Irep_List_Node is record")
        with indent(b):
            write(b, "A : Integer;            "
                    "--  Element [or pointer to first list link]")
            write(b, "B : Internal_Irep_List; "
                    "--  Next [or pointer to last list link]")
        write(b, "end record;")
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
            write(b, "N : constant Irep_List_Node := (A => 0,")
            write(b, "                                B => 0);")
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
                for sn in self.top_sorted_sn:
                    if sn in self.const and "id" in self.const[sn]:
                        write(b, "when %s =>" % self.schemata[sn]["ada_name"])
                        write(b, '   return "%s";' % self.const[sn]["id"]["id"])
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
            write(b, "N : Irep_Node;")
        write(b, "begin")
        with indent(b):
            write(b, "N.Kind  := Kind;")
            for i in xrange(MAX_INTS):
                write(b, "N.Int_%u := Empty_Default (Semantics (Kind).Int_%u);" % (i, i))
            write(b, "Irep_Table.Append (N);")
            write(b, "return Irep_Table.Last;")
        write(b, "end New_Irep;")
        write(b, "")

        for fn, category in [(self.emit_getter, "getters"),
                            (self.emit_setter, "setters")]:
            write(b, "-" * 70)
            write(b, "--  sub %s" % category)
            write(b, "-" * 70)
            write(b, "")

            # Print all setters for 'subs'
            # sub ::= setter_name -> value|list         -> {schema: (op_id, type)}
            for fn_name in sorted(self.sub_setters):
                assert len(self.sub_setters[fn_name]) == 1
                assert ("value" in self.sub_setters[fn_name] or
                        "list"  in self.sub_setters[fn_name])
                is_list = "list" in self.sub_setters[fn_name]
                data    = list(self.sub_setters[fn_name].itervalues())[0]

                fn(fn_name    = fn_name,
                fn_kind    = ("irep"
                                if "value" in self.sub_setters[fn_name]
                                else "list"),
                value_type = "irep",
                inputs     = dict((sn, data[sn][1]) for sn in data),
                s          = s,
                b          = b)

            write(b, "-" * 70)
            write(b, "--  namedSub and comment %s" % category)
            write(b, "-" * 70)
            write(b, "")

            # Print all setters for 'named' and 'comment'
            # nam ::= setter_name -> value|list|trivial ->
            #           {schema: (is_comment, type)}
            for fn_name in sorted(self.named_setters):
                assert len(self.named_setters[fn_name]) >= 1
                assert set(self.named_setters[fn_name]) <= set(["trivial",
                                                            "irep",
                                                            "list"])
                for kind in self.named_setters[fn_name]:
                    data = self.named_setters[fn_name][kind]

                    if kind == "trivial":
                        vt = list(x[1] for x in data.itervalues())[0]
                        inputs = dict((cls, None) for cls in data)
                    else:
                        vt = "irep"
                        inputs = dict((cls, data[cls][1]) for cls in data)
                    fn(fn_name    = fn_name,
                    fn_kind    = kind,
                    value_type = vt,
                    inputs     = inputs,
                    s          = s,
                    b          = b)

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

            write(b, "--  Visit children")
            for i in xrange(MAX_INTS):
                write(b, "case Semantics (Irep_Table.Table (I).Kind).Int_%u is" % i)
                with indent(b):
                    write(b, "when S_Irep =>")
                    with indent(b):
                        write(b, "Recurse (Irep (Irep_Table.Table (I).Int_%u));" % i)
                    write(b, "when S_List =>")
                    with indent(b):
                        write(b, "Recurse (Irep_List (Irep_Table.Table (I).Int_%u));" % i)
                    write(b, "when others => null;")
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
            write(b, "end return;")
        write(b, "end Trivial_String;")
        write(b, "")

        write_comment_block(b, "Trivial_Sloc")
        write(b, "function Trivial_Sloc (S : Source_Ptr) return JSON_Value")
        write(b, "is separate;")
        continuation(b)
        write(b, "")

        write_comment_block(b, "To_JSON")
        write(b, "function To_JSON (I : Irep) return JSON_Value")
        write(b, "is")
        with indent(b):
            write(b, "V : constant JSON_Value := Create_Object;")
        write(b, "begin")
        manual_indent(b)
        write(b, "if I = 0 then")
        with indent(b):
            write(b, "return Trivial_String (\"nil\");")
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
        write(b, 'V.Set_Field ("id", Id (I));')
        write(b, "case N.Kind is")

        for sn in self.top_sorted_sn:
            schema = self.schemata[sn]
            with indent(b):
                write(b, "when %s =>" % schema["ada_name"])
                with indent(b):
                    needs_null = True

                    # Set all subs
                    subs = {}
                    for setter_name in self.sub_setters:
                        for kind in self.sub_setters[setter_name]:
                            assert kind in ("value", "list")
                            if sn in self.sub_setters[setter_name][kind]:
                                op_id = self.sub_setters[setter_name][kind][sn][0]
                                subs[op_id] = (setter_name, kind == "list")
                    for i in xrange(len(subs)):
                        needs_null = False
                        setter_name, is_list = subs[i]
                        layout_kind, layout_index, layout_typ =\
                            self.layout[sn][setter_name]
                        tbl_field = "N." + ada_component_name(layout_kind,
                                                            layout_index)
                        if is_list:
                            assert len(subs) == 1
                            write(b, "Sub := To_JSON (Irep_List (%s));" % tbl_field)
                        else:
                            write(b, "if %s /= 0 then" % tbl_field)
                            with indent(b):
                                write(b, "Append (Sub, To_JSON (Irep (%s)));" %
                                tbl_field)
                            write(b, "end if;")

                    # Set all namedSub and comments
                    for setter_name in self.named_setters:
                        for kind in self.named_setters[setter_name]:
                            assert kind in ("irep", "list", "trivial")
                            if sn in self.named_setters[setter_name][kind]:
                                needs_null = False
                                is_comment, _, _ =\
                                    self.named_setters[setter_name][kind][sn]
                                layout_kind, layout_index, layout_typ =\
                                    self.layout[sn][setter_name]
                                tbl_field = "N." + ada_component_name(layout_kind,
                                                                    layout_index)

                                obj = "Comment" if is_comment else "Named_Sub"
                                if kind == "irep":
                                    val = "To_JSON (Irep (%s))" % tbl_field
                                elif layout_kind == "str":
                                    val = "Trivial_String (String_Id (%s))" % tbl_field
                                elif layout_kind == "int":
                                    val = "Trivial_Integer (%s)" % tbl_field
                                elif layout_kind == "bool":
                                    val = "Trivial_Boolean (%s)" % tbl_field
                                elif layout_kind == "sloc":
                                    val = "Trivial_Sloc (Source_Ptr (%s))" % tbl_field
                                else:
                                    assert False

                                if is_comment:
                                    key_name = "#" + setter_name
                                else:
                                    key_name = setter_name

                                tmp = "%s.Set_Field (" % obj
                                write(b, tmp + '"' + key_name + '",')
                                write(b, " " * len(tmp) + val + ");")
                                continuation(b)


                    # Set all constants
                    for kind, data in self.const.get(sn, {}).iteritems():
                        if kind == "id":
                            continue
                        elif kind == "namedSub":
                            obj = "Named_Sub"
                        elif kind == "comment":
                            obj = "Comment"
                        else:
                            print sn, kind, self.const[sn]
                            assert False
                        for const_name, const_value in data.iteritems():
                            needs_null = False
                            tmp = "%s.Set_Field (" % obj
                            write(b, tmp + '"%s"' % const_name + ",")
                            write(b, " " * len(tmp) + 'Trivial_String ("%s"));'
                                % const_value)
                            continuation(b)

                    if needs_null:
                        write(b, "null;")
                    write(b, "")
        write(b, "end case;")
        write(b, "")
        write(b, 'if Length (Sub) /= 0 then')
        with indent(b):
            write(b, 'V.Set_Field ("sub",      Sub);')
        write(b, 'end if;')
        write(b, 'if not Is_Empty (Named_Sub) then')
        with indent(b):
            write(b, 'V.Set_Field ("namedSub", Named_Sub);')
        write(b, 'end if;')
        write(b, 'if not Is_Empty (Comment) then')
        with indent(b):
            write(b, 'V.Set_Field ("comment",  Comment);')
        write(b, 'end if;')
        manual_outdent(b)
        write(b, "end;")
        write(b, "return V;")
        manual_outdent(b)
        write(b, "end To_JSON;")
        write(b, "")

        ##########################################################################
        # Record-style constructors

        # Invert the sub_setter and named_setter maps:
        # (Omit list-typed subexpressions for now)
        sub_setters_by_schema = {}
        named_setters_by_schema = {}

        for (friendly_name, kinds) in self.sub_setters.iteritems():
            for (kind, schema_names) in kinds.iteritems():
                if kind == "list":
                    continue
                for (schema_name, (_, _, default_value)) in schema_names.iteritems():
                    if schema_name not in sub_setters_by_schema:
                        sub_setters_by_schema[schema_name] = []
                    sub_setters_by_schema[schema_name].append((friendly_name, default_value))

        for (friendly_name, kinds) in self.named_setters.iteritems():
            for (kind, schema_names) in kinds.iteritems():
                for (schema_name, (is_comment, actual_type, default_val)) in schema_names.iteritems():
                    if schema_name not in named_setters_by_schema:
                        named_setters_by_schema[schema_name] = []
                    param_type = actual_type if kind == "trivial" else kind
                    named_setters_by_schema[schema_name].append((friendly_name, param_type, default_val))

        for sn in self.top_sorted_sn:
            schema = self.schemata[sn]
            proc_name = "Make_%s" % schema["ada_name"][2:]

            formal_args = []
            # Print args named for each non-list member:
            if sn in sub_setters_by_schema:
                for (friendly_name, default_value) in sub_setters_by_schema[sn]:
                    formal_name = escape_reserved_words(friendly_name)
                    formal_args.append((ada_casing(formal_name), ada_casing(friendly_name), "Irep", default_value))
            if sn in named_setters_by_schema:
                for (friendly_name, actual_type, default_value) in named_setters_by_schema[sn]:
                    formal_name = escape_reserved_words(friendly_name)
                    ada_type = IREP_TO_ADA_TYPE[actual_type]
                    formal_args.append((ada_casing(formal_name), ada_casing(friendly_name), ada_type, default_value))
            has_args = len(formal_args) != 0
            open_paren = "(" if has_args else ""
            write(b, "function %s %s" % (proc_name, open_paren))
            write(s, "function %s %s" % (proc_name, open_paren))
            arg_strings = ["%s : %s%s" % (formal_name, formal_type, initialiser(default_value, formal_type)) for
                        (formal_name, _, formal_type, default_value) in formal_args]
            arg_strings[:-1] = ["%s;" % argstr for argstr in arg_strings[:-1]]
            with indent(b), indent(s):
                for argstr in arg_strings:
                    write(b, argstr)
                    write(s, argstr)
            if has_args:
                write(b, ")")
                write(s, ")")
            write(b, "return Irep is")
            write(s, "return Irep;")
            with indent(b):
                write(b, "Ret : constant Irep := New_Irep (%s);" % schema["ada_name"])
            write(b, "begin")
            with indent(b):
                for (formal_name, friendly_name, formal_type, _) in formal_args:
                    if formal_type == "Irep":
                        write(b, "if %s /= Ireps.Empty then" % formal_name)
                        manual_indent(b)
                    write(b, "Set_%s (Ret, %s);" % (friendly_name, formal_name))
                    if formal_type == "Irep":
                        manual_outdent(b)
                        write(b, "end if;")
                write(b, "return Ret;")
            write(b, "end %s;" % proc_name)
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
        write(b, "procedure PI_Sloc (S : Source_Ptr);")
        write(b, "--  Print one-line source location")
        write(b, "--    e.g. \"foo.bar:42:666\" (Source_Ptr=12345)")
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
                for sn in self.top_sorted_sn:
                    write(b, "when %s =>" % self.schemata[sn]["ada_name"])
                    write(b, "   return \"%s\";" % self.schemata[sn]["ada_name"])
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

        write_comment_block(b, "PI_Sloc")
        write(b, "procedure PI_Sloc (S : Source_Ptr)")
        write(b, "is")
        continuation(b)
        write(b, "begin")
        with indent(b):
            write(b, "if S = No_Location then")
            with indent(b):
                write(b, 'Write_Str ("No_Location");')
            write(b, "else")
            with indent(b):
                write(b, 'Write_Str ("TODO");')
            write(b, "end if;")
            write(b, 'Write_Str (" (Source_Ptr=");')
            write(b, "Write_Int (Int (S));")
            write(b, "Write_Char (')');")
            write(b, "Write_Eol;")
        write(b, "end PI_Sloc;")
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
        write(b, "case N.Kind is")
        manual_indent(b)
        for sn in self.top_sorted_sn:
            write(b, "when %s =>" % self.schemata[sn]["ada_name"])
            manual_indent(b)
            needs_null = True
            post = []
            for friendly_name in sorted(self.layout[sn]):
                needs_null = False
                layout_kind, layout_index, layout_typ = self.layout[sn][friendly_name]
                cn = ada_component_name(layout_kind, layout_index)
                write(b, 'Write_Str ("%s = ");' % ada_casing(friendly_name))
                if layout_kind == "str":
                    assert layout_typ == "trivial"
                    write(b, "PI_String (String_Id (N.%s));" % cn)
                elif layout_kind == "bool":
                    assert layout_typ == "trivial"
                    write(b, 'PI_Bool (N.%s);' % cn)
                elif layout_kind == "sloc":
                    assert layout_typ == "trivial"
                    write(b, "PI_Sloc (Source_Ptr (N.%s));" % cn)
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

    ##########################################################################
    # Documentation
    def generate_documentation(self, s):
        for sn in self.top_sorted_sn:
            schema = self.schemata[sn]
            assert schema["used"]

            write(s, "--  %s" % schema["ada_name"])

            # sub_setters ::= setter_name -> value|list -> {schema: (op_id, type)}
            # subs        ::= op_id -> (setter_name, type)
            subs = {}
            for setter_name, data in self.sub_setters.iteritems():
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
            for setter_name, setter_kinds in self.named_setters.iteritems():
                for kind in setter_kinds:
                    if sn in setter_kinds[kind]:
                        is_comment, typ, _ = setter_kinds[kind][sn]
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
            for kind in self.const.get(sn, {}):
                for const_name, const_value in self.const[sn][kind].iteritems():
                    tmp = "constant %s: %s" % (ada_casing(const_name), const_value)
                    if kind != "id":
                        tmp += " (%s)" % kind
                    write(s, "--  %s" % tmp)

            write(s, "")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("-O", "--optimize",
                    action="store_true",
                    help="Optimise table layout. Requires CVC4 binary on path.")

    args = ap.parse_args()

    mypath = os.path.dirname(os.path.realpath(__file__))

    schema_files = glob(os.path.join(mypath, "irep_specs", "*.json"))

    irep_gen = IrepsGenerator()
    irep_gen.generate_code(optimize = args.optimize,
                schema_file_names = schema_files)


if __name__ == "__main__":
    main()
