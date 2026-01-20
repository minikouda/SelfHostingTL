#!/usr/bin/env python3
from __future__ import annotations

import sys
import re
import json
from dataclasses import dataclass
from typing import Any

# --- AST types
@dataclass(frozen=True)
class Sym:
    name: str

# --- Tokenizer / parser (same sexpr grammar as VM)
_tok_re = re.compile(
    r"""\s*(?:
        (?P<COMMENT>;[^\n]*) |
        (?P<LP>\() |
        (?P<RP>\)) |
        (?P<STR>"([^"\\]|\\.)*") |
        (?P<INT>-?\d+) |
        (?P<SYM>[A-Za-z_+\-*/<>=!?][A-Za-z0-9_+\-*/<>=!?]*)
    )""",
    re.VERBOSE,
)

def _unescape_string(s: str) -> str:
    body = s[1:-1]
    return bytes(body, "utf-8").decode("unicode_escape")

def tokenize(src: str):
    i = 0
    out = []
    while i < len(src):
        m = _tok_re.match(src, i)
        if not m:
            raise SyntaxError(f"Unexpected character at {i}: {src[i:i+30]!r}")
        i = m.end()
        kind = m.lastgroup
        if kind == "COMMENT":
            continue
        if kind == "LP":
            out.append(("LP", "("))
        elif kind == "RP":
            out.append(("RP", ")"))
        elif kind == "STR":
            out.append(("STR", _unescape_string(m.group("STR"))))
        elif kind == "INT":
            out.append(("INT", int(m.group("INT"))))
        elif kind == "SYM":
            out.append(("SYM", m.group("SYM")))
    out.append(("EOF", None))
    return out

def parse_sexprs(src: str):
    toks = tokenize(src)
    k = 0

    def peek():
        return toks[k]

    def eat(tt=None):
        nonlocal k
        t = toks[k]
        if tt and t[0] != tt:
            raise SyntaxError(f"Expected {tt}, got {t}")
        k += 1
        return t

    def parse_one():
        t = peek()
        if t[0] == "INT":
            eat("INT")
            return t[1]
        if t[0] == "STR":
            eat("STR")
            return t[1]
        if t[0] == "SYM":
            eat("SYM")
            return Sym(t[1])
        if t[0] == "LP":
            eat("LP")
            lst = []
            while peek()[0] != "RP":
                if peek()[0] == "EOF":
                    raise SyntaxError("Unclosed '('")
                lst.append(parse_one())
            eat("RP")
            return lst
        raise SyntaxError(f"Bad token: {t}")

    forms = []
    while peek()[0] != "EOF":
        forms.append(parse_one())
    return forms

# --- Compiler
class C:
    def __init__(self):
        self.bc: list[str] = []
        self.label_id = 0
        self.prim_set = {
            "read-all","parse-sexprs","emit","gensym",
            "str-cat","to-str",
            "sym","sym-name","sym-eq?",
            "int?","sym?","pair?","null?","str?","json-dumps",
            "car","cdr","error",
        }

    def gensym(self, p="L"):
        self.label_id += 1
        return f"{p}{self.label_id}"

    def emit(self, s: str):
        self.bc.append(s)

    def symname(self, x: Any) -> str:
        if not isinstance(x, Sym):
            raise SyntaxError(f"Expected symbol, got {x!r}")
        return x.name

    def compile_program(self, forms: list[Any]) -> str:
        # jump over function bodies
        start = "__START__"
        self.emit(f"JMP {start}")

        # compile DEFUN blocks for all top-level defines first
        # then compile remaining forms at __START__
        defines = []
        rest = []
        for f in forms:
            if isinstance(f, list) and f and isinstance(f[0], Sym) and f[0].name == "define":
                defines.append(f)
            else:
                rest.append(f)

        for d in defines:
            self.compile_define(d)

        self.emit(f"LABEL {start}")
        for f in rest:
            self.compile_form(f)

        # Top-level return to stop VM cleanly
        self.emit("PUSH 0")
        self.emit("RET")
        return "\n".join(self.bc)

    def compile_define(self, form: list[Any]):
        # (define (fname p1 p2) body)
        # or (define x expr) [we compile as global assignment at __START__ instead; keep it simple]
        if len(form) != 3:
            raise SyntaxError("define: expected (define (f args..) body)")
        sig = form[1]
        body = form[2]
        if not (isinstance(sig, list) and sig and isinstance(sig[0], Sym)):
            raise SyntaxError("define: function form only, e.g. (define (f x) body)")
        fname = sig[0].name
        params = [self.symname(p) for p in sig[1:]]
        self.emit("DEFUN " + " ".join([fname] + params))
        self.compile_form(body)
        # ensure return value exists
        self.emit("RET")

    def compile_form(self, x: Any):
        if isinstance(x, int):
            self.emit(f"PUSH {x}")
            return
        if isinstance(x, str):
            self.emit(f"PUSHSTR {json.dumps(x)}")
            return
        if isinstance(x, Sym):
            self.emit(f"LOAD {x.name}")
            return
        if not isinstance(x, list) or len(x) == 0:
            # nil -> push empty list not supported; use 0
            self.emit("PUSH 0")
            return

        op = x[0]
        args = x[1:]

        if isinstance(op, Sym) and op.name == "begin":
            for a in args:
                self.compile_form(a)
            return

        if isinstance(op, Sym) and op.name == "if":
            if len(args) != 3:
                raise SyntaxError("if: expected (if cond then else)")
            cond, thn, els = args
            l_else = self.gensym("ELSE")
            l_end = self.gensym("END")
            self.compile_form(cond)
            self.emit(f"JZ {l_else}")
            self.compile_form(thn)
            self.emit(f"JMP {l_end}")
            self.emit(f"LABEL {l_else}")
            self.compile_form(els)
            self.emit(f"LABEL {l_end}")
            return

        if isinstance(op, Sym) and op.name in ("let", "set"):
            if len(args) != 2 or not isinstance(args[0], Sym):
                raise SyntaxError(f"{op.name}: expected ({op.name} x expr)")
            name = args[0].name
            self.compile_form(args[1])
            self.emit(f"STORE {name}")
            return

        if isinstance(op, Sym) and op.name == "while":
            if len(args) < 2:
                raise SyntaxError("while: expected (while cond body...)")
            cond = args[0]
            body = args[1:]
            top = self.gensym("TOP")
            end = self.gensym("END")
            self.emit(f"LABEL {top}")
            self.compile_form(cond)
            self.emit(f"JZ {end}")
            for st in body:
                self.compile_form(st)
            self.emit(f"JMP {top}")
            self.emit(f"LABEL {end}")
            # value of while is 0
            self.emit("PUSH 0")
            return

        # Built-in ops compiled to instructions
        if isinstance(op, Sym) and op.name in ("+", "-", "*", "/", "<", "=="):
            if len(args) != 2:
                raise SyntaxError(f"{op.name}: expected 2 args")
            self.compile_form(args[0])
            self.compile_form(args[1])
            self.emit({"+":"ADD","-":"SUB","*":"MUL","/":"DIV","<":"LT","==":"EQ"}[op.name])
            return

        if isinstance(op, Sym) and op.name == "print":
            if len(args) != 1:
                raise SyntaxError("print: expected 1 arg")
            self.compile_form(args[0])
            self.emit("PRINT")
            self.emit("PUSH 0")
            return

        # Function call or primitive call
        if not isinstance(op, Sym):
            raise SyntaxError("call: operator must be a symbol")

        for a in args:
            self.compile_form(a)

        name = op.name
        if name in self.prim_set:
            self.emit(f"CALLPRIM {name} {len(args)}")
        else:
            self.emit(f"CALL {name} {len(args)}")

def main():
    src = sys.stdin.read()
    forms = parse_sexprs(src)
    c = C()
    print(c.compile_program(forms))

if __name__ == "__main__":
    main()
