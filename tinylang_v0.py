# tinylisp_v0.py
# Compile TinyLisp S-expressions -> stack VM bytecode

import re
from typing import Any, List, Tuple, Union

Token = Tuple[str, str]  # (type, value)

def tokenize(src: str) -> List[Token]:
    # tokens: '(', ')', integers, symbols
    pattern = r"""\s*(?:
        (?P<LPAREN>\() |
        (?P<RPAREN>\)) |
        (?P<INT>-?\d+) |
        (?P<SYM>[A-Za-z_+\-*/<>=!?][A-Za-z0-9_+\-*/<>=!?]*)
    )"""
    rx = re.compile(pattern, re.VERBOSE)
    toks: List[Token] = []
    i = 0
    while i < len(src):
        if src[i] == ";":
            j = src.find("\n", i)
            if j == -1:
                break  # comment goes to EOF
            i = j + 1
            continue

        m = rx.match(src, i)
        if not m:
            raise SyntaxError(f"Unexpected character at {i}: {src[i:i+20]!r}")
        i = m.end()
        kind = m.lastgroup
        val = m.group(kind) if kind else ""
        if kind == "LPAREN":
            toks.append(("LPAREN", val))
        elif kind == "RPAREN":
            toks.append(("RPAREN", val))
        elif kind == "INT":
            toks.append(("INT", val))
        elif kind == "SYM":
            toks.append(("SYM", val))
    toks.append(("EOF", ""))
    return toks

def parse(tokens: List[Token]) -> Any:
    # Parse into nested lists, ints, and symbols (as strings)
    k = 0

    def peek() -> Token:
        return tokens[k]

    def eat(t: str = None) -> Token:
        nonlocal k
        tok = tokens[k]
        if t and tok[0] != t:
            raise SyntaxError(f"Expected {t}, got {tok}")
        k += 1
        return tok

    def parse_expr() -> Any:
        tok = peek()
        if tok[0] == "INT":
            eat("INT")
            return int(tok[1])
        if tok[0] == "SYM":
            eat("SYM")
            return tok[1]  # symbol name as string
        if tok[0] == "LPAREN":
            eat("LPAREN")
            lst = []
            while peek()[0] != "RPAREN":
                if peek()[0] == "EOF":
                    raise SyntaxError("Unclosed '('")
                lst.append(parse_expr())
            eat("RPAREN")
            return lst
        raise SyntaxError(f"Bad token: {tok}")

    forms: List[Any] = []
    while peek()[0] != "EOF":
        forms.append(parse_expr())
    return forms  # top-level is a list of forms

class Compiler:
    def __init__(self):
        self.bc: List[str] = []
        self._label_id = 0

    def gensym(self, prefix="L") -> str:
        self._label_id += 1
        return f"{prefix}{self._label_id}"

    def emit(self, line: str) -> None:
        self.bc.append(line)

    def compile_program(self, forms: List[Any]) -> List[str]:
        for f in forms:
            self.compile_form(f)
        return self.bc

    def compile_form(self, form: Any) -> None:
        # atoms
        if isinstance(form, int):
            self.emit(f"PUSH {form}")
            return
        if isinstance(form, str):
            # symbol => variable load
            self.emit(f"LOAD {form}")
            return
        if not isinstance(form, list) or len(form) == 0:
            raise SyntaxError(f"Bad form: {form!r}")

        op = form[0]
        args = form[1:]

        if op == "print":
            self._expect_arity(op, args, 1)
            self.compile_form(args[0])
            self.emit("PRINT")
            return

        if op == "+":
            self._expect_arity(op, args, 2)
            self.compile_form(args[0])
            self.compile_form(args[1])
            self.emit("ADD")
            return

        if op == "-":
            self._expect_arity(op, args, 2)
            self.compile_form(args[0])
            self.compile_form(args[1])
            self.emit("SUB")
            return

        if op == "*":
            self._expect_arity(op, args, 2)
            self.compile_form(args[0])
            self.compile_form(args[1])
            self.emit("MUL")
            return

        if op == "/":
            self._expect_arity(op, args, 2)
            self.compile_form(args[0])
            self.compile_form(args[1])
            self.emit("DIV")
            return

        if op == "<":
            self._expect_arity(op, args, 2)
            self.compile_form(args[0])
            self.compile_form(args[1])
            self.emit("LT")
            return

        if op == "==":
            self._expect_arity(op, args, 2)
            self.compile_form(args[0])
            self.compile_form(args[1])
            self.emit("EQ")
            return

        if op in ("let", "set"):
            # (let x expr) or (set x expr)
            self._expect_arity(op, args, 2)
            name = args[0]
            if not isinstance(name, str):
                raise SyntaxError(f"{op}: first arg must be a symbol, got {name!r}")
            self.compile_form(args[1])
            self.emit(f"STORE {name}")
            return

        if op == "begin":
            for a in args:
                self.compile_form(a)
            return

        if op == "while":
            if len(args) < 2:
                raise SyntaxError("while: need (while cond body...)")
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
            return

        raise SyntaxError(f"Unknown op: {op!r} in {form!r}")

    def _expect_arity(self, op: str, args: List[Any], n: int) -> None:
        if len(args) != n:
            raise SyntaxError(f"{op}: expected {n} args, got {len(args)}")

def compile_src(src: str) -> str:
    toks = tokenize(src)
    forms = parse(toks)
    c = Compiler()
    bc = c.compile_program(forms)
    return "\n".join(bc)

if __name__ == "__main__":
    import sys
    src = sys.stdin.read()
    print(compile_src(src))
