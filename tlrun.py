#!/usr/bin/env python3
# tlrun.py — TinyLisp interpreter runner with primitives to run compiler.tl

from __future__ import annotations
import sys
import re
from dataclasses import dataclass
from typing import Any, Callable

# ---------- Symbol ----------
@dataclass(frozen=True)
class Sym:
    name: str
    def __repr__(self) -> str:
        return f"'{self.name}"

# ---------- S-expression parser (supports ; comments) ----------
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
    # s includes quotes
    body = s[1:-1]
    return bytes(body, "utf-8").decode("unicode_escape")

def tokenize(src: str):
    i = 0
    out = []
    while i < len(src):
        m = _tok_re.match(src, i)
        if not m:
            raise SyntaxError(f"Unexpected character at {i}: {src[i:i+20]!r}")
        i = m.end()
        if m.lastgroup == "COMMENT":
            continue
        if m.lastgroup == "LP":
            out.append(("LP", "("))
        elif m.lastgroup == "RP":
            out.append(("RP", ")"))
        elif m.lastgroup == "STR":
            out.append(("STR", _unescape_string(m.group("STR"))))
        elif m.lastgroup == "INT":
            out.append(("INT", int(m.group("INT"))))
        elif m.lastgroup == "SYM":
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

# ---------- Interpreter ----------
Env = dict[str, Any]

@dataclass
class Function:
    params: list[str]
    body: Any
    env: Env  # closure

class TL:
    def __init__(self):
        self.gensym_id = 0
        self.global_env: Env = {}
        self._install_builtins()

    def _install_builtins(self):
        def builtin(name: str):
            def deco(fn: Callable[..., Any]):
                self.global_env[name] = fn
                return fn
            return deco

        # --- IO / compiler primitives ---
        @builtin("read-all")
        def _read_all() -> str:
            return sys.stdin.read()

        @builtin("parse-sexprs")
        def _parse(s: str):
            return parse_sexprs(s)

        @builtin("emit")
        def _emit(line: str):
            # one bytecode instruction per line
            sys.stdout.write(line + "\n")
            return 0

        @builtin("gensym")
        def _gensym(prefix: str) -> str:
            self.gensym_id += 1
            return f"{prefix}{self.gensym_id}"

        # --- type predicates ---
        @builtin("int?")
        def _intp(x): return 1 if isinstance(x, int) else 0

        @builtin("str?")
        def _strp(x): return 1 if isinstance(x, str) else 0

        @builtin("sym?")
        def _symp(x): return 1 if isinstance(x, Sym) else 0

        @builtin("pair?")
        def _pairp(x): return 1 if isinstance(x, list) and len(x) > 0 else 0

        @builtin("null?")
        def _nullp(x): return 1 if x == [] else 0

        # --- list ops (lists are Python lists) ---
        @builtin("car")
        def _car(x): return x[0]

        @builtin("cdr")
        def _cdr(x): return x[1:]

        # --- symbol helpers ---
        @builtin("sym-name")
        def _sym_name(x: Sym) -> str: return x.name

        @builtin("sym")
        def _sym(x: str) -> Sym: return Sym(x)

        @builtin("sym-eq?")
        def _sym_eq(a: Sym, b: Sym) -> int:
            return 1 if isinstance(a, Sym) and isinstance(b, Sym) and a.name == b.name else 0

        # --- numeric ops ---
        @builtin("+")
        def _add(a, b): return a + b

        @builtin("-")
        def _sub(a, b): return a - b

        @builtin("*")
        def _mul(a, b): return a * b

        @builtin("/")
        def _div(a, b): return a // b

        @builtin("<")
        def _lt(a, b): return 1 if a < b else 0

        @builtin("==")
        def _eq(a, b): return 1 if a == b else 0

        # --- string utils ---
        @builtin("to-str")
        def _to_str(x) -> str: return str(x)

        @builtin("str-cat")
        def _cat(a: str, b: str) -> str: return a + b

        # --- errors ---
        @builtin("error")
        def _err(msg: str):
            raise RuntimeError(msg)

    def eval(self, expr: Any, env: Env | None = None) -> Any:
        if env is None:
            env = self.global_env

        # atoms
        if isinstance(expr, int) or isinstance(expr, str):
            return expr
        if isinstance(expr, Sym):
            name = expr.name
            if name in env:
                return env[name]
            raise NameError(f"Unbound symbol: {name}")

        # lists
        if not isinstance(expr, list) or len(expr) == 0:
            return []

        head = expr[0]
        args = expr[1:]

        # special forms (define / if / begin / let)
        if isinstance(head, Sym) and head.name == "begin":
            val = 0
            for a in args:
                val = self.eval(a, env)
            return val

        if isinstance(head, Sym) and head.name == "if":
            if len(args) != 3:
                raise SyntaxError("if: expected (if cond then else)")
            cond = self.eval(args[0], env)
            return self.eval(args[1], env) if cond != 0 else self.eval(args[2], env)

        if isinstance(head, Sym) and head.name == "define":
            # (define (f a b) body) or (define x expr)
            if len(args) != 2:
                raise SyntaxError("define: expected 2 args")
            sig = args[0]
            body = args[1]
            if isinstance(sig, list):
                if len(sig) < 1 or not isinstance(sig[0], Sym):
                    raise SyntaxError("define: bad function signature")
                fname = sig[0].name
                params = []
                for p in sig[1:]:
                    if not isinstance(p, Sym):
                        raise SyntaxError("define: param must be symbol")
                    params.append(p.name)
                env[fname] = Function(params=params, body=body, env=env)
                return 0
            else:
                if not isinstance(sig, Sym):
                    raise SyntaxError("define: name must be symbol")
                env[sig.name] = self.eval(body, env)
                return 0

        if isinstance(head, Sym) and head.name == "let":
            # (let x expr) binds/sets in current env
            if len(args) != 2 or not isinstance(args[0], Sym):
                raise SyntaxError("let: expected (let x expr)")
            env[args[0].name] = self.eval(args[1], env)
            return 0

        # function call
        fn = self.eval(head, env)
        evaled_args = [self.eval(a, env) for a in args]

        # builtin python function
        if callable(fn) and not isinstance(fn, Function):
            return fn(*evaled_args)

        # user function
        if isinstance(fn, Function):
            if len(evaled_args) != len(fn.params):
                raise TypeError("arity mismatch")
            new_env = dict(fn.env)
            for p, v in zip(fn.params, evaled_args):
                new_env[p] = v
            return self.eval(fn.body, new_env)

        raise TypeError(f"Not callable: {fn!r}")


def main():
    tl = TL()
    #   python tlrun.py compiler.tl              (runs compiler.tl; compiler reads stdin)
    #   python tlrun.py compiler.tl program.tl   (runs compiler.tl; read-all reads program.tl)
    if len(sys.argv) >= 2:
        program_text = open(sys.argv[1], "r", encoding="utf-8").read()
    else:
        program_text = sys.stdin.read()

    forms = parse_sexprs(program_text)

    # If a second filename is provided, preload it as the input for (read-all)
    if len(sys.argv) >= 3:
        src_to_compile = open(sys.argv[2], "r", encoding="utf-8").read()

        # Override the read-all builtin to return the provided source file
        def _read_all_override():
            return src_to_compile

        tl.global_env["read-all"] = _read_all_override

    for f in forms:
        tl.eval(f)


if __name__ == "__main__":
    main()
