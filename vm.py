#!/usr/bin/env python3
from __future__ import annotations

import sys
import json
import re
from dataclasses import dataclass
from typing import Any, Callable

# --------------------------
# TinyLisp AST types
# --------------------------

@dataclass(frozen=True)
class Sym:
    name: str
    def __repr__(self) -> str:
        return f"Sym({self.name})"

# --------------------------
# S-expression parser (primitive parse-sexprs)
# Supports:
#   - integers
#   - strings "..."
#   - symbols
#   - lists (...)
#   - ; comments to end of line
# --------------------------

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

def tokenize_sexpr(src: str):
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
    toks = tokenize_sexpr(src)
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

# --------------------------
# Bytecode VM
# --------------------------

def run(bytecode_text: str, stdin_text: str) -> str:
    # Parse program lines into tokens
    prog = []
    for raw in bytecode_text.splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue

        if line.startswith("PUSHSTR "):
            prog.append(["PUSHSTR", line[len("PUSHSTR "):]])
            continue

        # generic split is fine for everything else
        prog.append(line.split())


    # Label map
    labels: dict[str, int] = {}
    for i, inst in enumerate(prog):
        if inst[0] == "LABEL":
            labels[inst[1]] = i

    # Function table (DEFUN name p1 p2 ...). Entry points are the instruction after DEFUN.
    fun_entry: dict[str, int] = {}
    fun_params: dict[str, list[str]] = {}
    for i, inst in enumerate(prog):
        if inst[0] == "DEFUN":
            name = inst[1]
            params = inst[2:]
            fun_entry[name] = i + 1
            fun_params[name] = params

    # Runtime state
    stack: list[Any] = []
    globals_env: dict[str, Any] = {}
    frames: list[dict[str, Any]] = [globals_env]  # frames[-1] is current
    callstack: list[int] = []
    ip = 0

    # Output capture so compiler can "emit" into a file easily
    out_lines: list[str] = []

    # ---- Primitives (CALLPRIM name argc) ----
    gensym_id = 0

    def prim_read_all() -> str:
        return stdin_text

    def prim_parse_sexprs(s: str):
        return parse_sexprs(s)

    def prim_emit(line: str) -> int:
        out_lines.append(line)
        return 0

    def prim_gensym(prefix: str) -> str:
        nonlocal gensym_id
        gensym_id += 1
        return f"{prefix}{gensym_id}"

    def prim_str_cat(a: str, b: str) -> str:
        return a + b

    def prim_to_str(x: Any) -> str:
        return str(x)

    def prim_sym(s: str) -> Sym:
        return Sym(s)

    def prim_sym_name(x: Sym) -> str:
        return x.name

    def prim_sym_eq(a: Sym, b: Sym) -> int:
        return 1 if isinstance(a, Sym) and isinstance(b, Sym) and a.name == b.name else 0

    def prim_intp(x: Any) -> int:
        return 1 if isinstance(x, int) else 0

    def prim_symp(x: Any) -> int:
        return 1 if isinstance(x, Sym) else 0

    def prim_pairp(x: Any) -> int:
        return 1 if isinstance(x, list) and len(x) > 0 else 0

    def prim_nullp(x: Any) -> int:
        return 1 if x == [] else 0

    def prim_strp(x: Any) -> int:
        return 1 if isinstance(x, str) else 0

    def prim_json_dumps(s: str) -> str:
        return json.dumps(s)

    def prim_car(x: list[Any]) -> Any:
        return x[0]

    def prim_cdr(x: list[Any]) -> list[Any]:
        return x[1:]

    def prim_error(msg: str) -> int:
        raise RuntimeError(msg)

    PRIMS: dict[str, Callable[..., Any]] = {
        "read-all": lambda: prim_read_all(),
        "parse-sexprs": prim_parse_sexprs,
        "emit": prim_emit,
        "gensym": prim_gensym,
        "str-cat": prim_str_cat,
        "to-str": prim_to_str,
        "sym": prim_sym,
        "sym-name": prim_sym_name,
        "sym-eq?": prim_sym_eq,
        "int?": prim_intp,
        "sym?": prim_symp,
        "pair?": prim_pairp,
        "null?": prim_nullp,
        "str?": prim_strp,
        "json-dumps": prim_json_dumps,
        "car": prim_car,
        "cdr": prim_cdr,
        "error": prim_error,
    }

    def load_var(name: str) -> Any:
        # current frame, then globals
        if name in frames[-1]:
            return frames[-1][name]
        if name in globals_env:
            return globals_env[name]
        return 0

    def store_var(name: str, val: Any) -> None:
        frames[-1][name] = val

    # Execution loop
    while ip < len(prog):
        inst = prog[ip]
        op = inst[0]

        if op == "PUSH":
            stack.append(int(inst[1]))

        elif op == "PUSHSTR":
            stack.append(json.loads(inst[1]))

        elif op == "LOAD":
            stack.append(load_var(inst[1]))

        elif op == "STORE":
            store_var(inst[1], stack.pop())

        elif op == "ADD":
            b, a = stack.pop(), stack.pop()
            stack.append(a + b)

        elif op == "SUB":
            b, a = stack.pop(), stack.pop()
            stack.append(a - b)

        elif op == "MUL":
            b, a = stack.pop(), stack.pop()
            stack.append(a * b)

        elif op == "DIV":
            b, a = stack.pop(), stack.pop()
            stack.append(a // b)

        elif op == "LT":
            b, a = stack.pop(), stack.pop()
            stack.append(1 if a < b else 0)

        elif op == "EQ":
            b, a = stack.pop(), stack.pop()
            stack.append(1 if a == b else 0)

        elif op == "PRINT":
            # print to VM stdout (not compiler emit channel)
            val = stack.pop()
            print(val)

        elif op == "LABEL":
            pass

        elif op == "JMP":
            ip = labels[inst[1]]
            continue

        elif op == "JZ":
            cond = stack.pop()
            if cond == 0:
                ip = labels[inst[1]]
                continue

        elif op == "DEFUN":
            # DEFUN name p1 p2 ... ; record entry, params; execution continues but compiler will JMP over bodies
            pass
            # name = inst[1]
            # params = inst[2:]
            # fun_entry[name] = ip + 1
            # fun_params[name] = params

        elif op == "CALL":
            # CALL fname argc
            fname = inst[1]
            argc = int(inst[2])
            if fname not in fun_entry:
                raise RuntimeError(f"CALL unknown function: {fname}")
            params = fun_params.get(fname, [])
            if len(params) != argc:
                raise RuntimeError(f"CALL arity mismatch for {fname}: expected {len(params)} got {argc}")

            # bind args into new frame
            new_frame: dict[str, Any] = {}
            # args are pushed left-to-right; pop in reverse
            args = [stack.pop() for _ in range(argc)][::-1]
            for p, v in zip(params, args):
                new_frame[p] = v

            callstack.append(ip + 1)
            frames.append(new_frame)
            ip = fun_entry[fname]
            continue

        elif op == "RET":
            # return value is on stack (or push 0 before RET)
            if len(frames) == 1:
                # returning from top-level: end program
                break
            frames.pop()
            ip = callstack.pop()
            continue

        elif op == "CALLPRIM":
            # CALLPRIM name argc
            pname = inst[1]
            argc = int(inst[2])
            if pname not in PRIMS:
                raise RuntimeError(f"Unknown primitive: {pname}")
            args = [stack.pop() for _ in range(argc)][::-1]
            res = PRIMS[pname](*args)
            stack.append(res)

        else:
            raise RuntimeError(f"Unknown instruction: {inst}")

        ip += 1

    return "\n".join(out_lines)

def main():
    # Usage:
    #   python vm.py program.bc                 (bytecode from file, stdin empty)
    #   python vm.py program.bc < input.tl      (bytecode from file, TL source provided via stdin_text)
    #   python vm.py < program.bc               (bytecode from stdin)
    argv = sys.argv[1:]
    if argv:
        bytecode_text = open(argv[0], "r", encoding="utf-8").read()
    else:
        bytecode_text = sys.stdin.read()

    # The TL program being compiled is fed via stdin redirection
    stdin_text = ""
    if not sys.stdin.isatty():
        # If bytecode came from file, stdin is free to be input
        if argv:
            stdin_text = sys.stdin.read()

    emitted = run(bytecode_text, stdin_text)
    # The compiler uses emit(...) to output bytecode
    if emitted:
        sys.stdout.write(emitted)
        if not emitted.endswith("\n"):
            sys.stdout.write("\n")

if __name__ == "__main__":
    main()
