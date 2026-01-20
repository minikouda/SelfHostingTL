# selfHosting (TinyLisp)

A tiny, self-hosting compiler experiment.

- **Source language:** TinyLisp (S-expressions)
- **Target language:** a small stack-based bytecode
- **Runtime:** a Python bytecode VM

## Files

- `compiler.tl` — the self-hosted TinyLisp→bytecode compiler (written in TinyLisp)
- `tinylisp_v0.py` — bootstrap compiler written in Python (v0)
- `vm.py` — bytecode virtual machine + primitives used by the compiler
- `test.tl` / `test.bc` — small example program and its compiled bytecode

## Quickstart

### 1) Bootstrap: compile the self-hosted compiler

This uses the Python v0 compiler to compile `compiler.tl` into bytecode.

```sh
python tinylisp_v0.py < compiler.tl > compiler.bc
```

### 2) Use the self-hosted compiler to compile a TinyLisp program

`vm.py` runs the compiler bytecode. The compiler reads the program source via the `read-all` primitive (VM stdin), and outputs bytecode via the `emit` primitive (VM stdout).

```sh
python vm.py compiler.bc < test.tl > test.bc
```

### 3) Run the compiled program

```sh
python vm.py test.bc
```

You should see the program’s `PRINT` output in the terminal.

## TinyLisp syntax (very small)

TinyLisp uses parenthesized lists (S-expressions):

```lisp
(let x 0)
(while (< x 3)
  (begin
    (print x)
    (set x (+ x 1))))
```

## Notes

- This repo is intentionally minimal: the “self-hosting” step is that `compiler.tl` can compile itself once you have an initial bootstrap compiler (`tinylisp_v0.py`).
- If you regenerate `compiler.bc`, it should be created by the command in the Bootstrap section above.
- To make sure it is truly self-hosting, you can run `vm.py compiler.bc < compiler.tl > compiler2.bc` and check that `compiler2.bc` is identical to `compiler.bc`.