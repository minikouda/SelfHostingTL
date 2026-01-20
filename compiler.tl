; Phase-1 compiler written in the bootstrap subset.
; It "compiles" a hardcoded TinyLisp program consisting of prints of integers.
; Output is bytecode lines produced via (print ...).
; Hardcoded "source program" (conceptually):
;   (print 1)
;   (print 2)
;   (print 3)
; Emit: PUSH 1; PRINT; PUSH 2; PRINT; PUSH 3; PRINT
; Since we don't have string output in the subset, we emit numeric opcodes:
;   1 n  means "PUSH n"
;   2    means "PRINT"
;
; Your VM (or a tiny post-processor) can later map:
;   1 n -> PUSH n
;   2   -> PRINT
(print 1) (print 1)
(print 2)
(print 1) (print 2)
(print 2)
(print 1) (print 3)
(print 2)