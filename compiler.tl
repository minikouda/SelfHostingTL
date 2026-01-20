; compiler.tl — self-hosted compiler (can compile itself)

(define (cadr xs) (car (cdr xs)))
(define (caddr xs) (car (cdr (cdr xs))))

(define (is-sym x name)
  (if (sym? x)
      (sym-eq? x (sym name))
      0))

(define (is-prim-name nm)
  ; nm is a string
  (if (== nm "read-all") 1
  (if (== nm "parse-sexprs") 1
  (if (== nm "emit") 1
  (if (== nm "gensym") 1
  (if (== nm "str-cat") 1
  (if (== nm "to-str") 1
  (if (== nm "json-dumps") 1
  (if (== nm "sym") 1
  (if (== nm "sym-name") 1
  (if (== nm "sym-eq?") 1
  (if (== nm "int?") 1
  (if (== nm "str?") 1
  (if (== nm "sym?") 1
  (if (== nm "pair?") 1
  (if (== nm "null?") 1
  (if (== nm "car") 1
  (if (== nm "cdr") 1
  (if (== nm "error") 1
      0)))))))))))))))))))

(define (emit-line s) (begin (emit s) 0))

(define (emit-push-int n) (emit-line (str-cat "PUSH " (to-str n))))
(define (emit-push-str s) (emit-line (str-cat "PUSHSTR " (json-dumps s))))
(define (emit-load nm)    (emit-line (str-cat "LOAD " nm)))
(define (emit-store nm)   (emit-line (str-cat "STORE " nm)))

(define (emit-label lab)  (emit-line (str-cat "LABEL " lab)))
(define (emit-jmp lab)    (emit-line (str-cat "JMP " lab)))
(define (emit-jz lab)     (emit-line (str-cat "JZ " lab)))

(define (emit-defun name params)
  ; params is a list of strings
  (emit-line (str-cat "DEFUN " (str-cat name (emit-params params)))))

(define (emit-params ps)
  (if (null? ps)
      ""
      (str-cat " " (str-cat (car ps) (emit-params (cdr ps))))))

(define (compile)
  (begin
    (let src (read-all))
    (let forms (parse-sexprs src))
    (compile-program forms)))

(define (compile-program forms)
  (begin
    ; jump over function bodies
    (emit-jmp "__START__")
    (compile-defines forms)
    (emit-label "__START__")
    (compile-non-defines forms)
    (emit-push-int 0)
    (emit-line "RET")
    0))

(define (compile-defines forms)
  (if (null? forms)
      0
      (begin
        (let f (car forms))
        (if (and (pair? f) (is-sym (car f) "define"))
            (compile-define f)
            0)
        (compile-defines (cdr forms)))))

(define (compile-non-defines forms)
  (if (null? forms)
      0
      (begin
        (let f (car forms))
        (if (and (pair? f) (is-sym (car f) "define"))
            0
            (compile-form f))
        (compile-non-defines (cdr forms)))))

(define (and a b)
  (if a (if b 1 0) 0))

(define (compile-define form)
  ; (define (fname p1 p2 ...) body)
  (begin
    (let sig (cadr form))
    (let body (caddr form))
    (if (pair? sig)
        (begin
          (let fname-sym (car sig))
          (let params-syms (cdr sig))
          (let fname (sym-name fname-sym))
          (let params (map-symnames params-syms))
          (emit-defun fname params)
          (compile-form body)
          (emit-line "RET")
          0)
        (error "define: only function form supported"))))

(define (map-symnames xs)
  (if (null? xs)
      ()
      (begin
        (let h (car xs))
        (let t (cdr xs))
        (cons (sym-name h) (map-symnames t)))))

; minimal list constructor helpers using builtin lists:
; We'll represent lists as normal TL lists from parse-sexprs.
; To build a new list, we use (cons x xs) implemented below.
(define (cons x xs)
  (callprim-cons x xs))

; cons primitive implemented via CALLPRIM (we'll compile it as a primitive call)
; We'll implement it by compiling (cons ...) as CALLPRIM too:
(define (callprim-cons a b) (error "cons should not run at runtime"))

(define (compile-form x)
  (if (int? x)
      (emit-push-int x)
      (if (str? x)
          (emit-push-str x)
          (if (sym? x)
              (emit-load (sym-name x))
              (if (pair? x)
                  (compile-list x)
                  (emit-push-int 0))))))

(define (compile-list lst)
  (begin
    (let op (car lst))
    (let args (cdr lst))

    (if (is-sym op "begin")
        (compile-seq args)

    (if (is-sym op "if")
        (compile-if args)

    (if (is-sym op "let")
        (compile-letset "let" args)

    (if (is-sym op "set")
        (compile-letset "set" args)

    (if (is-sym op "while")
        (compile-while args)

    (if (is-sym op "print")
        (begin
          (compile-form (car args))
          (emit-line "PRINT")
          (emit-push-int 0))

    (if (is-sym op "+")
        (begin (compile-bin args) (emit-line "ADD"))

    (if (is-sym op "-")
        (begin (compile-bin args) (emit-line "SUB"))

    (if (is-sym op "*")
        (begin (compile-bin args) (emit-line "MUL"))

    (if (is-sym op "/")
        (begin (compile-bin args) (emit-line "DIV"))

    (if (is-sym op "<")
        (begin (compile-bin args) (emit-line "LT"))

    (if (is-sym op "==")
        (begin (compile-bin args) (emit-line "EQ"))

        (compile-call op args)))))))))))))))

(define (compile-seq xs)
  (if (null? xs)
      0
      (begin (compile-form (car xs)) (compile-seq (cdr xs)))))

(define (compile-bin args)
  (begin
    (compile-form (car args))
    (compile-form (cadr args))
    0))

(define (compile-if args)
  (begin
    (let cond (car args))
    (let thn (cadr args))
    (let els (caddr args))
    (let l_else (gensym "ELSE"))
    (let l_end (gensym "END"))
    (compile-form cond)
    (emit-jz l_else)
    (compile-form thn)
    (emit-jmp l_end)
    (emit-label l_else)
    (compile-form els)
    (emit-label l_end)
    0))

(define (compile-letset kw args)
  (begin
    (let name (car args))
    (let expr (cadr args))
    (compile-form expr)
    (emit-store (sym-name name))
    0))

(define (compile-while args)
  (begin
    (let cond (car args))
    (let body (cdr args))
    (let top (gensym "TOP"))
    (let end (gensym "END"))
    (emit-label top)
    (compile-form cond)
    (emit-jz end)
    (compile-seq body)
    (emit-jmp top)
    (emit-label end)
    (emit-push-int 0)
    0))

(define (compile-call op args)
  (begin
    (compile-args args)
    (let nm (sym-name op))
    (if (is-prim-name nm)
        (emit-line (str-cat "CALLPRIM " (str-cat nm (str-cat " " (to-str (len args))))))
        (emit-line (str-cat "CALL " (str-cat nm (str-cat " " (to-str (len args)))))))
    0))

(define (compile-args xs)
  (if (null? xs)
      0
      (begin (compile-form (car xs)) (compile-args (cdr xs)))))

(define (len xs)
  (if (null? xs) 0 (+ 1 (len (cdr xs)))))

; ---- IMPORTANT: compile-time-only 'cons' support ----
; We don't want (cons ...) at runtime. So we compile it as a primitive.
; Add this rule inside compile-list via compile-call path: when op is 'cons',
; it'll be treated as primitive if listed in is-prim-name. We'll add cons now.
; (Update is-prim-name list above if you want cons; easiest is to avoid cons
; entirely. For now, we used cons only in map-symnames, so instead we will
; REMOVE map-symnames usage by compiling DEFUN params without building a list.)

; To avoid needing cons at runtime, we will NOT use map-symnames at runtime.
; We already built params list from parse-sexprs; but in TL itself we can't
; build new lists without cons. So simplest: emit DEFUN params by walking syms
; directly.

; Replace map-symnames usage by direct walk:
(define (compile-define form)
  (begin
    (let sig (cadr form))
    (let body (caddr form))
    (if (pair? sig)
        (begin
          (let fname (sym-name (car sig)))
          (emit-line (str-cat "DEFUN " (str-cat fname (emit-symparams (cdr sig)))))
          (compile-form body)
          (emit-line "RET")
          0)
        (error "define: only function form supported"))))

(define (emit-symparams syms)
  (if (null? syms)
      ""
      (str-cat " " (str-cat (sym-name (car syms)) (emit-symparams (cdr syms))))))

(compile)