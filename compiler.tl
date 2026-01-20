; compiler.tl — TinyLisp -> bytecode compiler (balanced, no quote syntax)

(define (compile)
  (begin
    (let src (read-all))
    (let forms (parse-sexprs src))
    (compile-program forms)))

(define (compile-program forms)
  (if (null? forms)
      0
      (begin
        (compile-form (car forms))
        (compile-program (cdr forms)))))

(define (emit-push n)  (emit (str-cat "PUSH " (to-str n))))
(define (emit-load nm) (emit (str-cat "LOAD " nm)))
(define (emit-store nm) (emit (str-cat "STORE " nm)))

(define (emit-label lab) (emit (str-cat "LABEL " lab)))
(define (emit-jmp lab)   (emit (str-cat "JMP " lab)))
(define (emit-jz lab)    (emit (str-cat "JZ " lab)))

(define (cadr xs) (car (cdr xs)))

(define (isop op name)
  (sym-eq? op (sym name)))

(define (compile-form x)
  (if (int? x)
      (emit-push x)
      (if (sym? x)
          (emit-load (sym-name x))
          (compile-list x))))

(define (compile-seq xs)
  (if (null? xs)
      0
      (begin
        (compile-form (car xs))
        (compile-seq (cdr xs)))))

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
    (emit-label end)))

(define (compile-list lst)
  (begin
    (let op (car lst))
    (let args (cdr lst))

    (if (isop op "print")
        (begin
          (compile-form (car args))
          (emit "PRINT"))

        (if (isop op "+")
            (begin (compile-form (car args)) (compile-form (cadr args)) (emit "ADD"))

            (if (isop op "-")
                (begin (compile-form (car args)) (compile-form (cadr args)) (emit "SUB"))

                (if (isop op "*")
                    (begin (compile-form (car args)) (compile-form (cadr args)) (emit "MUL"))

                    (if (isop op "/")
                        (begin (compile-form (car args)) (compile-form (cadr args)) (emit "DIV"))

                        (if (isop op "<")
                            (begin (compile-form (car args)) (compile-form (cadr args)) (emit "LT"))

                            (if (isop op "==")
                                (begin (compile-form (car args)) (compile-form (cadr args)) (emit "EQ"))

                                (if (isop op "let")
                                    (begin
                                      (compile-form (cadr args))
                                      (emit-store (sym-name (car args))))

                                    (if (isop op "set")
                                        (begin
                                          (compile-form (cadr args))
                                          (emit-store (sym-name (car args))))

                                        (if (isop op "begin")
                                            (compile-seq args)

                                            (if (isop op "while")
                                                (compile-while args)
                                                (error "unknown op"))))))))))))))
(compile)