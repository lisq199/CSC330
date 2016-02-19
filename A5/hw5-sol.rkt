;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list

(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem A
(define (mupllist->racketlist lst)
  (cond
    [(aunit? lst) null]
    [(var? lst) lst]
    [(int? lst) lst]
    [(apair? lst) (cons (mupllist->racketlist (apair-e1 lst))
                        (mupllist->racketlist (apair-e2 lst)))]
    [#t (error "invalid object in list")]
    ))

(define (racketlist->mupllist lst)
  (if (null? lst)
      (aunit)
      (let* ([hd (car lst)]
             [rhd (cond
                    [(list? hd) (racketlist->mupllist hd)]
                    [(int? hd) hd]
                    [(var? hd) hd]
                    [(closure? hd) hd]
                    [#t] (error "invalid object in list"))
                  ])
        (apair rhd (racketlist->mupllist (cdr lst)))
        )))


;; Problem B

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; "CHANGE" add more cases here
        [(closure? e) e]
        [(aunit? e) e]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env)
                           (eval-under-env (apair-e2 e) env))]
        [(int? e)
         (if (number? (int-num e))
             e
             (error (format "MUPL int is not a number ~v" e))
             )
         ]
        [(mlet? e) (if (string? (mlet-var e))
                    (let* (
                       [val (eval-under-env (mlet-e e) env)]
                       [newenv (cons (cons (mlet-var e) val) env)]
                       )
                     (eval-under-env (mlet-body e) newenv)
                     )
                    (error (format "not a variable name in mlet: ~v" e))
                    )
         ]
        [(ifgreater? e) (let (
                              [left (eval-under-env (ifgreater-e1 e) env)]
                              [right (eval-under-env (ifgreater-e2 e) env)]
                              )
                          (if (> (int-num left) (int-num right))
                              (eval-under-env (ifgreater-e3 e) env)
                              (eval-under-env (ifgreater-e4 e) env)
                              )
                          )
         ]
        [(fun? e) (closure env e) ; create a closure with function itself
         ]
        [(call? e) (let* (
                          [f-closure (eval-under-env (call-funexp e) env)] ; compute get closure to call
                          [parm-value (eval-under-env (call-actual e) env)] ; compute value of parameter
                          )
                     (if (closure? f-closure)
                         (let* (
                                [parm-name (fun-formal (closure-fun f-closure))] ; get name of parameter
                                [parm-pair-env (cons parm-name parm-value)]
                                [func-env (closure-env f-closure)] ; find function environment
                                [func-name (fun-nameopt (closure-fun f-closure))];; get name of function
                                [calling-env (if func-name
                                                 (cons parm-pair-env (cons (cons func-name f-closure) func-env))
                                                 (cons parm-pair-env func-env)
                                                 )]
                                [fun (fun-body (closure-fun f-closure))] ;; body of the function to evaluate
                                )
                           (eval-under-env fun calling-env)
                           )
                         (error (format "not a closure in first parm to call: ~v" e))
                         )
                     )
         ]
        [(fst? e) (let
                      ([pr (eval-under-env (fst-e e) env)])
                    (if (apair? pr)
                        (apair-e1 pr)
                        (error (format "not a pair in fst: ~v" e pr))
                        ))]
        [(snd? e) (let
                      ([pr (eval-under-env (snd-e e) env)])
                    (if (apair? pr)
                        (apair-e2 pr)
                        (error (format "not a pair in snd: ~v" e))
                        ))]
        [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))
         ]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem C

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))
      )
  )

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x"  e1)
               (cons "_y"  e2))
         (ifgreater (var "_x") (var "_y") e4
                    (ifgreater (var "_y") (var "_x") e4 e3)
                    )
         )
  )

;; Problem D

(define mupl-map
  (fun "mupl-map" "f"
       (fun #f "list"
            (ifaunit (var "list")
                     (aunit)
                     (apair (call (var "f") (fst (var "list")))
                            (call (call (var "mupl-map") (var "f")) (snd (var "list")))
                            )
                     )
            )
       )
  )

(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "x" (add (var "x") (var "i"))))
             )
        )
  )
