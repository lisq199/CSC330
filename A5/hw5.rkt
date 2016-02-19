;; Programming Languages, Homework 5 version 1.1
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

;; CHANGE (put your solutions here)
(define (mupllist->racketlist lst)
  (if (aunit? lst) null
      (cons (let* ([head (apair-e1 lst)])
              (if (apair? head) (mupllist->racketlist head) head))
            (mupllist->racketlist (apair-e2 lst)))))

(define (racketlist->mupllist lst)
  (if (null? lst) (aunit)
      (apair (let* ([head (car lst)])
               (if (list? head) (racketlist->mupllist head)
                   head))
             (racketlist->mupllist (cdr lst)))))

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
        [(int? e) e]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(fun? e) (closure env e)]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([cenv (closure-env v1)]
                      [cfun (closure-fun v1)]
                      [cfunbody (fun-body cfun)]
                      [envn (cons (fun-nameopt cfun) v1)]
                      [envf (cons (fun-formal cfun) v2)]
                      [cname (car envn)]
                      [newenv (cons envf
                                    (if cname (cons envn cenv) cenv))])
                 (eval-under-env cfunbody newenv))
               (error "MUPL call applied to non-closure")))]
        [(mlet? e)
         (let* ([me (mlet-e e)]
                [mvar (mlet-var e)]
                [mbody (mlet-body e)]
                [v (eval-under-env me env)]
                [newenv (cons (cons mvar v) env)])
           (eval-under-env mbody newenv))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v) (apair-e1 v)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v) (apair-e2 v)
               (error "MUPL snd applied to non-apair")))]
        [(aunit? e) e]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1) (int 0))]
        [(closure? e) e]
        ;; "CHANGE" add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem C

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst) e2
      (let ([head (car lstlst)]
            [tail (cdr lstlst)])
        (mlet
         (car head) (cdr head)
         (mlet* tail e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet*
   (list (cons "_x" e1) (cons "_y" e2))
   (let* ([greater (ifgreater (var "_x") (var "_y") (int 1) (int 0))]
          [smaller (ifgreater (var "_y") (var "_x") (int 1) (int 0))]
          [equal (add greater smaller)])
     (ifgreater equal (int 0) e4 e3))))

;; Problem D

(define mupl-map
  (fun #f "fn"
       (fun "map" "xs"
            (ifaunit (var "xs") (aunit)
                     (apair
                      (call (var "fn") (fst (var "xs")))
                      (call (var "map") (snd (var "xs"))))))))

(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map")
                   (fun #f "x"
                        (add (var "x") (var "i")))))))
