#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below


(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs i)
  (cond [(< i 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (list-ref xs (remainder i (length xs)))]))


(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([st (s)])
        (cons (car st) (stream-for-n-steps (cdr st) (- n 1))))))

(define funny-number-stream
  (letrec
      ([f (lambda (x)
            (cons (if (= (remainder x 5) 0) (- x) x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define cat-then-dog
  (letrec
      ([f (lambda (x)
            (cons (if (= (remainder x 2) 0) "cat.jpg" "dog.jpg") (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

(define (stream-add-zero s)
  (letrec
      ([f (lambda (s2)
            (let ([st (s2)])
              (cons (cons 0 (car st)) (lambda () (f (cdr st))))))])
    (lambda () (f s))))


(define (cycle-lists xs ys)
  (letrec
      ([f (lambda (x)
            (cons [cons (list-nth-mod xs x) (list-nth-mod ys x)] (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec
      ([len (vector-length vec)]
       [helper (lambda (i)
         (if (= i len) #f
             (let
                 ([i-th (vector-ref vec i)])
               (if (and (pair? i-th)
                      (equal? [car i-th] v))
                 i-th
                 (helper  (+ i 1))))))]        )
    (helper 0)))

(define (cached-assoc xs n)
  (letrec
      (
       [cache (make-vector n #f)]
       [i 0]
       [add-to-cache (lambda (v)
                       (vector-set! cache i v)
                       (set! i (remainder (+ i 1) n))
                       v)]
       [compute-and-cache (lambda (v)
                          (let ([result (assoc v xs)])
                            (add-to-cache (cons v result))
                            result))]

      )
  (lambda (v)
    (letrec (
             [found (vector-assoc v cache)]
         )
      (if found (cdr found)
          (compute-and-cache v))
        ))))


;(define digits '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten") )

;(define seq (sequence 1 100000 1))

;(define my-st (cycle-lists seq digits))

;(define the-long-list (stream-for-n-steps my-st 100000))
