
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high) '() (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

(define (list-nth-mod xs n)
  (if (< n 0) (error "list-nth-mod: negative number")
      (if (null? xs) (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

(define (stream-for-n-steps s n)
  (if (<= n 0) '()
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (if (= 0 (remainder x 5)) (- 0 x) x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define cat-then-dog
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (if (equal? x "cat.jpg") "dog.jpg" "cat.jpg")))))])
    (lambda () (f "cat.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda (x)
                (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x)
                (cons (cons (list-nth-mod xs x) (list-nth-mod ys x))
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [assoc-helper (lambda (i)
                           (if (>= i len) #f
                               (letrec ([current (vector-ref vec i)]
                                        [not-pair (not (pair? current))])
                                 (if not-pair (assoc-helper (+ i 1))
                                     (if (equal? (car current) v) current
                                         (assoc-helper (+ i 1)))))))])
    (assoc-helper 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [cache-len 0]
           [incr-cache-len (lambda ()
                             (set! cache-len (remainder (+ cache-len 1) n)))])
    (lambda (v)
      (letrec ([result-from-cache (vector-assoc v cache)])
        (if result-from-cache result-from-cache
            (letrec ([result-from-xs (assoc v xs)])
              (if result-from-xs
                  (begin
                    (vector-set! cache cache-len result-from-xs)
                    (incr-cache-len)
                    result-from-xs)
                  #f)))))))
