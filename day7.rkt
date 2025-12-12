#lang racket

;; -------------------------------------------------------------------------------------------------

(define (create-dataset raw-data)
  (define lines
    (filter (lambda (x) (not (equal? "\n" x)))
            (string-split (string-join (string-split raw-data "\r") "") "\n")))

  (for/vector ([line (in-list lines)])
    (list->vector (string->list line))))

;; -------------------------------------------------------------------------------------------------

(define (get-start-position dataset)
  (define rows (vector-length dataset))
  (define (iter x y)
    (cond
      [(>= y rows) (list)]
      [(>= x (vector-length (vector-ref dataset y))) (iter 0 (add1 y))]
      [(equal? #\S (vector-ref (vector-ref dataset y) x)) (list x y)]
      [else (iter (add1 x) y)]))
  (iter 0 0))

;; -------------------------------------------------------------------------------------------------

(define (part-1 dataset)
  (define (traverse dataset)
    (define mark #\x)
    (define y-limit (sub1 (vector-length dataset)))
    (define data (vector-copy dataset))
    (define start-position (get-start-position data))

    (define (iter x y)
      (cond
        [(equal? y y-limit) 0]
        [(equal? mark (vector-ref (vector-ref data y) x)) 0]
        [(equal? #\^ (vector-ref (vector-ref data y) x))
         (+ 1 (iter (sub1 x) (add1 y)) (iter (add1 x) (add1 y)))]
        [else
         (vector-set! (vector-ref data y) x mark)
         (iter x (add1 y))]))

    (iter (first start-position) (second start-position)))

  (traverse dataset))

;; -------------------------------------------------------------------------------------------------

(define (part-2 dataset)
  (define (traverse dataset)
    (define y-limit (sub1 (vector-length dataset)))
    (define data (vector-copy dataset))
    (define start-position (get-start-position data))

    (define (iter x y)
      (cond
        [(equal? y y-limit) 1]
        [(number? (vector-ref (vector-ref data y) x)) (vector-ref (vector-ref data y) x)]
        [(equal? #\^ (vector-ref (vector-ref data y) x))
         (define paths (+ (iter (sub1 x) (add1 y)) (iter (add1 x) (add1 y))))
         (vector-set! (vector-ref data y) x paths)
         paths]
        [else (iter x (add1 y))]))

    (iter (first start-position) (second start-position)))

  (traverse dataset))

;; -------------------------------------------------------------------------------------------------

(define test-data (file->string "./testdata.txt"))
(define p1-test-answer (part-1 (create-dataset test-data)))
(define p2-test-answer (part-2 (create-dataset test-data)))

(define data (file->string "./day7data.txt"))
(define part-1-answer (part-1 (create-dataset data)))
(define part-2-answer (part-2 (create-dataset data)))

(displayln "AOC 2025 Day 7\n--------------")
(displayln (format "Part 1: ~a" part-1-answer))
(displayln (format "Part 2: ~a" part-2-answer))
(displayln "--------------")
(displayln (format "Test Part 1 Answer: ~a" p1-test-answer))
(displayln (format "Test Part 2 Answer: ~a" p2-test-answer))

;; -------------------------------------------------------------------------------------------------

