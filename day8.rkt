#lang racket

;; -------------------------------------------------------------------------------------------------
;; Point Creation and Operations

(define (create-points raw-data)
  (define split-carriage (filter (lambda (x) (not (equal? "\r" x))) (string-split raw-data "\r")))
  (define lines
    (filter (lambda (x) (not (equal? "\n" x))) (string-split (string-join split-carriage "") "\n")))

  (for/vector ([line (in-list lines)])
    (define parts (string-split line ","))
    (list (string->number (first parts))
          (string->number (second parts))
          (string->number (last parts)))))

(define (distance a b)
  (define x (expt (- (first a) (first b)) 2))
  (define y (expt (- (second a) (second b)) 2))
  (define z (expt (- (last a) (last b)) 2))
  (sqrt (+ x y z)))

(define (create-distances data)
  (define unsorted
    (for/fold ([result (list)]) ([i (in-range (sub1 (vector-length data)))])
      (define distances
        (for/fold ([sub-result (list)]) ([j (in-range (add1 i) (vector-length data))])
          (define a (vector-ref data i))
          (define b (vector-ref data j))
          (cons (list (distance a b) i j) sub-result)))
      (for/fold ([new-result result]) ([dist (in-list distances)])
        (cons dist new-result))))

  (sort unsorted < #:key first))

;; -------------------------------------------------------------------------------------------------
;; Union-Sets

(define (create-nodes total)
  (define nodes (make-hash))
  (for ([i (in-range total)])
    (hash-set! nodes i i))
  nodes)

(define (find-set nodes value)
  (cond
    [(equal? value (hash-ref nodes value)) value]
    [else
     (define new-value (find-set nodes (hash-ref nodes value)))
     (hash-set! nodes value new-value)
     new-value]))

(define (create-union-set nodes a b)
  (define a1 (find-set nodes a))
  (define b1 (find-set nodes b))
  (when (not (equal? a1 b1))
    (hash-set! nodes b1 a1)))

;; -------------------------------------------------------------------------------------------------
;; P1 - Connect X (1000) Wires and sum the 3 largest circuits that form.

(define (part-1 distances wire-count)
  (define (create-circuits nodes data dataset-length)
    (for ([point (in-list data)])
      (create-union-set nodes (second point) (last point)))

    (for/fold ([result (hash)]) ([i (in-range dataset-length)])
      (define value (find-set nodes i))
      (cond
        [(hash-has-key? result value) (hash-update result value add1)]
        [else (hash-set result value 1)])))

  (define (collect-results circuits)
    (define to-multiply 3)
    (define results (hash-values circuits))
    (apply * (take (sort results >) to-multiply)))

  (define total-distances (length distances))
  (define nodes (create-nodes total-distances))
  (define circuits (create-circuits nodes (take distances wire-count) total-distances))
  (collect-results circuits))

;; -------------------------------------------------------------------------------------------------
;; P2 - Multiply the X values of the 2 points that Links every other circuit into a single circuit.

(define (part-2 points distances)
  (define nodes (create-nodes (length distances)))
  (define last-connection (sub1 (vector-length points)))

  (for/fold ([connections 0]
             [result 0]
             #:result result)
            ([line (in-list distances)]
             #:when (equal? result 0))

    (define a (find-set nodes (second line)))
    (define b (find-set nodes (last line)))

    (cond
      [(equal? a b) (values connections result)]
      [else
       (create-union-set nodes a b)
       (define new-connections (add1 connections))

       (cond
         [(equal? new-connections last-connection)
          (define point-a (vector-ref points (second line)))
          (define point-b (vector-ref points (last line)))
          (define new-result (* (first point-a) (first point-b)))
          (values new-connections new-result)]

         [else (values new-connections result)])])))

;; -------------------------------------------------------------------------------------------------

(define test-data (file->string "./testdata.txt"))
(define test-points (create-points test-data))
(define test-distances (create-distances test-points))
(define max-wires-test 10)
(define test-part-1 (part-1 test-distances max-wires-test))
(define test-part-2 (part-2 test-points test-distances))

(define data (file->string "./day8data.txt"))
(define points (create-points data))
(define distances (create-distances points))
(define max-wires-p1 1000)
(define part-1-answer (part-1 distances max-wires-p1))
(define part-2-answer (part-2 points distances))

(displayln "AOC 2025 Day 8\n--------------")
(displayln (format "Part 1: ~a" part-1-answer))
(displayln (format "Part 2: ~a" part-2-answer))
(displayln "--------------")
(displayln (format "Test Part 1: ~a" test-part-1))
(displayln (format "Test Part 2: ~a" test-part-2))

;; -------------------------------------------------------------------------------------------------
