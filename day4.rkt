#lang racket

;; --------------------------------------------------------------------------------------------------

(define (create-dataset raw-data)
  (define rows (string-split raw-data "\n"))
  (define chars
    (for/list ([row (in-list rows)])
      (string->list row)))

  (for/vector ([row (in-list chars)])
    (for/vector ([char (in-list row)])
      (cond
        [(equal? #\. char) 0]
        [else 1]))))

;; --------------------------------------------------------------------------------------------------

(define (create-offsets rows columns)
  (define base #(#(-1 0) #(1 0) #(0 -1) #(0 1) #(-1 -1) #(-1 1) #(1 -1) #(1 1)))
  (for/vector ([y (in-range rows)])
    (for/vector ([x (in-range columns)])
      (for/fold ([offsets (vector)]) ([offset (in-vector base)])
        (define offset-x (+ x (vector-ref offset 0)))
        (define offset-y (+ y (vector-ref offset 1)))
        (cond
          [(or (< offset-x 0) (>= offset-x columns) (< offset-y 0) (>= offset-y rows)) offsets]
          [else
           (vector-extend offsets (add1 (vector-length offsets)) (vector offset-x offset-y))])))))

;; --------------------------------------------------------------------------------------------------

(define (part-1 dataset offsets)
  (define futures
    (for/list ([y (in-range (vector-length dataset))])
      (future
       (lambda ()
         (for/fold ([result 0]) ([x (in-range (vector-length (vector-ref dataset y)))])
           (cond
             [(= 0 (vector-ref (vector-ref dataset y) x)) result]
             [else
              (define check-neighbours
                (for/fold ([acc 0]) ([offset (in-vector (vector-ref (vector-ref offsets y) x))])
                  (+ acc
                     (vector-ref (vector-ref dataset (vector-ref offset 1)) (vector-ref offset 0)))))
              (cond
                [(< check-neighbours 4) (add1 result)]
                [else result])]))))))

  (define values
    (for/list ([f (in-list futures)])
      (touch f)))

  (for/fold ([result 0]) ([value (in-list values)])
    (+ result value)))

;; --------------------------------------------------------------------------------------------------

(define (part-2 grid offsets)
  (define (count-total-rolls grid)
    (define futures
      (for/list ([row (in-vector grid)])
        (future (lambda ()
                  (for/fold ([row-value 0]) ([num (in-vector row)])
                    (+ row-value num))))))
    (for/fold ([result 0]) ([f (in-list futures)])
      (+ result (touch f))))

  (define (remove-rolls grid previous-total)
    (define futures
      (for/list ([y (in-range (vector-length grid))])
        (future
         (lambda ()
           (for/vector ([x (in-range (vector-length (vector-ref grid y)))])
             (cond
               [(= 0 (vector-ref (vector-ref grid y) x)) 0]
               [else
                (define check-neighbours
                  (for/fold ([acc 0]) ([offset (in-vector (vector-ref (vector-ref offsets y) x))])
                    (+ acc
                       (vector-ref (vector-ref grid (vector-ref offset 1)) (vector-ref offset 0)))))
                (cond
                  [(< check-neighbours 4) 0]
                  [else 1])]))))))
    (define new-grid
      (for/vector ([f (in-list futures)])
        (touch f)))
    (define new-total (count-total-rolls new-grid))
    (cond
      [(= new-total previous-total) new-total]
      [else (remove-rolls new-grid new-total)]))

  (define starting-rolls (count-total-rolls grid))
  (define ending-rolls (remove-rolls grid (count-total-rolls grid)))
  (- starting-rolls ending-rolls))

;; --------------------------------------------------------------------------------------------------

(define test-data (file->string "./testdata.txt"))
(define test-dataset (create-dataset test-data))
(define test-offsets
  (create-offsets (vector-length test-dataset) (vector-length (vector-ref test-dataset 0))))

(define data (file->string "./day4data.txt"))
(define dataset (create-dataset data))
(define offsets (create-offsets (vector-length dataset) (vector-length (vector-ref dataset 0))))

(define part-1-test (part-1 test-dataset test-offsets))
(define part-2-test (part-2 test-dataset test-offsets))

(define part-1-answer (part-1 dataset offsets))
(define part-2-answer (part-2 dataset offsets))

(displayln "AOC 2025 Day 4\n--------------")
(displayln (format "Part 1: ~a" part-1-answer))
(displayln (format "Part 2: ~a" part-2-answer))
(displayln "----------------------")
(displayln (format "Part 1 Test: ~a" part-1-test))
(displayln (format "Part 2 Test: ~a" part-2-test))

;; --------------------------------------------------------------------------------------------------
