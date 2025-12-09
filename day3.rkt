#lang racket

(define-struct vec2 (x y))

;; --------------------------------------------------------------------------------------------------

(define (create-dataset raw-data)
  (for/list ([row (in-list (string-split raw-data "\n"))])
    (reverse (for/fold ([new-row (list)]) ([ch (in-list (string->list row))])
               (cons (- (char->integer ch) 48) new-row)))))

;; --------------------------------------------------------------------------------------------------

(define (part-1 dataset)
  (define (find-first-number numbers stop)
    (for/fold ([result (vec2 0 0)])
              ([num (in-list numbers)]
               [i (in-naturals)]
               #:when (< i stop))
      (cond
        [(> num (vec2-x result)) (vec2 num (add1 i))]
        [else result])))

  (define (find-second-number numbers start)
    (define remaining-data (drop numbers (vec2-y start)))
    (define data (find-first-number remaining-data (length remaining-data)))
    (vec2-x data))

  (for/fold ([result 0]) ([row (in-list dataset)])
    (define first-num-data (find-first-number row (sub1 (length row))))
    (define second-num (find-second-number row first-num-data))
    (+ result (string->number (format "~a~a" (vec2-x first-num-data) second-num)))))

;; --------------------------------------------------------------------------------------------------

(define (part-2 dataset)
  (define (find-highest seq)
    (for/fold ([result (vec2 0 0)])
              ([num (in-vector seq)]
               [i (in-naturals)])
      (cond
        [(> num (vec2-x result)) (vec2 num (add1 i))]
        [else result])))

  (define (largest-sequence dataset)
    (define (iter data to-pick result)
      (cond
        [(< to-pick 0) result]
        [else
         (define data-length (vector-length data))
         (define end-idx (- data-length to-pick))
         (define choices (vector-copy data 0 end-idx))
         (define choice (find-highest choices))
         (define next-data (vector-copy data (vec2-y choice) data-length))
         (iter next-data (sub1 to-pick) (cons (vec2-x choice) result))]))

    (reverse (iter (list->vector dataset) 11 (list))))

  (define (collect-data dataset)
    (define numbers
      (for/list ([seq (in-list dataset)])
        (define raw-seq (largest-sequence seq))
        (for/fold ([result ""]) ([num (in-list raw-seq)])
          (format "~a~a" result num))))
    (for/fold ([result 0]) ([num (in-list numbers)])
      (+ result (string->number num))))

  (collect-data dataset))

;; --------------------------------------------------------------------------------------------------

(define data (file->string "./day3data.txt"))
(define dataset (create-dataset data))

(define part-1-result (part-1 dataset))
(define part-2-result (part-2 dataset))

(displayln "AOC 2025 Day 3\n--------------")
(displayln (format "Part 1 result: ~a" part-1-result))
(displayln (format "Part 2 result: ~a" part-2-result))

;; --------------------------------------------------------------------------------------------------
