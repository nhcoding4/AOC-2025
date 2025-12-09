#lang racket

;; --------------------------------------------------------------------------------------------------

(define (create-dataset raw-data)
  (define split-commas (string-split raw-data ","))

  (define split-dashes
    (for/list ([raw (in-list split-commas)])
      (string-split raw "-")))

  (define removed-newlines
    (for/list ([start-stop (in-list split-dashes)])
      (flatten (for/list ([num (in-list start-stop)])
                 (cond
                   [(string-contains? num "\r\n") (string-split "\r\n")]
                   [else (string-split num "\n")])))))

  (define ranges
    (for/list ([start-stop (in-list removed-newlines)])
      (define start-num (string->number (first start-stop)))
      (define stop-num (add1 (string->number (last start-stop))))
      (range start-num stop-num)))

  (for/list ([num-range (in-list ranges)])
    (for/list ([number (in-list num-range)])
      (format "~a" number))))

;; --------------------------------------------------------------------------------------------------

(define (part-1 dataset)
  (define (invalid? data)
    (define data-length (string-length data))
    (cond
      [(odd? data-length) #f]
      [else
       (define half-point (/ data-length 2))
       (define part-1 (substring data 0 half-point))
       (define part-2 (substring data half-point))
       (cond
         [(equal? part-1 part-2) #t]
         [else #f])]))

  (define invalid-sequences
    (flatten (for/fold ([result (list)]) ([data-range (in-list dataset)])
               (define invalid-seqs
                 (for/fold ([invalid (list)]) ([num (in-list data-range)])
                   (cond
                     [(invalid? num) (cons num invalid)]
                     [else invalid])))
               (cond
                 [(empty? invalid-seqs) result]
                 [else (cons invalid-seqs result)]))))

  (for/fold ([result 0]) ([num (in-list invalid-sequences)])
    (+ result (string->number num))))

;; --------------------------------------------------------------------------------------------------

(define (part-2 dataset)
  (define (get-chunk-sizes seq-length)
    (define range-limit (add1 (floor (/ seq-length 2))))
    (for/fold ([result (list)]) ([i (in-range 1 range-limit)])
      (cond
        [(= 0 (modulo seq-length i)) (cons i result)]
        [else result])))

  (define (invalid? data data-length slice-size)
    (define slice (substring data 0 slice-size))

    (define (iter idx)
      (define substring-end (+ idx slice-size))
      (cond
        [(> substring-end data-length) #t]
        [else
         (define chunk (substring data idx substring-end))
         (cond
           [(equal? slice chunk) (iter (+ idx slice-size))]
           [else #f])]))

    (iter 0))

  (define (test-sequences seqs)
    (define (test seq factors)
      (cond
        [(empty? factors) #f]
        [else
         (define seq-length (string-length seq))
         (define factor (first factors))
         (cond
           [(= factor seq-length) (test seq (rest factors))]
           [(invalid? seq seq-length factor) #t]
           [else (test seq (rest factors))])]))

    (for/fold ([result (list)]) ([seq (in-list seqs)])
      (define chunk-sizes (get-chunk-sizes (string-length seq)))
      (cond
        [(equal? #t (test seq chunk-sizes)) (cons seq result)]
        [else result])))

  (define invalid-sequences
    (flatten (for/fold ([result (list)]) ([seqs (in-list dataset)])
               (define invalid-seqs (test-sequences seqs))
               (cond
                 [(empty? invalid-seqs) result]
                 [else (cons invalid-seqs result)]))))

  (for/fold ([result 0]) ([invalid-seq (in-list invalid-sequences)])
    (+ result (string->number invalid-seq))))

;; --------------------------------------------------------------------------------------------------

(define data (file->string "./day2data.txt"))
(define dataset (create-dataset data))

(define part-1-result (part-1 dataset))
(define part-2-result (part-2 dataset))

(displayln "AOC 2025 Day 2\n--------------")
(displayln (format "Part 1: ~a" part-1-result))
(displayln (format "Part 2: ~a" part-2-result))

;; --------------------------------------------------------------------------------------------------
