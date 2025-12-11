#lang racket

;; -------------------------------------------------------------------------------------------------

(define (part-1 raw-data)
  (define lines (string-split raw-data "\n"))

  (define cleaned-lines
    (for/list ([line (in-list lines)])
      (define split-line (string-split line " "))
      (filter (lambda (x) (not (equal? x ""))) split-line)))

  (define numbers
    (for/list ([line (in-list (take cleaned-lines (sub1 (length cleaned-lines))))])
      (map (lambda (x) (string->number x)) line)))

  (define operations
    (for/list ([op (in-list (last cleaned-lines))])
      (cond
        [(equal? "*" op) *]
        [else +])))

  (define data
    (for/list ([i (in-range (length (first numbers)))])
      (reverse (for/fold ([result (list)]) ([column (in-list numbers)])
                 (cons (first (drop column i)) result)))))

  (for/fold ([result 0])
            ([row (in-list data)]
             [op (in-list operations)])
    (+ result (apply op row))))

;; -------------------------------------------------------------------------------------------------

(define (part-2 raw-data)
  (define (longest-seq data)
    (define numbers (string-split data " "))
    (for/fold ([result 0]) ([num (in-list numbers)])
      (define num-length (string-length num))
      (cond
        [(> num-length result) num-length]
        [else result])))

  (define (build-row data row-len result)
    (cond
      [(empty? data) (flatten result)]
      [else
       (define new-res (append result (take data row-len)))
       (build-row (drop data row-len) row-len new-res)]))

  (define (build-grouped-expressions data cur result)
    (cond
      [(empty? data) (reverse (cons cur result))]
      [(empty? (first data))
       (build-grouped-expressions (rest data) (list) (cons (reverse cur) result))]
      [else
       (build-grouped-expressions (rest data)
                                  (cons (string->number (list->string (first data))) cur)
                                  result)]))

  (define split-newlines (string-split raw-data "\n"))

  (define raw-numbers (take split-newlines (sub1 (length split-newlines))))

  (define operation-chars
    (filter (lambda (x) (not (char-whitespace? x))) (string->list (last split-newlines))))

  (define operations
    (for/list ([op (in-list operation-chars)])
      (cond
        [(equal? #\* op) *]
        [else +])))

  (define raw-number-lines
    (for/list ([row (in-list raw-numbers)])
      (build-row (string->list row) (longest-seq row) (list))))

  (define ungrouped-numbers
    (for/list ([i (in-range (length (first raw-number-lines)))])
      (reverse (for/fold ([number (list)]) ([row (in-list raw-number-lines)])
                 (define maybe-num (first (drop row i)))
                 (cond
                   [(char-whitespace? maybe-num) number]
                   [else (cons maybe-num number)])))))

  (define number-lists (build-grouped-expressions ungrouped-numbers (list) (list)))

  (for/fold ([result 0])
            ([op (in-list operations)]
             [numbers (in-list number-lists)])
    (+ result (apply op numbers))))

;; -------------------------------------------------------------------------------------------------

(define test-data (file->string "./testdata.txt"))
(define test-p1-answer (part-1 test-data))
(define test-p2-answer (part-2 test-data))

(define data (file->string "./day6data.txt"))
(define p1-answer (part-1 data))
(define p2-answer (part-2 data))

(displayln "AOC 2025 Day 6\n--------------")
(displayln (format "Part 1: ~a" p1-answer))
(displayln (format "Part 2: ~a" p2-answer))
(displayln "--------------------")
(displayln (format "Test p1 answer: ~a" test-p1-answer))
(displayln (format "Test p2 answer: ~a" test-p2-answer))

;; -------------------------------------------------------------------------------------------------
