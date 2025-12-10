#lang racket

;; -------------------------------------------------------------------------------------------------

(define (create-ranges raw-input)
  (define first-split (string-split raw-input "\n\n"))
  (define ranges-raw (first first-split))
  (define limits-raw (string-split ranges-raw "\n"))
  (reverse (for/fold ([result (list)]) ([limits (in-list limits-raw)])
             (define parts (string-split limits "-"))
             (cons (list (string->number (first parts)) (string->number (second parts))) result))))

(define (create-test-numbers raw-input)
  (define first-split (string-split raw-input "\n\n"))
  (define test-numbers (second first-split))
  (for/list ([num (in-list (string-split test-numbers "\n"))])
    (string->number num)))

;; -------------------------------------------------------------------------------------------------

(define (part-1 valid-ranges test-numbers)
  (define (valid? test-num limits)
    (and (>= test-num (first limits)) (<= test-num (second limits))))

  (define futures
    (for/list ([num (in-list test-numbers)])
      (future (lambda ()
                (for/fold ([passing #f])
                          ([limits (in-list valid-ranges)]
                           #:when (equal? #f passing))
                  (cond
                    [(valid? num limits) #t]
                    [else #f]))))))

  (for/fold ([result 0]) ([f (in-list futures)])
    (cond
      [(equal? #t (touch f)) (add1 result)]
      [else result])))

;; -------------------------------------------------------------------------------------------------

(define (part-2 limits)
  (define (get-range ranges start stop)
    (cond
      [(empty? ranges) (values ranges (- (add1 stop) start))]
      [else
       (define cur (first ranges))
       (define new-start (first cur))
       (define new-stop (second cur))
       (cond
         [(> new-start stop) (values ranges (- (add1 stop) start))]
         [(> new-stop stop) (get-range (rest ranges) start new-stop)]
         [else (get-range (rest ranges) start stop)])]))

  (define (find-total-numbers ranges total)
    (cond
      [(empty? ranges) total]
      [else
       (let*-values ([(remaining result)
                      (get-range (rest ranges) (first (first ranges)) (second (first ranges)))])

         (find-total-numbers remaining (+ total result)))]))

  (define sorted-ranges (sort limits < #:key first))
  (find-total-numbers sorted-ranges 0))

;; -------------------------------------------------------------------------------------------------

(define test-input (file->string "./testdata.txt"))
(define test-valid-ranges (create-ranges test-input))
(define test-test-numbers (create-test-numbers test-input))

(define input (file->string "./day5data.txt"))
(define valid-ranges (create-ranges input))
(define test-numbers (create-test-numbers input))

(define test-part-1-answer (part-1 test-valid-ranges test-test-numbers))
(define test-part-2-answer (part-2 test-valid-ranges))

(define part-1-answer (part-1 valid-ranges test-numbers))
(define part-2-answer (part-2 valid-ranges))

(displayln "AOC 2025 Day 5\n--------------")
(displayln (format "Part 1 Answer: ~a" part-1-answer))
(displayln (format "Part 2 Answer: ~a" part-2-answer))
(displayln "--------------------")
(displayln (format "P1 Test Answer: ~a" test-part-1-answer))
(displayln (format "P2 Test Answer: ~a" test-part-2-answer))

;; -------------------------------------------------------------------------------------------------
