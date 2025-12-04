#lang racket

(define (load-raw-data path)
  ;; I dont have a unix system to test this on but this should filter out both \r\n's if it's correct.
  (define data (file->string path))
  (define split-carriage (filter (lambda (x) (not (equal? "\r" x))) (string-split data "\r")))
  (filter (lambda (x) (not (equal? "\n" x))) (string-split (string-join split-carriage "") "\n")))

(define (raw->instructions raw-data)
  (reverse (for/fold ([result (list)]) ([raw (in-list raw-data)])
             (define instruction (substring raw 0 1))
             (define amount (substring raw 1))
             (cond
               [(equal? instruction "L") (cons (list - (string->number amount)) result)]
               [else (cons (list + (string->number amount)) result)]))))

;; Part 1
(define (find-password-p1 instructions)
  (let*-values ([(_ password) (for/fold ([score 50]
                                         [result 0])
                                        ([ins (in-list instructions)])
                                (define new-score (modulo ((first ins) score (last ins)) 100))
                                (cond
                                  [(= 0 new-score) (values new-score (add1 result))]
                                  [else (values new-score result)]))])
    password))

;; Part 2
(define (find-password-p2 instructions)
  (define full-rotation 100)

  (define (get-hits start to-move operation)
    (define rotations (quotient to-move full-rotation))
    (define movement (- to-move (* full-rotation rotations)))
    (define touch-point
      (cond
        [(zero? (modulo start full-rotation)) full-rotation]
        [(equal? operation -) (abs (- 0 start))]
        [else (- full-rotation start)]))
    (cond
      [(>= movement touch-point) (add1 rotations)]
      [else rotations]))

  (let*-values ([(_ result) (for/fold ([cur 50]
                                       [sub-total 0])
                                      ([ins (in-list instructions)])
                              (define operation (first ins))
                              (define moves (last ins))
                              (define zero-hits (get-hits cur moves operation))
                              (values (modulo (operation cur moves) full-rotation)
                                      (+ sub-total zero-hits)))])
    result))

(define path "./day1Input.txt")
(define raw-data (load-raw-data path))
(define instructions (raw->instructions raw-data))
(define test-input (list "L68"  "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14" "L82"))
(define test-instructions (raw->instructions test-input))

(define test-password (find-password-p2 test-instructions))
(define password-p1 (find-password-p1 instructions))
(define password-p2 (find-password-p2 instructions))

(displayln "AOC 2025 Day 1")
(displayln (format "Test input Answer 2: ~a" test-password))
(displayln (format "Part 1 answer: ~a" password-p1))
(displayln (format "Part 2 answer: ~a" password-p2))

