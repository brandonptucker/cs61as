#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))

; Exercise 1 - Define describe-time
(define (describe-time secs)
  (cond ((< secs 60) (se secs 'seconds))
        ((< secs 3600) (se (quotient secs 60) 'minutes (describe-time (remainder secs 60))))
        ((< secs 86400) (se (quotient secs 3600) 'hours (describe-time (remainder secs 3600))))
        (else (se (quotient secs 86400) 'days (describe-time (remainder secs 86400)))))
)

; Exercise 2 - Define remove-once
(define (remove-once wd sent)
  (cond ((empty? sent) '())
	((equal? wd (first sent))
	(remove-once '() (bf sent)))
	(else (se (first sent) (remove-once wd (bf sent)))))
)

; Exercise 3 - Define differences
(define (differences nums)
(if (= (count nums) 1)
      '()
      (se (- (first (bf nums)) (first nums))
          (differences (bf nums))))
)

; Exercise 4 - Define location
(define (location-helper small big count)
(cond ((empty? big) #f)
        ((equal? small (first big))
         count)
        (else (location-helper small (bf big) (+ count 1))))
)

(define (location small big)
  (cond ((not (member? small big)) #f) 
        ((equal? small (first big)) 1)
        (else (+ 1 (location small (bf big)))))
)

; Exercise 5 - Define initials
(define (initials sent)
 (if (empty? sent)
      '()
      (se (first (first sent))
	  (initials (bf sent))))
)

; Exercise 6 - Define copies
(define (copies num wd)
  (if (= num 0)
      '()
      (se wd 
	  (copies (- num 1) wd)))
)

; Exercise 7 - Define gpa
(define (base-grade grade)
  (cond ((equal? (first grade) 'A) 4)
        ((equal? (first grade) 'B) 3)
        ((equal? (first grade) 'C) 2)
        ((equal? (first grade) 'D) 1)
        (else 0))
)

(define (grade-modifier grade)
  (cond ((equal? (last grade) '+) .33)
        ((equal? (last grade) '-) -.33)
        (0))
)

(define (gpa-helper grades)
  (if (empty? grades)
      0
      (+ (base-grade (first grades)) 
         (grade-modifier (first grades)) 
         (gpa-helper (bf grades))))
)

(define (gpa grades)
  (/ (gpa-helper grades) (count grades))
)

; Exercise 8 - Define repeat-words
(define (repeat-word wd count)
  (if (= count 1)
      '()
      (se wd (repeat-word wd (- count 1))))
)

(define (repeat-words sent)
  (cond ((empty? sent) '())
        ((number? (first sent)) (se (repeat-word (first (bf sent)) (first sent)) (repeat-words (bf sent))))
        (else (se (first sent) (repeat-words (bf sent)))))
)

; Exercise 9 - Define same-shape?
(define (count-not-equal? a b)
  (not (= (count a) (count b)))
)

(define (same-shape? sent1 sent2)
  (cond ((count-not-equal? sent1 sent2) #f)
        ((empty? sent1) #t)
        ((count-not-equal? (first sent1) (first sent2)) #f)
        (else (same-shape? (bf sent1) (bf sent2))))
)
