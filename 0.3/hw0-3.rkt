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
  (location-helper small big 1)
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
(define (gpa grades)
  ; your code here
 (error "Not yet implemented")
)

; Exercise 8 - Define repeat-words
(define (repeat-words sent)
  ; your code here
 (error "Not yet implemented")
)

; Exercise 9 - Define same-shape?
(define (same-shape? sent1 sent2)
  ; your code here
 (error "Not yet implemented")
)
