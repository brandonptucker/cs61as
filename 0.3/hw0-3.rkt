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
  ;your code here
 (error "Not yet implemented")
)

; Exercise 4 - Define location
(define (location small big)
  ; your code here
 (error "Not yet implemented")
)

; Exercise 5 - Define initials
(define (initials sent)
  ; your code here
 (error "Not yet implemented")
)

; Exercise 6 - Define copies
(define (copies num wd)
  ; your code here
 (error "Not yet implemented")
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
