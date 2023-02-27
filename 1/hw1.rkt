#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))

; Exercise 1 - Define dupls-removed
(define (dupls-removed sent)
  (cond ((empty? sent) '())
        ((member? (first sent) (bf sent)) (dupls-removed (bf sent)))
        (else (se (first sent) (dupls-removed (bf sent)))))
)

; Exercise 2 - Define count-word
(define (count-word sent wd)
  (cond ((empty? sent) 0)
        ((equal? (first sent) wd) (+ 1 (count-word (bf sent) wd)))
        (else (+ 0 (count-word (bf sent) wd))))
)

; Exercise 3
(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

; Explain what would happen if you used new-if instead of if below.
#|
Infinite loop

|#

; Exercise 4 - Define squares
(define (square x) 
  (* x x))

(define (squares sent)
  (if (empty? sent)
      '()
      (se (square (first sent)) (squares (bf sent))))
)

; Exercise 5 - Define switch
(define (switch-helper sent)
  (cond ((empty? sent) '())
        ((or (equal? (first sent) 'I) (equal? (first sent) 'me)) (se 'you (switch-helper (bf sent))))
        ((equal? (first sent) 'you) (se 'me (switch-helper (bf sent))))
        (else (se (first sent) (switch-helper (bf sent)))))
)

(define (switch sent)
  (if (equal? (first sent) 'you)
      (se 'I (switch-helper (bf sent)))
      (switch-helper sent))
)

; Exercise 6 - Define ordered?
(define (ordered? sent)
  (cond ((= (count sent) 1) #t)
        ((> (first sent) (first (bf sent))) #f)
        (else (ordered? (bf sent))))
)

; Exercise 7 - Define ends-e
(define (ends-e sent)
  (cond ((empty? sent) '())
        ((equal? (last (first sent)) 'e) (se (first sent) (ends-e (bf sent))))
        (else (ends-e (bf sent))))
)

; Exercise 8
#|
Both special form, see the following:

(and (= 1 0) (/ 1 0))
> #f

(or (= 1 1) (/ 1 0))
> #t

They both avoid unnecessary evaluations
|#
