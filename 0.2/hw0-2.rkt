#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))

;Exercise 0
;Write 5 expressions whose values are the number ten:
;1. Atom
;1
;2. Compound Expression (3 Atoms)
;(+ 1 2)
;3. Compound Expression (4 Atoms)
;(+ 1 2 3)
;4. Compound Expression (1 Atom and 2 subexpressions)
;(+ (+ 1 1) (+ 1 1))
;5. Any Other Kind Expression
;(if #t 'foo 'bar)

;Exercise 1
(define (second wd)
  (first (bf wd)))

;1. Define first-two
(define (first-two wd)
  (word (first wd) (second wd))
)

;;2. Define two-first
(define (two-first x y)
  (word (first x) (first y))
)

;;3. Define two-first-sent
(define (two-first-sent sent)
  (word (first (first sent)) (first (second sent)))
)

;Exercise 2 - Define teen?
(define (teen? num)
  (if (and (> num 12) (< num 20))
      #t
      #f)
)

;Exercise 3 - Define indef-article
(define (starts-with-vowel? wd)
  (member? (first wd) 'aeiou)
)

(define (indef-article wd)
  (if (starts-with-vowel? wd)
      (sentence 'an wd)
      (sentence 'a wd))
)

;Exercise 4 - Define insert-and
(define (insert-and sent)
  (sentence (butlast sent) 'and (last sent))
)

;Exercise 5 - Define query
(define (query sent)
  (sentence (second sent) (first sent) (butfirst (butfirst (butlast sent))) (word (last sent) '?))
)

;Exercise 6 - Define european-time and american-time
(define (european-time time)
  (cond ((and (= (first time) 12) (equal? (second time) 'am)) 0)
        ((and (= (first time) 12) (equal? (second time) 'pm)) 12)
        ((equal? (second time) 'am) (first time))
        (else (+ (first time) 12)))
)

(define (american-time time)
  (cond ((= time 0) '(12 am))
        ((= time 12) '(12 pm))
        ((< time 12) (sentence time 'am))
        (else (sentence (- time 12) 'pm)))
)

;Exercise 7 - Define describe-time
(define (describe-time secs)
  (cond ((< secs 60) (sentence secs 'seconds))
        ((< secs 3600) (sentence (/ secs 60.0) 'minutes))
        ((< secs 86400) (sentence (/ secs 3600.0) 'hours))
        (else (sentence (/ secs 86400) 'days)))
)

;Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective wd)
  (se (word adjective 'est) wd)
)

#|

The formal parameter `word` was overriding the procedure defined as `word`.

|#
