#lang racket

(require (planet dyoo/simply-scheme))

(provide (all-defined-out))

; > (downup1 'm)
; '(m)
(define (downup1 wd)
  (se wd))

; > (downup2 'ma)
; '(ma m ma)
(define (downup2 wd)
  (se wd (downup1 (bl wd)) wd))

; > (downup3 'mar)
; '(mar ma m ma mar)
(define (downup3 wd)
  (se wd (downup2 (bl wd)) wd))

; > (downup4 'mars)
; '(mars mar ma m ma mar mars)
(define (downup4 wd)
  (se wd (downup3 (bl wd)) wd))

(define (downup wd)
  (if (= (count wd) 1)
    (se wd)
    (se wd (downup (bl wd)) wd)))

; > (pigl0 'alabaster)
; 'alabasteray
(define (pigl0 wd)
  (word wd 'ay))

; > (pigl1 'salami)
; 'alamisay
(define (pigl1 wd)
  (pigl0 (word (bf wd) (first wd))))

; > (pigl2 'trace)
; 'acetray
(define (pigl2 wd)
  (pigl1 (word (bf wd) (first wd))))

(define (pigl wd)
  (if (member? (first wd) 'aeiou)
    (word wd 'ay)
    (pigl (word (bf wd) (first wd)))))

; > (explode 'dynamite)
; 'd y n a m i t e


; > (explode '())
; '()
(define (explode0 wd) '())

; > (explode 'a)
; '(a)
(define (explode1 wd)
  (se (first wd) (explode0 (bf wd))))

; > (explode 'ab)
; '(a b)
(define (explode2 wd)
  (se (first wd) (explode1 (bf wd))))

; > (explode 'abc)
; '(a b c)
(define (explode3 wd)
  (se (first wd) (explode2 (bf wd))))

(define (explode wd)
  (if (empty? wd)
    '()
    (se (first wd) (explode (bf wd)))))

; > (letter-pairs 'george)
; '(ge eo or rg ge)


; > (letter-pairs0 '())
; '()
(define (letter-pairs0 wd) '())

; > (letter-pairs1 'g)
; '()
(define (letter-pairs1 wd) '())

; > (letter-pairs2 'ge)
; '(ge)
(define (letter-pairs2 wd) 
  (se wd))

; > (letter-pairs3 'geo)
; '(ge eo)
(define (letter-pairs3 wd) 
  (se (bl wd) 
      (letter-pairs2 (bf wd))))

; > (letter-pairs4 'geor)
; '(ge eo or)
(define (letter-pairs4 wd) 
  (se (bl (bl wd)) 
      (letter-pairs3 (bf wd))))

(define (first-two wd)
  (word (first wd) (first (bf wd))))

(define (letter-pairs wd)
  (if (<= (count wd) 1)
      '()
      (se (first-two wd)
	  (letter-pairs (bf wd)))))

; > (reverse 'beatle)
; 'eltaeb

; > (reverse 'beatles)
; 'seltaeb

; (last wd) (reverse (bl wd))

(define (reverse wd)
  (if (= (count wd) 1)
    wd
    (word (last wd) (reverse (bl wd)))))


(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

; > (evens '(i want to hold your hand))
; (WANT HOLD HAND)

(define (evens sent)
  (if (<= (count sent) 1)
      '()
      (se (first (bf sent))
	  (evens (bf (bf sent))))))


(define (down wd)
  (if (empty? wd)
    '()
    (se wd (down (bl wd)))))

