#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))

;;Begin Project 1
(require "adjectives.rkt") 

;;Below is some procedures to help you out. Dont worry about what it says or does.
;;Questions are below.

(define (want-exit? line)
  (or (member? 'exit line) (member? 'quit line) (member? 'bye line)))

(define (print-sentence sent)
  (for-each (lambda (x) (display x) (display " "))
            sent)
  (newline))

(define (interact bot)
  (define (helper)
    (display "> ") (flush-output)
    (let ([line (read-line)])
      (unless (want-exit? line)
        (print-sentence (bot line))
        (helper))))
  (read-line)
  (helper))

(define (chatter bot1 bot2 start iterations)
  (define (helper b1 b2 sent i)
    (when (< i iterations)
          (display "bot ") (display (add1 (remainder i 2))) (display ": ")
          (let ((out (b1 sent)))
            (print-sentence out)
            (helper b2 b1 out (add1 i)))))
  (display "start: ") (print-sentence start)
  (helper bot1 bot2 start 0))

;;; Checks if a given word is an adjective or not
;;; Requires adjectives.scm to be loaded beforehand
(define adjective?
  (let ((hash (make-hash)))
    (for-each (lambda (adj)
		(hash-set! hash adj #t))
	      adjectives)
    (lambda (wd) (hash-ref hash wd #f))))


;; Begin Questions:
;;Q1 - babybot
  (define (babybot sent) sent)

;;Q2 - stupidbot-creator
  (define (stupidbot-creator motto)
    (lambda (sent) (se motto))
  )

;;Q3 - matcherbot-creator
  (define (match? pattern sent)
    (cond ((empty? pattern) (empty? sent))
    ((empty? sent)
    (and (equal? (first pattern) '*) (match? (bf pattern) sent)))
    ((equal? (first pattern) '*)
    (or (match? pattern (bf sent))
        (match? (bf pattern) sent)))
    (else (and (equal? (first pattern) (first sent))
        (match? (bf pattern) (bf sent))))))

  (define (after-match pattern sent initial-pattern)
    (cond ((empty? pattern) sent)
          ((equal? (first pattern) (first sent)) 
            (after-match (bf pattern) (bf sent) initial-pattern))
          (else (after-match initial-pattern (bf sent) initial-pattern)))
  )

  (define (matcherbot-creator pattern)
    (lambda (sent)
      (cond ((empty? pattern) sent)
            ((match? (se '* pattern '*) sent) 
              (after-match pattern sent pattern))
            (else #f)))
  )

;;Q4 - substitutebot-creator
  (define (location small big)
    (cond ((not (member? small big)) #f) 
          ((equal? small (first big)) 1)
          (else (+ 1 (location small (bf big))))))

  (define (substitutebot-creator from to)
    (lambda (sent) (every (lambda (wd)
                              (cond ((member? wd from) (item (location wd from) to))
                                    ((member? wd to) (item (location wd to) from))
                                    (else (se wd)))) sent))
  )

;;Q5 - switcherbot
  (define (switcherbot sent)
    (se ((substitutebot-creator '(I am was my yours) '(you are were your mine)) (se (first sent) '()))
        ((substitutebot-creator '(me I am was my yours) '(you you are were your mine)) (bf sent)))
  )


;;Q6 - inquisitivebot
  (define (inquisitivebot sent)
    (se (switcherbot sent) '?)
  )
  
;;Q7 - eliza
  (define (eliza sent)
    (cond ((empty? sent) '(how can I help you ?))
          ((equal? (first sent) 'hello) '(hello there!))
          ((equal? '? (last sent)) '(I can not answer your question.))
          ((match?  '(* I am *) sent) (se '(why are you) (inquisitivebot ((matcherbot-creator '(I am)) sent))))
          (else (switcherbot sent)))
  )

;;Q8 - reactorbot-creator
  (define (reactorbot-creator bot pat out)
    (lambda (sent) 
      (if (match? (se '* pat '*) sent)
        out
        (bot sent)))
  )

;;Q9 - replacerbot-creator
  (define (replacerbot-creator bot pat before after)
    (lambda (sent) 
      (if (match? (se '* pat '*) sent)
        (se before (after-match pat sent pat) after)
        (bot sent)))
  )

;;Q10 - exagerate
 (define (exaggerate bot num)
   (if (zero? num)
       bot
       (lambda (sent) (bot ((exaggerate  bot (- num 1)) (add-very sent))))))


(define (add-very sent)
  (cond ((empty? sent) '())
        (else (if (adjective? (first sent))
                  (se 'very (first sent) (add-very (bf sent)))
                  (se (first sent) (add-very (bf sent)))))))

;;REMEMBER TO ADD YOUR OWN TESTS TO GRADER.RKT!
;;END OF PROJECT 1
