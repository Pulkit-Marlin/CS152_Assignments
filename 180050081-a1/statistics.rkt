#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt")
         racket/match)

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; Ciphertext Statistics                                                           ;;
;; =====================                                                           ;;
;;                                                                                 ;;
;; The module should provide a bunch of functions that do statistical analysis of  ;;
;; ciphertext. The output is almost always just an ordered list (counts are        ;;
;; omitted).                                                                       ;;
;;                                                                                 ;;
;; Fill in the body for the skeletons and do not change the arguments. You can     ;;
;; define as many functions as you require, there's no special credit for          ;;
;; implementing/using all of them.                                                 ;;
;;                                                                                 ;;
;; CAUTION:                                                                        ;;
;; 1. Be mindful that bi, tri and quadgrams do not cross word boundaries. Hence,   ;;
;; you must process each word separately.                                          ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Analyses
(provide cipher-monograms
         cipher-bigrams
         cipher-unique-neighbourhood
         cipher-neighbourhood
         cipher-trigrams
         cipher-quadgrams
         cipher-common-words-single
         cipher-common-words-double
         cipher-common-words-triple
         cipher-common-words-quadruple
         cipher-common-initial-letters
         cipher-common-final-letters
         cipher-common-double-letters
         ;; any other functions of your design come below:

         ;; my-fundoo-analysis
         )

;; Takes ciphertext and produces a list of cipher chars sorted in decreasing
;; order of frequency.
(define (cipher-monograms ciphertext)
  (cipher-monograms-helper (string->list ciphertext) '())
  )

(define (cipher-monograms-helper l final-lst)
  (cond [(null? l) (takes-first (sort final-lst (lambda (x y) (if (> (cdr x) (cdr y)) #t #f))))]
        [else (if (char-alphabetic? (car l)) (let ([a (assoc (car l) final-lst)])
                                               (if a (cipher-monograms-helper (cdr l) (cons (cons (car a) (+ 1 (cdr a))) (remove a final-lst)))
                                                   (cipher-monograms-helper (cdr l) (cons (cons (car l) 1) final-lst)))) (cipher-monograms-helper (cdr l) final-lst))]))

(define (takes-first l)
  (match l
    ['() '()]
    [(cons a b) (cons (car a) (takes-first (cdr l)))]))



;; Takes the cipher-word-list and produces a list of 2-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-bigrams cipher-word-list)
  (cipher-bigrams-helper cipher-word-list '())
  )

(define (cipher-bigrams-helper cipher-w-l final-lst)
  (cond [(null? cipher-w-l) (takes-first (sort final-lst (lambda (x y) (if (> (cdr x) (cdr y)) #t #f))))]
        [else (cipher-bigrams-helper (cdr cipher-w-l) (cipher-bigrams-helper2 (word-to-bigrams (string->list (car cipher-w-l)) '()) final-lst))]))

(define (cipher-bigrams-helper2 l final-lst)
  (cond [(null? l) final-lst]
        [else(let ([a (assoc (car l) final-lst)])
               (if a (cipher-bigrams-helper2 (cdr l) (cons (cons (car a) (+ 1 (cdr a))) (remove a final-lst)))
                   (cipher-bigrams-helper2 (cdr l) (cons (cons (car l) 1) final-lst))))]))
(define (word-to-bigrams l final-lst)
  (if (= (length l) 1) final-lst
      (word-to-bigrams (cdr l) (cons (string (car l) (cadr l)) final-lst))))


 

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter. Only unique
;; neighbours are to be counted.
;; Consider the character #\o.
;;
;; Takes an argument `mode`:
;; 1. (cipher-unique-neighbourhood cipher-bigrams 'predecessor)
;;    - Count only those unique occurrences where the (given) char preceeds
;;      other char.
;;    - Patterns of the form: "o?"
;; 2. (cipher-unique-neighbourhood cipher-bigrams 'successor)
;;    - Count only those unique occurrences where the (given) char succeeds
;;      other char.
;;    - Patterns of the form: "?o"
;; 3. (cipher-unique-neighbourhood cipher-bigrams 'both)
;;    - Count all unique occurrences where the (given) char neighbours the other
;;      char.
;;    - Patterns of the form "?o" and "o?". Note that this is not sum of (1) and
;;    (2) always.
;;
;; The output is a list of pairs of cipher char and the count of it's
;; neighbours. The list must be in decreasing order of the neighbourhood count.
(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  (add-remaining atoz (cond [(equal? mode 'predecessor) (sort (rle (lc a : a <- atoz b <- cipher-bigrams-list @(equal? (car (string->list b)) a)))(lambda (x y) (if (> (cdr x) (cdr y)) #t #f)))]
                            [(equal? mode 'successor) (sort (rle (lc a : a <- atoz b <- cipher-bigrams-list @(equal? (last (string->list b)) a)))(lambda (x y) (if (> (cdr x) (cdr y)) #t #f)))]
                            [else (sort (rle (lc a : a <- atoz b <- cipher-bigrams-list @(member a (string->list b)))) (lambda (x y) (if (> (cdr x) (cdr y)) #t #f)))])))

(define (rle l)
  (if (null? l) '()
      (reverse (rle-helper l 1 1 '()))))

(define (rle-helper l i sub-count final-list)
  
  (if (= i (length l)) (cons (cons (list-ref l (- i 1)) sub-count) final-list)
      (if (equal? (list-ref l (- i 1)) (list-ref l i))
          (rle-helper l (+ 1 i) (+ 1 sub-count) final-list)
          (rle-helper l (+ 1 i) 1 (cons (cons (list-ref l (- i 1)) sub-count) final-list)))))

(define atoz (build-list 26 (lambda (x) (integer->char (+ x utils:CIPHER-BEGIN)))))

(define (add-remaining l1 l2)
  (if (null? l1) l2
      (if (assoc (car l1) l2) (add-remaining (cdr l1) l2)
          (add-remaining (cdr l1) (append l2 (list (cons (car l1) 0)))))))





;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter, but counts each
;; occurrence distinctly. This comment contains 6 bigrams with "w", all with "i" or "h".
;; So "w" has:
;; when mode is 'both,        6 neighbours
;; when mode is 'predecessor, 6 neighbours
;; when mode is 'successor,   0 neighbours
(define (cipher-neighbourhood cipher-word-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  

  (define (anti-rle l)
    (if (null? l) '()
        (append (build-list (cdar l) (lambda (x) (caar l))) (anti-rle (cdr l)))))

  (define (cipher-bigrams cipher-word-list)
    (cipher-bigrams-helper cipher-word-list '()))

  (define (cipher-bigrams-helper cipher-w-l final-lst)
    (cond [(null? cipher-w-l) (sort final-lst (lambda (x y) (if (> (cdr x) (cdr y)) #t #f)))]
          [else (cipher-bigrams-helper (cdr cipher-w-l) (cipher-bigrams-helper2 (word-to-bigrams (string->list (car cipher-w-l)) '()) final-lst))]))

  (define (cipher-bigrams-helper2 l final-lst)
    (cond [(null? l) final-lst]
          [else(let ([a (assoc (car l) final-lst)])
                 (if a (cipher-bigrams-helper2 (cdr l) (cons (cons (car a) (+ 1 (cdr a))) (remove a final-lst)))
                     (cipher-bigrams-helper2 (cdr l) (cons (cons (car l) 1) final-lst))))]))
  (define (word-to-bigrams l final-lst)
    (if (= (length l) 1) final-lst
        (word-to-bigrams (cdr l) (cons (string (car l) (cadr l)) final-lst))))

  (let* ([x (cipher-bigrams cipher-word-list)]
         [y (anti-rle x)]) 
    (add-remaining atoz (cond [(equal? mode 'predecessor) (sort (rle (lc a : a <- atoz b <- y @(equal? (car (string->list b)) a)))(lambda (x y) (if (> (cdr x) (cdr y)) #t #f)))]
                              [(equal? mode 'successor) (sort (rle (lc a : a <- atoz b <- y @(equal? (last (string->list b)) a)))(lambda (x y) (if (> (cdr x) (cdr y)) #t #f)))]
                              [else (sort (rle (lc a : a <- atoz b <- y @(member a (string->list b)))) (lambda (x y) (if (> (cdr x) (cdr y)) #t #f)))]))))


;; Takes the cipher-word-list and produces a list of 3-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-trigrams cipher-word-list)
  '())

;; Takes the cipher-word-list and produces a list of 4-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-quadgrams cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of single letter words, sorted
;; in decreasing order of frequency. Each element must be a string!
(define (cipher-common-words-single cipher-word-list)
  (takes-first (sort (rle (sort (lc a : a <- cipher-word-list @(= 1 (string-length a))) string<?)) (lambda (x y) (if (> (cdr x) (cdr y)) #t #f)))))

;; Takes the cipher word list and produces a list of double letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-double cipher-word-list)
  (takes-first (sort (rle (sort (lc a : a <- cipher-word-list @(= 2 (string-length a))) string<?)) (lambda (x y) (if (> (cdr x) (cdr y)) #t #f)))))

;; Takes the cipher word list and produces a list of triple letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-triple cipher-word-list)
  (takes-first (sort (rle (sort (lc a : a <- cipher-word-list @(= 3 (string-length a))) string<?)) (lambda (x y) (if (> (cdr x) (cdr y)) #t #f)))))

;; Takes the cipher word list and produces a list of four letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-quadruple cipher-word-list)
  (takes-first (sort (rle (sort (lc a : a <- cipher-word-list @(= 4 (string-length a))) string<?)) (lambda (x y) (if (> (cdr x) (cdr y)) #t #f)))))

;; Takes the cipher word list and produces a list of chars that appear at the
;; start of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-initial-letters cipher-word-list)
  (cipher-common-initial-letters-helper (map (lambda (x) (car (string->list x))) cipher-word-list) '()))

(define (cipher-common-initial-letters-helper initial-letter-list l)  
  (if (null? initial-letter-list) (takes-first (sort l (lambda (x y) (if (> (cdr x) (cdr y)) #t #f))))
      (let ([a (assoc (car initial-letter-list) l)]) (cipher-common-initial-letters-helper (cdr initial-letter-list) (if a (cons (cons (car a) (+ 1 (cdr a))) (remove a l))
                                                                                                                         (cons (cons (car initial-letter-list) 1) l))))))
 
      

;; Takes the cipher word list and produces a list of chars that appear at the
;; end of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-final-letters cipher-word-list)
  (cipher-common-final-letters-helper (map (lambda (x) (last (string->list x))) cipher-word-list) '()))

(define (cipher-common-final-letters-helper final-letter-list l)  
  (if (null? final-letter-list) (takes-first (sort l (lambda (x y) (if (> (cdr x) (cdr y)) #t #f))))
      (let ([a (assoc (car final-letter-list) l)]) (cipher-common-final-letters-helper (cdr final-letter-list) (if a (cons (cons (car a) (+ 1 (cdr a))) (remove a l))
                                                                                                                   (cons (cons (car final-letter-list) 1) l))))))
      
;; Takes the cipher word list and produces a list of chars that appear as
;; consecutive double letters in some word, sorted in decreasing order of
;; frequency. Each element must thus be a char!
(define (cipher-common-double-letters cipher-word-list)
  '())
