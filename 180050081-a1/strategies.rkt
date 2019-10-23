#lang racket

;; You can require more modules of your choice.
(require racket/list
         (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt")
         racket/match)

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                     ;;
;; Strategies                                                                          ;;
;; ==========                                                                          ;;
;; For the purpose of this assignment, just the `etai` strategy is expected, since     ;;
;; we have dictionary-closure and secret-word-enumeration to leap-frog to the right    ;;
;; key. This technique would fail for harder keys which are arbitrary permutations of  ;;
;; the alphabet. We will be forced to explore many more strategies (along with         ;;
;; dictionary-closure of course).                                                      ;;
;;                                                                                     ;;
;; Strategies to guess substitutions for the key using statistical information about   ;;
;; - the English language from utils.rkt                                               ;;
;; - the cipher text      from statistics.rkt                                          ;;
;;                                                                                     ;;
;; Follow the function signature as indicated below. Deviations will make it           ;;
;; impossible for automatic grading of your submission.                                ;;
;; Moreover, we do not expect your strategies to require any more/different            ;;
;; arguments. Note that you recieve the key as argument, so you can at the very        ;;
;; least ensure that all the substitutions are monoalphabetic wrt this key.            ;;
;;                                                                                     ;;
;; Signature:                                                                          ;;
;; ```                                                                                 ;;
;; (define (my-fundoo-strategy key)                                                    ;;
;;   ;; Make use of `utils:ciphertext`, `utils:cipher-word-list`                       ;;
;;   ...)                                                                              ;;
;; ```                                                                                 ;;
;;                                                                                     ;;
;; Substitutions                                                                       ;;
;; -------------                                                                       ;;
;; In order to extend the key incrementally, we use `utils:add-substitution` to        ;;
;; extend a given key with a substitution.                                             ;;
;;                                                                                     ;;
;; A substitution is a list of pairs, each pair mapping a plaintext char to a          ;;
;; ciphertext char. For example, to extend the key with T -> a and O -> r              ;;
;; (simultaneously), we use the substitution:                                          ;;
;; ```                                                                                 ;;
;; (list (cons #\T #\a) (cons #\O #\r))                                                ;;
;; ```                                                                                 ;;
;; For a single substitution use a singleton list (containing just one pair).          ;;
;;                                                                                     ;;
;; **CAUTION**                                                                         ;;
;; -----------                                                                         ;;
;; 1. Note that add-substitution does not do sanity checks on the substitution and use ;;
;;    of `utils:is-monoalphabetic` is recommended to ensure that you don't             ;;
;;    inadvertently create invalid keys.                                               ;;
;; 2. You must provide a list called `compositions` in this module.                    ;;
;;                                                                                     ;;
;; See docs in "utils.rkt" for more information.                                       ;;
;;                                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You must add "public" functions of this module to this list.
(provide etai
         ;; Some more suggested strategies:
         
         ;; common-words-double
         ;; bigrams
         common-initial-letters
         common-final-letters
         ;; common-words-triple
         ;; trigrams
         ;; common-double-letters
         ;; common-words-quadruple
         ;; quadgrams
         
         ;; lists of strategies
         composition)

;; A strategy that uses some statistical information to generate potential
;; substitutions for E, T, A and I.
;; Refer the assignment manual for tips on developing this strategy. You can
;; interact with our etai with the executable we provide.
(define (etai key)
  (cond [(>= (length (stats:cipher-common-words-single utils:cipher-word-list)) 2)
         (let* ([x1 (car (string->list (first (stats:cipher-common-words-single utils:cipher-word-list))))]   ;x1 stores the character of A first then I
                [x2 (car (string->list (second (stats:cipher-common-words-single utils:cipher-word-list))))]   ;x2 stores the character of B first then I
                [top5 (match (stats:cipher-monograms utils:ciphertext)
                        [(list a b c d e f ... ) (list a b c d e)])]
                [ET_ (remove* (list x1 x2) top5)]
                [my-neighbourhood (stats:cipher-unique-neighbourhood (stats:cipher-bigrams utils:cipher-word-list) 'both)]
                [order-of-T (sort (lc a : a <- my-neighbourhood @(member (car a) ET_)) (lambda (x y) (if (< (cdr x) (cdr y)) #t #f)))])
           
           (if (<= (- (cdr (second order-of-T)) (cdr (first order-of-T))) 3)
               (lc z : z <- (list-based-on-neighbour (possible-ET order-of-T ET_) x1 x2) @(utils:is-monoalphabetic? z key))
               (lc z : z <- (list-based-on-freq ET_ order-of-T x1 x2) @(utils:is-monoalphabetic? z key))))]
        

        [(= 1 (length (stats:cipher-common-words-single utils:cipher-word-list)))
         (let* ([x1 (car (string->list (first (stats:cipher-common-words-single utils:cipher-word-list))))]   ;x1 stores the character of A first then I
                [top5 (match (stats:cipher-monograms utils:ciphertext)
                        [(list a b c d e f ... ) (list a b c d e)])]
                [ET_ (remove* (list x1) top5)]
                [my-neighbourhood (stats:cipher-unique-neighbourhood (stats:cipher-bigrams utils:cipher-word-list) 'both)]
                [order-of-T (sort (lc a : a <- my-neighbourhood @(member (car a) ET_)) (lambda (x y) (if (< (cdr x) (cdr y)) #t #f)))])

           (lc z : z <- (append* (lc (list (list (cons #\E b) (cons #\T (car a)) (cons #\A x1) (cons #\I c)) (list (cons #\E b) (cons #\T (car a)) (cons #\A c) (cons #\I x1)))
                                     : a <- order-of-T b <- (remove (car a) ET_) c <- (remove (car a) ET_) @(not (equal? b c)))) @(utils:is-monoalphabetic? z key)))]

        [else
         (let* (
                [top5 (match (stats:cipher-monograms utils:ciphertext)
                        [(list a b c d e f ... ) (list a b c d e)])]
                [my-neighbourhood (stats:cipher-unique-neighbourhood (stats:cipher-bigrams utils:cipher-word-list) 'both)]
                [order-of-T (sort (lc a : a <- my-neighbourhood @(member (car a) top5)) (lambda (x y) (if (< (cdr x) (cdr y)) #t #f)))])

           (lc z : z <- (lc (list (cons #\E b) (cons #\T (car a)) (cons #\A c) (cons #\I d))
                            : a <- order-of-T b <- (remove (car a) top5) c <- (remove (car a) top5) d <- (remove (car a) top5)
                            @(and (not (equal? b c)) (not (equal? d c)) (not (equal? d b))))  @(utils:is-monoalphabetic? z key)))]))


(define (double-AI list-of-1-sub) 
  (define (AI-helper a b)
    (append (take list-of-1-sub 2) (list (cons (car a) (cdr b))) (list (cons (car b) (cdr a)))))
  (list list-of-1-sub (AI-helper (third list-of-1-sub) (fourth list-of-1-sub))))

(define (possible-ET order-of-T ET_)
  (if (null? order-of-T) '()
      (append (list (list (remove (caar order-of-T) ET_) (caar order-of-T))) (possible-ET (cdr order-of-T) ET_))))

(define (list-based-on-neighbour possible x1 x2)
  (append* (lc (double-AI (list (cons #\E b) (cons #\T (second a)) (cons #\A x1) (cons #\I x2))) :  
               a <- possible b <- (car a))))


(define (list-based-on-freq ET_ order-of-T x1 x2)
  (let ([l (first-4-lists order-of-T ET_ x1 x2)])
    (append l (remove* l (make-freq-list (lc (cons y z) : y <- ET_ z <- ET_ @(not (equal? y z))) x1 x2)))))
                                               
  
    
(define (make-freq-list l x1 x2)
  (if (null? l) '()
      (append (double-AI (list (cons #\E (caar l)) (cons #\T (cdar l)) (cons #\A x1) (cons #\I x2))) (make-freq-list (cdr l) x1 x2))))

(define (first-4-lists order-of-T ET_ x1 x2)
  (let* ([x3 (caar order-of-T)]
         [E_ (remove x3 ET_)]
         [x4 (first E_)]
         [x5 (second E_)])
    (define l1
      (list (cons #\E x4) (cons #\T x3) (cons #\A x1) (cons #\I x2)))
    (define l2
      (list (cons #\E x5) (cons #\T x3) (cons #\A x1) (cons #\I x2)))
    (append (double-AI l1) (double-AI l2))))



;
;(define (common-initial-letters key)
;  (match (stats:cipher-common-initial-letters utils:cipher-word-list)
;    [(list a b c d e ... )
;     (let ([l1 (list (list (cons #\T a)) (list (cons #\T b)) (list (cons #\T c)) (list (cons #\T d)))])
;       (filter (lambda (x) (utils:is-monoalphabetic? x key)) l1))]))

(define (common-initial-letters key)
  (let* ([l (my-take 4  (stats:cipher-common-initial-letters utils:cipher-word-list))]
         [l1 (lc (list (cons #\T x)) : x <- l)])
    (filter (lambda (x) (utils:is-monoalphabetic? x key)) l1)))




(define (common-final-letters key)
  (let* ([l (my-take 5 (stats:cipher-common-final-letters utils:cipher-word-list))] 
         [l1 (lc (list (cons #\E x) (cons #\S y)) : x <- l y <- (remove x l))])

    (filter (lambda (x) (utils:is-monoalphabetic? x key)) l1)))

(define (my-take x l)
  (if (>= (length l) x) (take l x)
      l))
  
  




;; A suggested composition of strategies that might work well. Has not been
;; exhaustively tested by us. Be original ;)
(define composition (list etai common-final-letters common-initial-letters))
;; common-words-double
;; bigrams
;; common-initial-letters
;; common-final-letters
;; common-words-triple
;; trigrams
;; common-double-letters))
;; common-words-quadruple
;; quadgrams))

