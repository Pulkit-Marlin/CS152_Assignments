#lang racket


;; You can require more modules of your choice.
(require racket/string
         racket/list
         (prefix-in utils: "utils.rkt"))

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))

(provide secret-word-enumeration-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                           ;;
;; Secret Word Enumeration                                                                   ;;
;; =======================                                                                   ;;
;;                                                                                           ;;
;; This step exploits the fact that all keys begin with a secret word that is a              ;;
;; proper SIX-LETTER word from the English dictionary.                                       ;;
;;                                                                                           ;;
;; Given a partial key, we can query the dictionary for a list of 6 letter words             ;;
;; that can potentially begin the key.                                                       ;;
;; We can then filter out potential candidates whose keys do not agree with our partial key. ;;
;;                                                                                           ;;
;; It is possible that given a (wrong) partial key, we discover that none of these           ;;
;; potential candidates can agree with our partial key. This really implies a                ;;
;; mistake in the choice of substitution, since dictionary-closure is completely             ;;
;; deterministic (modulo any bugs in your implementation :)                                  ;;
;;                                                                                           ;;
;; Hence, this function must return either of:                                               ;;
;; a. `#f` if there are no consistent candidates for this key.                               ;;
;; b. the original key if there are multiple consistent candidates.                          ;;
;; c. the complete key if there's only one consistent candidate!                             ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define 6-letter-list
  (filter (lambda (x) (if (check-duplicates x) #f #t)) (lc (string->list (string-downcase a)) : a <- utils:dictionary @(= (string-length a) 6))))

(define (secret-word-enumeration-1 key-after-dictionary-closure)
  (match key-after-dictionary-closure
    [(list a b c d e f g ... )
     (check-complete-key (lc p : p <- 6-letter-list @(letter-match (list a b c d e f) p)) key-after-dictionary-closure '())]))

(define (check-complete-key l key ans) ;;l is list of possible list if first 6 - returns either #f or key(when multiple) or complete-key

  (define (valid key l1)
    (cond [(or (null? l1) (null? key))  #t]
          [(and (>= (char->integer (car key)) 97) (<= (char->integer (car key)) 122))
           (if (equal? (car key) (car l1)) (valid (cdr key) (cdr l1))
               #f)]
          [else (valid (cdr key) (cdr l1))]))

  (cond [(> (length ans) 1) (begin (displayln "swe1: multiple matches found") key)]
        [(null? l) (cond [(null? ans) #f]
                         [(= (length ans) 1) (begin (displayln "swe1: complete key found!")
                                                    (display "swe1: secret word is ")
                                                    (match (car ans) [(list a b c d e f g ... ) (displayln (list->string (list a b c d e f)))])
                                                    (append* ans))]
                         [else (begin (displayln "swe1: multiple matches found") key)])]
        [else (let* ([l1 (utils:encryption-key (list->string (car l)))]
                     [v (valid key l1)])
                ;              (if v (check-complete-key (cdr l) key (append (list l1) ans))
                ;                 (check-complete-key (cdr l) key ans)))]))
                (if v (if (check-in-dictionary (list->string (car l))) (begin (display "swe1: potential consistent candidates: ")
                                                                              (displayln (car l))
                                                                              (check-complete-key (cdr l) key (append (list l1) ans)))
                          (check-complete-key (cdr l) key ans))
                    (check-complete-key (cdr l) key ans)))]))

(define (check-in-dictionary x)
  (let* ([compl-key (utils:encryption-key x)]
         [new-word-list (utils:cipher-word-list-f (utils:decrypt compl-key utils:ciphertext))])
   

    (check-in-dictionary2 new-word-list)))

(define (check-in-dictionary2 new-word-list)
  (if (null? new-word-list) #t
      (if (member (car new-word-list) utils:dictionary) (check-in-dictionary2 (cdr new-word-list)) #f)))


(define (letter-match l l-dict)
  (if (null? l) #t
      (cond [(and (>= (char->integer (car l)) 97) (<= (char->integer (car l)) 122))
             (if (equal? (car l) (car l-dict)) (letter-match (cdr l) (cdr l-dict))
                 #f)]
            [else (letter-match (cdr l) (cdr l-dict))])))