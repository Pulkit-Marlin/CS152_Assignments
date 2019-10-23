#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

(provide dictionary-closure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dictionary Closure                                                               ;;
;; ==================                                                               ;;
;;                                                                                  ;;
;; A choice of substitution can really trigger more substitutions by looking at the ;;
;; partially decrypted text - surely there will be some words which can be uniquely ;;
;; determined using the dictionary. It is prudent to concretize this "choice" as    ;;
;; this purely deterministic (involving absolutely no guess-work). In more          ;;
;; technical terms, this is called "maintaining arc-consistency" (look it up on     ;;
;; Wikipedia).                                                                      ;;
;;                                                                                  ;;
;; This function must utilise the dictionary and the cipher-word-list. Decrypt each ;;
;; word (`utils:decrypt`) and check if the dictionary has:                          ;;
;;                                                                                  ;;
;; 1. a unique completetion!                                                        ;;
;;    - Extend your key using the information with this match. Continue exploring   ;;
;;      the words under the extended key.                                           ;;
;; 2. many completions.                                                             ;;
;;    - Do nothing, just continue exploring the words under the same key. If none   ;;
;;      of the words fall into (1), return with the key that you have built so far. ;;
;; 3. no completions!                                                               ;;
;;    - Return `#f` (false), indicating that this partial key is wrong, and we must ;;
;;      revert to the original key.                                                 ;;
;;                                                                                  ;;
;; Returns either of:                                                               ;;
;; a. a (possibly) extended-key.                                                    ;;
;; b. `#f` if this key led to case (3) for some word.                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (zip l1 l2)
  (cond [(or (null? l1) (null? l2)) '()]
        [else (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))]))

(define PLAIN-BEGIN  65)  ; upcase
(define AtoZ (build-list 26 (lambda (x) (integer->char (+ x PLAIN-BEGIN)))))
(define cons-AtoZ (zip AtoZ AtoZ))

(define (capital? x)
  (let ([int_x (char->integer x)])
    (if (and (>= int_x 65) (<= int_x 90)) #t #f)))

(define (dictionary-closure key)
  (display "dc*:")
  (utils:show-key key)
  (displayln "Starting at the beginning of the word list:")
  (dc-helper1 key (cypher-word-list key)))
  
(define (cypher-word-list key)
  (utils:cipher-word-list-f (utils:decrypt key utils:ciphertext)))


(define (dc-helper1 key l)
  (cond [(null? l) key]
        [else (let ([word (car l)])
                (cond [(all-plain? word) (begin (display word) (displayln " --> skipping this one") (dc-helper1 key (cdr l)))]
                      [else (let ([v (valid word key dict '())])
                              (cond [(not v) #f]
                                    [(equal? v key) (dc-helper1 key (cdr l))]
                                    [else (dictionary-closure v)]))]))]))
  
(define (all-plain? word)
  (if (equal? word (string-upcase word)) #t #f))
  
(define dict (remove-duplicates utils:dictionary))
;;(define dict-len (length utils:dictionary))

(define (valid word key dict MATCHES)
  (cond [(null? dict) (cond [(null? MATCHES) (begin (display word)
                                                    (displayln " word cannot be completed")
                                                    #f)]
                            [(> (length MATCHES) 1) (begin (display word)
                                                           (display " --> multiple matches (")
                                                           (display (first MATCHES))
                                                           (display " ")
                                                           (display (second MATCHES))
                                                           (displayln " ... )")
                                                           key)]
                            [else (begin (display word)
                                         (display " --> unique match ")
                                         (displayln (string-downcase (car MATCHES)))
                                         (utils:add-substitution (remove-duplicates (filter (lambda (x) (if (capital? (cdr x)) #f #t)) (zip (string->list (car MATCHES)) (string->list word)))) key))])]
        [(> (length MATCHES) 1) (begin (display word)
                                       (display " --> multiple matches (")
                                       (display (first MATCHES))
                                       (display " ")
                                       (display (second MATCHES))
                                       (displayln " ... )")
                                       key)]
        [else 
         (if (not (equal? (string-length word) (string-length (car dict)))) (valid word key (cdr dict) MATCHES)
             (let* ([dict-word (car dict)]
                    [word-list (string->list word)]
                    [dict-word-list (string->list dict-word)])
               (if (andmap (lambda (x y) (if (capital? x) (if (equal? x y) #t #f) #t)) word-list dict-word-list)
                   (if (utils:is-monoalphabetic? (remove-duplicates (filter (lambda (x) (if (capital? (cdr x)) #f #t)) (zip dict-word-list word-list))) key)
                       (valid word key (cdr dict) (append (list dict-word) MATCHES))
                       (valid word key (cdr dict) MATCHES))
                   (valid word key (cdr dict) MATCHES))))]))
                        
                    
  
  
  
  
  
