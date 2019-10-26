#lang racket

(require 2htdp/batch-io)

(require "decision_functions.rkt")

;input dataset
(provide toytrain)
(define toytrain "../data/toy_train.csv")

(provide titanictrain)
(define titanictrain "../data/titanic_train.csv")

(provide mushroomtrain)
(define mushroomtrain "../data/mushrooms_train.csv")

;output tree (dot file)
(provide toyout)
(define toyout "../output/toy-decision-tree.dot")

(provide titanicout)
(define titanicout "../output/titanic-decision-tree.dot")

(provide mushroomout)
(define mushroomout "../output/mushroom-decision-tree.dot")



;reading input datasets
;read the csv file myfile as a list of strings
;with each line of the original file as an element of the list
;further split each line at commas
;so then we have a list of list of strings

(provide toy-raw)
(define toy-raw
  (cdr (read-csv-file toytrain)))
  
(provide titanic-raw)
(define titanic-raw
  (cdr (map (lambda (x) (drop x 2)) (read-csv-file titanictrain))))

(provide mushroom-raw)
(define mushroom-raw
  (cdr (read-csv-file mushroomtrain)))

;function to convert data to internal numerical format
;(features . result)
(provide format)
(define (format data)
  (cons (map (lambda (x) (string->number x)) (cdr data)) (string->number (car data))))

;list of (features . result)
(provide toy)
(define toy
  (map (lambda (x) (format x)) toy-raw))

(provide titanic)
(define titanic
  (map (lambda (x) (format x)) titanic-raw))

(provide mushroom)
(define mushroom
  (map (lambda (x) (format x)) mushroom-raw))

;============================================================================================================
;============================================================================================================
;============================================================================================================

;get fraction of result fields that are 1
;used to find probability value at leaf
(provide get-leaf-prob)
(define (get-leaf-prob data)
  (get-leaf-prob-helper data 0 (length data)))

(define (get-leaf-prob-helper data cnt l)
  (cond [(null? data) (/ cnt l)]
        [(= (cdr (car data)) 1) (get-leaf-prob-helper (cdr data) (+ 1 cnt) l)]
        [else (get-leaf-prob-helper (cdr data) cnt l)]))
      

;get entropy of dataset
(provide get-entropy)
(define (get-entropy data)
  (let* ([p1 (get-leaf-prob data)]
         [p2 (- 1 p1)])
    (cond [(= 0 p1) (- 0 (* p2 (log p2 2)))]
          [(= 0 p2) (* p1 (log p1 2))]
          [else (- 0 (+ (* p1 (log p1 2)) (* p2 (log p2 2))))])))

;find the difference in entropy achieved
;by applying a decision function f to the data
(provide entropy-diff)
(define (entropy-diff f data)
(- (get-entropy data) (/ (entropy-diff-helper f data '() '()) (length data))))

(define (entropy-diff-helper f data l1 ans)
  (if (null? data) (my-sum ans)
      (let* ([y (f (caar data))]
             [i (index-of l1 y)]
             [c (car data)])
        (if (number? i) (entropy-diff-helper f (cdr data) l1 (list-update ans i (lambda (x) (cons c x))))
            (entropy-diff-helper f (cdr data) (cons y l1) (cons (list c) ans))))))

(define (my-sum l)
  (foldr (lambda (x y) (+ y (* (get-entropy x) (length x)))) 0 l))

;choose the decision function that most reduces entropy of the data
(provide choose-f)
(define (choose-f candidates data) ; returns a decision function
(argmax (lambda (x) (entropy-diff (cdr x) data)) candidates))


(provide DTree)
(struct DTree (desc func kids) #:transparent)

;build a decision tree (depth limited) from the candidate decision functions and data
(provide build-tree)

(define (build-tree candidates data depth)
  (build-tree1 candidates data depth 0))

(define (build-tree1 candidates data depth prev)
  (let ([x (get-leaf-prob data)]) 
  (if (or (null? candidates) (= depth 0) (= x 0) (= x 1)) (DTree (~a x) (cons 100 prev) '())
  (let* ([chosen (choose-f candidates data)]
        [l (divide-data chosen candidates data depth)])
    (DTree (car chosen) (cons (cdr chosen) prev) l)))))
    
  
(define (divide-data f candidates data depth)
  (divide-data-helper f candidates data depth '() '()))

(define (divide-data-helper f candidates data depth l1 ans)
  (if (null? data) (my-tree f (remove f candidates) ans (- depth 1))
      (let* ([y ((cdr f) (caar data))]
             [i (index-of l1 y)]
             [c (car data)])
        (if (number? i) (divide-data-helper f candidates (cdr data) depth l1 (list-update ans i (lambda (x) (cons c x))))
            (divide-data-helper f candidates (cdr data) depth (cons y l1) (cons (list c) ans))))))

(define (my-tree f candidates ans depth)
 
(define (my-tree-helper ans final-ans)
  (if (null? ans) final-ans
       (my-tree-helper (cdr ans) (append final-ans (list (build-tree1 candidates (car ans) depth  ((cdr f) (caaar ans))))))))
(my-tree-helper ans '()))

;(define (count-one data)
;  (count-one-helper data 0))
;
;(define (count-one-helper data ans)
;  (cond [(null? data) ans]
;        [(= (cdar data) 1) (count-one-helper (cdr data) (+ 1 ans))]
;        [else (count-one-helper (cdr data) ans)]))
;      

;given a test data (features only), make a decision according to a decision tree
;returns probability of the test data being classified as 1

(provide make-decision)
(define (make-decision tree test)
  (match tree
    [(DTree desc func kids)
     (if (or (null? kids) (number? (car func))) (string->number desc)
         (let* ([x ((car func) test)])
           (make-decision-helper kids x test)))]))

(define (make-decision-helper kids x test)
  (if (null? kids) 0
  (match (car kids)
    [(DTree desc func kid)
     (if (equal? x (cdr func)) (make-decision (car kids) test)
         (make-decision-helper (cdr kids) x test))])))
           
  

;============================================================================================================
;============================================================================================================
;============================================================================================================

;annotate list with indices
(define (pair-idx lst n)
  (if (empty? lst) `() (cons (cons (car lst) n) (pair-idx (cdr lst) (+ n 1))))
  )

;generate tree edges (parent to child) and recurse to generate sub trees
(define (dot-child children prefix tabs)
  (apply string-append
         (map (lambda (t)
                (string-append tabs
                               "r" prefix
                               "--"
                               "r" prefix "t" (~a (cdr t))
                               "[label=\"" (~a (cdr t)) "\"];" "\n"
                               (dot-helper (car t)
                                           (string-append prefix "t" (~a (cdr t)))
                                           (string-append tabs "\t")
                                           )
                               )
                ) children
                  )
         )
  )

;generate tree nodes and call function to generate edges
(define (dot-helper tree prefix tabs)
  (let* ([node (match tree [(DTree d f c) (cons d c)])]
         [d (car node)]
         [c (cdr node)])
    (string-append tabs
                   "r"
                   prefix
                   "[label=\"" d "\"];" "\n\n"
                   (dot-child (pair-idx c 0) prefix tabs)
                   )
    )
  )

;output tree (dot file)
(provide display-tree)
(define (display-tree tree outfile)
  (write-file outfile (string-append "graph \"decision-tree\" {" "\n"
                                     (dot-helper tree "" "\t")
                                     "}"
                                     )
              )
  )
;============================================================================================================
;============================================================================================================
;============================================================================================================
