#lang racket

;(null-ld? obj)
;the listdiff is empty if the head and tail
;are the same list
(define (null-ld? obj)
  (if (not(pair? obj)) #f
      (eq? (car obj) (cdr obj))))

;(listdiff? obj)
;first element is L, last element is L or
;a sublist of L
(define (listdiff? obj)
  (cond ((not(pair? obj)) #f)
        ((null-ld? obj) #t)
        (#t (let cmp-ld ((fst (car obj))
                         (lst (cdr obj)))
              (cond ((null? lst) #t)
                    ((not(pair? fst)) #f)
                    ((eq? fst lst) #t)
                    (#t (cmp-ld (cdr fst) lst)))))))

;(cons-ld obj listdiff)
;attach the object to the front of the represented listdiff,
;the prefix of the first element of the physical listdiff
(define (cons-ld obj listdiff)
  (if (listdiff? listdiff)
      (cons (cons obj (car listdiff)) (cdr listdiff)) #f))

;(car-ld listdiff)
;if the represented listdiff is not empty, then this represents the
;first element of the first element of the physical listdiff
(define (car-ld listdiff)
  (if (or (not(listdiff? listdiff)) (null-ld? listdiff)) #f
      (car (car listdiff))))

;(cdr-ld listdiff)
;Representation of the listdiff without the first element
;Remove the first element of the prefix from the physical listdiff
(define (cdr-ld listdiff)
  (if (or (not(listdiff? listdiff)) (null-ld? listdiff)) #f
      (cons (cdr (car listdiff)) (cdr listdiff))))

;(listdiff . obj)
;all arguments will be passed in as a list (L), so to create the
;create a new list whose first element L and second is the empty
;list, so the sublist that is the listdiff is the list L
(define (listdiff . obj)
  (cons obj '()))

;(length-ld listdiff)
;made tail-recursive by adding extra len argument to keep track
;of size "so far" as function recurses
(define (length-ld listdiff)
  (cond ((not(listdiff? listdiff)) #f)
        ((null-ld? listdiff) 0)
        (#t
         (let count ((fst (car listdiff))
                     (lst (cdr listdiff))
                     (len 0))
           (cond ((eq? fst lst) len)
                 ((not(pair? fst)) (+ len 1))
                 (#t (count (cdr fst) lst (+ len 1))))))))

;(append-ld listdiff …)
;Most definitely not tail-recursive but it works...
;use cons-ld to easily attach previous listdiffs to the first el
;of last listdiff, preserving pointer to last listdiff.
(define (append-ld . listdiff)
  (if (null? listdiff) '()
      (let makelstdff ((cur (car listdiff))
                       (rmn (cdr listdiff)))
        (cond ((null? rmn) cur)
              ((null-ld? cur) (makelstdff (car rmn) (cdr rmn)))
              ((not(pair? cur))
               (cons-ld (car cur) (makelstdff (car rmn) (cdr rmn))))
              ((eq? (car cur) (cdr cur))
               (makelstdff (car rmn) (cdr rmn)))
              (#t (cons-ld (car (car cur))
                           (makelstdff (cdr-ld cur) rmn)))))))
            

;(assq-ld obj alistdiff)
;time permitting, this can also be implemented with take
(define (assq-ld obj alistdiff)
  (cond ((not(listdiff? alistdiff)) #f)
        ((null-ld? alistdiff) #f)
        (#t (let mtch ((fst (car alistdiff))
                        (lst (cdr alistdiff))
                        (mt-obj obj))
              (cond ((not(pair? (car fst))) #f)
                    ((eq? fst lst) #f)
                    ((eq? mt-obj (car (car fst))) (car fst))
                    (#t (mtch (cdr fst) lst mt-obj)))))))

;(list->listdiff list)
;same as (listdiff obj ...) except the arguments are different
(define (list->listdiff list)
  (cons list '()))

;(listdiff->list listdiff)
;Can easily obtain the represented listdiff by using the
;library function take, which returns a list. Remember that
;the first element of the physical listdiff holds the prefix of
;the tail, so we can use the length-ld function to extract the
;prefix from from the first element.
(define (listdiff->list listdiff)
  (if (listdiff? listdiff)
      (take (car listdiff) (length-ld listdiff)) #f))

;(expr-returning listdiff)
;can utilize same sort of methodology as in previous function
;but return a scheme expression instead until it needs to be evaluated
(define (expr-returning listdiff)
  (quasiquote (cons ', (listdiff->list listdiff) '())))