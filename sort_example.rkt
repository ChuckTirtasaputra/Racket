#lang racket
; selection sort using user provided (possibly custom) ordering functions

(define (selection L direction) 
   (cond ( (null? L) '() )  ; if L is null return empty list
         ( else (cons  ; construct a new list from using the "smallest" element in L
                 (smallest L (car L) direction)
                 ; and then remove that element and recursively select the next ones
                 (selection (remove L (smallest L (car L) direction)) direction))
         )
   )
)

;def selection(L, direction):
;   if (L == undefined) return []
;   else return smallest(L, L[0], direction) + selection(remove(L, smallest(L, L[0], direction)), direction)


(define (smallest L A direction) ; looks for the smallest element in the list
                                 ; atom A is the current smallest
  (cond ( (null? L) A)
        ( (direction (car L) A) (smallest (cdr L)(car L) direction))
        (else (smallest (cdr L) A direction))
  )
)

;def smallest(L, A, direction):
;   if (L == undefined) return A
;   elseif direction(L[0], A) return smallest(L[1:], L[0], direction)
;   else smallest(L[1:], A, direction)


(define (remove L A)  ; remove the first occurance of atom A from L
  (cond ( (null? L) '() )           
        ( (= (car L) A) (cdr L)) ; Match found! 
        (else (cons (car L)(remove (cdr L) A)))   ; keep searching
  )
)

; def remove(L, A):
;    if (L == undefined) return []
;    elseif (L[0] == A) return L[1:]
;    else return L[0] + remove(L[1:], A)


  
(define (small-odd A B) 
  (cond ((and (odd? A) (even? B)) #t)
        ((and (odd? B) (even? A)) #f)
        (else (< A B))))


;(display ''(1 8 9 3 6 3 5 4 3))
;(newline)
;(selection '(1 8 9 3 6 3 5 4 3) <)
;(newline)
;(selection '(1 8 9 3 6 3 5 4 3) >)
;(newline)
;(selection '(1 8 9 3 6 3 5 4 3) small-odd)
