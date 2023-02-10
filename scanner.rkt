#lang racket

(define (fileAsList str)
  (define temp (string->list(file->string str))) ;take in a file as a string and convert it into a list
  (define temp3 (file->string str))
  (define temp2 (string-split (string-trim (string-replace (file->string str) "\n" " ")) " "))
  (println temp)
  (println temp3)
  (println temp2)
  (removeSpaces temp) ;remove all the unnecessary whitespaces
  )



(define (removeSpaces lst) ;algorithm modified off of removesymbols algorithm from https://stackoverflow.com/questions/36928298/remove-symbols-from-list-scheme
  (cond
    [(null? lst) '()]
    [(equal? (car lst) #\space) (removeSpaces (cdr lst))]
    [(equal? (car lst) #\newline) (removeSpaces (cdr lst))]
    [else (cons (car lst) (removeSpaces (cdr lst)))]))
  

;(define (flagger str)
 ; (cond
  ;  [(char-alphabetic? str) 'id]
    

(define (scanner tokens)
  ;(println tokens)
  (println "done")
  )


(define (parse fileName)
  (define tokens (fileAsList fileName))
  (scanner tokens))



;(define keywords (fileAsList "file01.txt"))
;(scanner keywords)
(parse "file02.txt")