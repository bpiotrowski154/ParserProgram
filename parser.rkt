#lang racket

(define (fileAsList str)
  (string-split
    (string-trim
      (string-replace
        (string-replace
          (string-replace
            (string-replace (file->string str) "\n" " ") ;Read in the file as a string, replace all \n characters with a space
            ;(string-replace str "\n" " ")
          "\r" " ") ;replace all \r characters with a space
        "(" "( ") ;separate left parentheses from whatever it is directly next to
      ")" " )") ;separate right parentheses from whatever it is directly next to
    " ")) ;split the string into a list with spaces as the separator
  ;(define temp (file->list str))
  ;(file->list str)
  ;(removeSpaces temp) ;remove all the unnecessary whitespaces
  )

(define (remove token L) 
  (if (equal? token (first L))
      (rest L)
      #f
      ;(parseError)
      ))

(define (program L)
  (if (equal? (first L) 'idx)
      ((lineList L) (remove '$$ L))
      #f
      ;(parseError)
      ))

(define (lineList L)
  (cond
    [(equal? '$$ (first L)) #t]
    [(equal? 'idx (first L)) (line L) (lineList L)]
    [#f ;(parseError)
     ]))

(define (line L)
  (if (equal? (first L) 'idx)
      (remove 'idx L)
      ;(parseError)
      #f)
  (stmt L)
  (linetail L))

(define (stmt L)
  (cond
    [(equal? (first L) 'id)
     ((remove 'id L)
      (remove 'equals L)
      (expr L))]
    
    [(equal? (first L) 'if)
     ((remove 'if L)
      (expr L)
      (remove 'then L)
      (stmt L))]
    
    [(equal? (first L) 'read)
     ((remove 'read L)
      (remove 'id L))]
    
    [(equal? (first L) 'write)
     ((remove 'write L)
      (expr L))]
    
    [(equal? (first L) 'goto)
     ((remove 'goto L)
      (remove 'idx L))]
    
    [(equal? (first L) 'gosub)
     ((remove 'gosub L)
      (remove 'idx L))]
    
    [(equal? (first L) 'return) (remove 'return L)]))
    ;[else parseError]
  


(define (linetail L)
  (if (equal? 'colon (first L))
      ((remove 'colon L) (stmt L))
      L))
      

(define (expr L)
  (cond
    [(equal? (first L) 'id)
     ((remove 'id L)
      (etail L))]
    [(equal? (first L) 'idx)
     ((remove 'idx L)
      (etail L))]
    [(equal? (first L) 'num)
     ((remove 'num L)
      (etail L))]
    [(equal? (first L) 'lparen)
     ((remove 'lparen L)
      (expr L)
      (remove 'rparen L))]))
    ;[else parseError]

      
       
(define (etail L)
  (cond
    [(equal? (first L) 'plus)
     ((remove 'plus L)
     (expr L))]
    [(equal? (first L) 'minus)
     ((remove 'minus L)
     (expr L))]
    [(equal? (first L) 'equals)
     ((remove 'equals L)
      (expr L))]))




(define (string-alphabetic? str) ;Algorithm from discussion board post
   (define (iter ch-list)
       (if (empty? ch-list)
          #t
          (if (char-alphabetic? (first ch-list))
              (iter (rest ch-list))
              #f)))
     (iter (string->list str)))

(define (string-numeric? str) ;Same as string-alphabetic? algortithm but tests for numbers only
  (define (iter ch-list)
    (if (empty? ch-list)
        #t
        (if (char-numeric? (first ch-list))
            (iter (rest ch-list))
            #f)))
  (iter (string->list str)))

(define (idxOrNum? str) ;checks if a number is and idx or a num
  (if (> (string->number str) 0)
      'idx
      'num))
  

(define (flagger str)
  (cond
    [(string-numeric? str) (idxOrNum? str)] ;If the string is only numbers then return idx
    [(string-alphabetic? str) ;if the string is fully alphabetic
      (cond
        [(equal? str "if") 'if]
        [(equal? str "then") 'then]
        [(equal? str "goto") 'goto]
        [(equal? str "gosub") 'gosub]
        [(equal? str "return") 'return]
        [(equal? str "read") 'read]
        [(equal? str "write") 'write]
        [else 'id])]
    [else
     (cond
        [(equal? str ":") 'colon]
        [(equal? str "(") 'lparen]
        [(equal? str ")") 'rparen]
        [(equal? str "$$") '$$]
        [(equal? str "+") 'plus]
        [(equal? str "-") 'minus]
        [(equal? str "=") 'equals]
        [else 'err])]))
    
    

(define (scanner tokens)
  (println tokens)
;  (set! tokens (map (lambda (x) ;changes all entries in the list to be a string data type instead of datum
 ;        (if (number? x)
  ;           (number->string x)
   ;          (~a x))) tokens))
  ;(set! tokens (map flagger tokens))
  (map flagger tokens))


(define (parse fileName)
  ;(define tokens (fileAsList fileName))
  ;(scanner tokens)
  (if (program (scanner (fileAsList fileName)))
      (println "ACCEPTED")
      (println "ERROR")))

;(define (parse fileName)
 ; (define tokenList (for/list ([line (file->lines fileName)])
  ;  (fileAsList line)))
   ; (scanner tokenList))


(parse "file01.txt")
;(println " ")
;(parse "file02.txt")
;(println " ")
;(parse "file03.txt")
;(println " ")
;(parse "file04.txt")
;(println " ")
;(parse "file05.txt")
;(println " ")
;(parse "file06.txt")