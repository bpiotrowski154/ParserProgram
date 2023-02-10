#lang racket

(define (fileAsList str)
  ;(define temp (string-split (string-trim (string-replace (file->string str) "\n" " ")) " "))
  ;(define temp (file->list str))
  (file->list str)
  ;(removeSpaces temp) ;remove all the unnecessary whitespaces
  )



;(define (removeSpaces lst) ;algorithm modified off of removesymbols algorithm from https://stackoverflow.com/questions/36928298/remove-symbols-from-list-scheme
 ; (cond
  ;  [(null? lst) '()]
   ; [(equal? (car lst) #\space) (removeSpaces (cdr lst))]
    ;[(equal? (car lst) #\newline) (removeSpaces (cdr lst))]
    ;[else (cons (car lst) (removeSpaces (cdr lst)))]))
  
(define (string-alphabetic? str) ;Algorithm from discussion board post
   (define (iter ch-list)
       (if (empty? ch-list)
          #t
          (if (char-alphabetic? (first ch-list))
              (iter (rest ch-list))
              #f)))
     (iter (string->list str)))


(define (flagger str)
  (if (string-alphabetic? str)
      (cond
        [(equal? str "if") 'if]
        [(equal? str "then") 'then]
        [(equal? str "goto") 'goto]
        [(equal? str "gosub") 'gosub]
        [(equal? str "return") 'return]
        [(equal? str "read") 'read]
        [(equal? str "write") 'write]
        [else 'id])
      (if (and (number? str)(positive? (string->number str)))
          'idx
          (cond
            [(equal? str ":") 'colon]
            [(equal? str "(") 'lparen]
            [(equal? str ")") 'rparen]
            [(equal? str "$$") '$$]
            [(equal? str "+") 'plus]
            [(equal? str "-") 'minus]
            [(equal? str "=") 'equals]
            [else 'err]))))

;Test for number then alphabetic then symbol


  ;(cond
  ;  [(equal? str "if") 'if]
  ;  [(equal? str "then") 'then]
  ;  [(equal? str "goto") 'goto]
  ;  [(equal? str "gosub") 'gosub]
  ;  [(equal? str "return") 'return]
  ;  [(equal? str "read") 'read]
  ;  [(equal? str "write") 'write]
  ;  [(equal? str ":") 'colon]
  ;  [(equal? str "(") 'lparen]
  ;  [(equal? str ")") 'rparen]
  ;  [(equal? str "$$") '$$]
  ;  [(equal? str "+") 'plus]
  ;  [(equal? str "-") 'minus]
   ; [(equal? str "=") 'equals]
   ;; ;[(char-alphabetic? str) 'id]
    ;[(positive? (string->number str)) 'idx]
    ;[else 'err]))
    
    

(define (scanner tokens)
  tokens
  (println tokens)
  (set! tokens (map (lambda (x)
         (if (number? x)
             (number->string x)
             (~a x))) tokens))
  (println tokens)
  (set! tokens (map flagger tokens))
  (println tokens)
  )


(define (parse fileName)
  (define tokens (fileAsList fileName))
  (scanner tokens))



(parse "file01.txt")