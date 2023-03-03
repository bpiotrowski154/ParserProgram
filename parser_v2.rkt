#lang racket

(define (parse fileName)
  (program (readFile fileName)))
            
(define (readFile fileName) ;Takes in the file name and converts each line in the text file to a list of strings making a list of the lines
  (for/list ([i (file->lines fileName)])
    (string-split i)))

(define (scanner str)
  (cond
    [(string-numeric? str) (idxOrNum? str)] ;If the string is only numbers then return idx and original number
    [(string-alphabetic? str) ;if the string is fully alphabetic
      (cond
        [(equal? str "if")        'if]
        [(equal? str "then")      'then]
        [(equal? str "goto")      'goto]
        [(equal? str "gosub")     'gosub]
        [(equal? str "return")    'return]
        [(equal? str "read")      'read]
        [(equal? str "write")     'write]
        [else 'id])]
    [else  ;Otherwise the string is some symbol
     (cond
        [(equal? str ":")         'colon]
        [(equal? str "(")         'lparen]
        [(equal? str ")")         'rparen]
        [(equal? str "$$")        '$$]
        [(equal? str "+")         'plus]
        [(equal? str "-")         'minus]                 
        [(equal? str "=")         'equals]                    
        [else                     'err])]))

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

(define (idxOrNum? str) 
  (if (> (string->number str) 0)
      'idx 'num))

(define (program L)
  (let ([result (linelist L)])
    (cond
      [(equal? result #t)            (println "ACCEPTED")]
      [(equal? result 'MISSINGLINE)  (println "MISSING LINE NUMBER")]
      [(equal? result 'MISSING$$)    (println "MISSING $$ AT END OF FILE")]
      [else                          (printf "SYNTAX ERROR ON LINE ~a" result)])))

(define (linelist L) ;Returns true if there is a linelist followed by $$ otherwise either returns the line number an error occurred or that there was no line number
  (cond
    [(empty? L)                                      'MISSING$$]
    [(equal? (first (first L)) "$$")                 #t]
    [(number? (string->number (first (first L))))
        (if (list? (line (first L)))                 (linelist (rest L))
                                                     (first (first L)))]
    [else                                            'MISSINGLINE]))
         
(define (line L)
  (if (empty? L)
      #f 
      (let ([lineNumber (first L)])
        (let ([tokens (map scanner L)])
          (if (equal? (first tokens) 'idx)
              (stmt (rest tokens))
              #f)))))

(define (linetail L)
  (cond
    [(empty? L)                    #f] ;checks if the list is empty, returns false if so
    [(equal? (first L) 'err)       #f] ;checks if the first item in the list is an unidentified op, returns false if so
    [(equal? (first L) 'colon)     (stmt (rest L))] ;removes colon from front oif the list and passes list into stmt
    [else                          L]))

(define (stmt L)
  (cond
    [(empty? L)                    #f] ;checks if the list is empty, returns false if so
    [(equal? (first L) 'err)       #f] ;checks if the first item in the list is an unidentified op, returns false if so
    [(equal? (first L) 'id)        (expr (rest (rest L)))] ;removes id and equals from front of list and passes that into expr   
    [(equal? (first L) 'if)        (stmt (rest (expr (rest L))))] ;removes if from front of list and passes that into expr, removes then from front of list returned by expr and passes that list into stmt    
    [(equal? (first L) 'read)      (rest (rest L))] ;removes read and id from front of list
    [(equal? (first L) 'write)     (expr (rest L))] ;removes write from front of list and passes that to expr
    [(equal? (first L) 'goto)      (rest (rest L))] ;removes goto and idx from front of list
    [(equal? (first L) 'gosub)     (rest (rest L))] ;removes gosub and idx from front of list
    [(equal? (first L) 'return)    (rest L)]
    [else                          #f]))

(define (expr L)
  (cond
    [(empty? L)                    #f] ;checks if the list is empty, returns false if so
    [(equal? (first L) 'err)       #f] ;checks if the first item in the list is an unidentified op, returns false if so
    [(equal? (first L) 'id)        (etail (rest L))] ;removes id from front of the list and passes list into etail
    [(equal? (first L) 'idx)       (etail (rest L))] ;removes idx from front of the list and passes list into etail
    [(equal? (first L) 'num)       (etail (rest L))] ;removes num from front of the list and passes list into etail
    [(equal? (first L) 'lparen)    (rest (expr (rest L)))] ;removes lparen from front of the list and passes list into etail then removes rparen from the list returned from etail
    [else                          #f]))

(define (etail L)
  (cond
    [(empty? L)                    L] ;checks if the list is empty, returns false if so
    [(equal? (first L) 'err)       #f] ;checks if the first item in the list is an unidentified op, returns false if so
    [(equal? (first L) 'plus)      (expr (rest L))];removes plus from the front of the list and passes list into expr
    [(equal? (first L) 'minus)     (expr (rest L))];removes minus from the front of the list and passes list into expr
    [(equal? (first L) 'equals)    (expr (rest L))];removes equals from the front of the list and passes list into expr    
    [else                          L])) ;epsilon

(parse "sampleParse.txt")
(parse "file01.txt")
(parse "file02.txt")
(parse "file03.txt")
(parse "file04.txt")
(parse "file05.txt")
(parse "file06.txt")
(parse "file07.txt")
(parse "file08.txt")
(parse "file09.txt")