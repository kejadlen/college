;;;
;;; Useful redefinitions
;;;

(define (first lst) (car lst))
(define (second lst) (cadr lst))
(define (rest lst) (cdr lst))
(define (last lst) (car (reverse lst)))
(define (neq? x y) (not (eq? x y)))
(define (atom? x) (not (list? x)))
(define (exists? x) (not (null? x)))

;;;
;;; These tests should be self-explanatory
;;;

(define (operand? x)
  (or (variable? x) (integer? x)))

(define (operator? x)
  (or (eq? x '+) (eq? x '-)))

(define (variable? x)
  (and (symbol? x)
       (let ((sym-as-string (symbol->string x)))
         (and (= (string-length sym-as-string) 1)
              (string<=? sym-as-string "e")
              (string>=? sym-as-string "a")))))

;;;
;;; Utility functions
;;;

(define (variable-id name)
  (- (char->integer (string-ref (symbol->string name) 0))
     (char->integer #\a)))

(define (assign-help var-id value state)
  (if (= 0 var-id)
      (cons value (rest state))
      (cons (first state) (assign-help (- var-id 1) value (rest state)))))

(define (assign var val state)
  (assign-help (variable-id var) val state))

;;;
;;; My poor man's regex. Regexes consist of a list with tokens.
;;; Returns a list of lists, each interior list containing the
;;; matched terms, not including the tokens. Essentially the
;;; same as /(.*)[token](.*)[token](.*).../. Interior lists must
;;; contain tokens in order to be in the returned list; this is
;;; to check syntax errors due to the nature of the block
;;; notation.
;;; 
;;; (match '(b d g) '(a b c d e f g h)) => '((a) (c) (e f) (h))
;;; (match '(b d g) '(b 1 d g)) => '((1))
;;;
(define (match-help regex statement current)
  (cond ((null? statement) current)
        ((null? regex) (list statement))
        ((eq? (first regex) (first statement))
         (append (if (null? current) '() (list current))
                 (match-help (rest regex)
                             (rest statement)
                             '())))
        (else
         (match-help
          regex 
          (rest statement)
          (append current
                  (list (first statement)))))))

(define (match regex statement)
  (match-help regex statement '()))

;;;
;;; Parsing functions
;;;

;;; <operand> -> <variable> | <integer>
(define (parse-operand op state)
  (cond ((integer? op) op)
        ((variable? op) (list-ref state (variable-id op)))))
  
;;; <expression> -> <operand> { ( + | - ) <operand> }
;;;
;;; parse-expr-help is used to facilitate left-associativity
;;; when parsing the expression.
(define (parse-expr-help expr state total)
  (if (null? expr)
      total
      (if (and (operator? (first expr)) (operand? (second expr)))
          (parse-expr-help
           (cddr expr) ; take two tokens off the expression
           state
           (eval (list
                  (first expr) ; the operator
                  total ; total first (left-associativity)
                  (parse-operand (second expr) state)))) ; the operand
          'syntax-error)))

(define (parse-expr expr state)
  (if (and (operand? (first expr))
           (= 1 (modulo (length expr) 2))) ; expressions must have an
                                           ; odd number of tokens
      (parse-expr-help
       (rest expr)
       state
       (parse-operand (first expr) state))
      'syntax-error))

;;; <assign> -> \( <variable> := <expression> \)
(define (parse-assign statement state)
  (let ((match-result (match '(:=) statement)))
    (if (= 2 (length match-result))
        (let* ((var (caar match-result))
               (expr (cadr match-result))
               (val (parse-expr expr state)))
          (if (integer? val)
              (assign var val state)
              'syntax-error))
    'syntax-error)))

;;; <if> -> \( if <expression> then <statement> else <statement> \)
(define (parse-then-else val then else state)
  ;; parses the then and passes the state to the else
  (let ((then-state (parse-statement then state))
        (else-state (parse-statement else state)))
    (if (or (atom? then-state) (atom? else-state))
        'syntax-error
        (if (> val 0) then-state else-state))))
        
(define (parse-if statement state)
  (let ((match-result (match '(if then else) statement)))
    (if (= 3 (length match-result))
        (let* ((expr (first match-result))
               (then (second match-result))
               (else (last match-result))
               (val (parse-expr expr state)))
        (if (integer? val)
            (parse-then-else val then else state)
            'syntax-error))
        'syntax-error)))

;;; <while> -> \( while <expression> do <statement> \)
(define (parse-while-help expr do state)
  (let ((val (parse-expr expr state))
        (new-state (parse-statement do state)))
    (cond ((or (not (integer? val)) (atom? new-state))
           'syntax-error)
          ((> val 0) (parse-while-help expr
                                       do
                                       new-state))
          (else state))))

(define (parse-while statement state)
  (let ((match-result (match '(while do) statement)))
    (if (and
          (= 2 (length match-result))
          (list? (last match-result))
          (list? (first (last match-result))))
        (let* ((expr (first match-result))
               (do (last match-result)))
          (parse-while-help expr do state))
        'syntax-error)))
 
(define (parse-statement statement state)
  (cond ((atom? statement) 'syntax-error)
        ((null? statement) state)
        (else
         (let ((x (first statement))
               (y (rest statement)))
           (cond ((and (list? x) (or (null? y) (list? (first y))))
                  (parse-statement y
                                   (parse-statement x state)))
                 ((variable? x)
                  (parse-assign statement state))
                 ((eq? x 'if)
                  (parse-if statement state))
                 ((eq? x 'while)
                  (parse-while statement state))
                 (else 'syntax-error))))))

(define (interpret-OSIL2 program state)
  (if (and (list? program) (or (null? program) (list? (first program))))
      (parse-statement program state)
      'syntax-error))

;;; for testing purposes
(define (test line actual expected)
  (if (equal? actual expected)
    (display ".")
    (disp-error line actual expected)))
    
(define (disp-error line actual expected)
  (newline)
  (display line)
  (display ": ")
  (display actual)
  (display ", ")
  (display expected)
  (newline))
 
;;; assign
(test 189 (assign 'a 1 '(0 0 0 0 0)) '(1 0 0 0 0))
(test 189 (assign 'e 1 '(0 0 0 0 0)) '(0 0 0 0 1))

;;; parse-operand
(test 192 (parse-operand 'a '(0 1 2 3 4)) 0)
(test 192 (parse-operand 'e '(0 1 2 3 4)) 4)
(test 192 (parse-operand 12 '(0 1 2 3 4)) 12)
(test 192 (parse-operand -14 '(0 1 2 3 4)) -14)

;;; parse-expr
(test 195 (parse-expr '(10) '(0 1 2 3 4)) 10)
(test 195 (parse-expr '(a) '(0 1 2 3 4)) 0)
(test 195 (parse-expr '(10 + b - 5 + a) '(0 1 2 3 4)) 6)
(test 195 (parse-expr '(10 + ab) '(0 1 2 3 4)) 'syntax-error)
(test 195 (parse-expr '(10 * a) '(0 1 2 3 4)) 'syntax-error)
(test 195 (parse-expr '(ab) '(0 1 2 3 4)) 'syntax-error)
(test 195 (parse-expr '(10 +) '(0 1 2 3 4)) 'syntax-error)

;;; parse-assign
(test 198 (parse-assign '(a := b + 10 + c) '(0 1 2 3 4)) '(13 1 2 3 4))
(test 198 (parse-assign '(a := ab) '(0 1 2 3 4)) 'syntax-error)
(test 198 (parse-assign '(a) '(0 1 2 3 4)) 'syntax-error)

;;; parse-if
(test 201 (parse-if '(if 1 then (a := 1) else (b := 0)) '(0 1 2 3 4)) '(1 1 2 3 4))
(test 201 (parse-if '(if -1 then (a := 1) else (b := 0)) '(0 1 2 3 4)) '(0 0 2 3 4))
(test 201 (parse-if '(if ab then (a := 1) else (b := 0)) '(0 1 2 3 4)) 'syntax-error)
(test 201 (parse-if '(if) '(0 1 2 3 4)) 'syntax-error)
(test 201 (parse-if '(if then) '(0 1 2 3 4)) 'syntax-error)
(test 201 (parse-if '(if then else) '(0 1 2 3 4)) 'syntax-error)
(test 201 (parse-if '(if a then (a := 1)) '(0 1 2 3 4)) 'syntax-error)
(test 201 (parse-if '(if then (a := 1) else (b := 2)) '(0 1 2 3 4)) 'syntax-error)
(test 201 (parse-if '(if 1 then) '(0 1 2 3 4)) 'syntax-error)
(test 201 (parse-if '(if -1 then (a := 1) else (b)) '(0 1 2 3 4)) 'syntax-error)

;;; parse-while
(test 204 (parse-while '(while e do (e := e - 1)) '(0 1 2 3 4)) '(0 1 2 3 0))
(test 204 (parse-while '(while 1 do (a)) '(0 1 2 3 4)) 'syntax-error)
(test 204 (parse-while '(while =1 do (a)) '(0 1 2 3 4)) 'syntax-error)
(test 204 (parse-while '(while ab do (e := e - 1)) '(0 1 2 3 4)) 'syntax-error)
(test 204 (parse-while '(while a do (a)) '(0 1 2 3 4)) 'syntax-error)
(test 204 (parse-while '(while) '(0 1 2 3 4)) 'syntax-error)
(test 204 (parse-while '(while do) '(0 1 2 3 4)) 'syntax-error)
(test 204 (parse-while '(while a do) '(0 1 2 3 4)) 'syntax-error)
(test 204 (parse-while '(while do b := 0) '(0 1 2 3 4)) 'syntax-error)

;;; parse-statement
(test 207 (parse-statement 'a '(0 1 2 3 4)) 'syntax-error)
(test 207 (parse-statement '() '(0 1 2 3 4)) '(0 1 2 3 4))
(test 207 (parse-statement '((a := 1) (b := 0)) '(0 1 2 3 4)) '(1 0 2 3 4))
(test 207 (parse-statement '((a := 1) (ab)) '(0 1 2 3 4)) 'syntax-error)

;;; programs from protect spec
(test 245 (interpret-OSIL2 '((a := 5) (b := 0) (while a do ((b := b + a) (a := a - 1)))) '(0 0 0 0 0)) '(0 15 0 0 0))
;23
(test 255 (interpret-OSIL2 '((a := 1) (c := 0) (e := 0) (while 10 - a do ((if e then ((c := c + 1) (e := 0 - 1)) else ()) (a := a + 1) (e := e + 1)))) '(0 0 0 0 0)) '(10 0 4 0 1))
;28
(test 238 (interpret-OSIL2 '((a := 0) b) '(0 1 2 3 4)) 'syntax-error)
;30
(test 240 (interpret-OSIL2 '((a := 10ab)) '(0 1 2 3 4)) 'syntax-error)
;33
(test 242 (interpret-OSIL2 '((a :=)) '(0 1 2 3 4)) 'syntax-error)
;34
(test 244 (interpret-OSIL2 '((a := 10 * b)) '(0 1 2 3 4)) 'syntax-error)
;35
(test 246 (interpret-OSIL2 '((a := 10 +)) '(0 1 2 3 4)) 'syntax-error)
;36
(test 248 (interpret-OSIL2 '((if then else)) '(0 1 2 3 4)) 'syntax-error)
;37
(test 250 (interpret-OSIL2 '((if 1 then (a := 0) else (b))) '(0 1 2 3 4)) 'syntax-error)
;38
(test 252 (interpret-OSIL2 '((if -1 then (a) else (b := 1))) '(0 1 2 3 4)) 'syntax-error)
;39
(test 254 (interpret-OSIL2 '((m := 1)) '(0 1 2 3 4)) 'syntax-error)
;40
(test 255 (interpret-OSIL2 '((if a - then (a := 0) else (b := 1))) '(0 0 0 0 0)) 'syntax-error)
;41
(test 256 (interpret-OSIL2 '((while)) '(0 0 0 0 0)) 'syntax-error)
;42
(test 204 (interpret-OSIL2 '((while e do (e := e - 1))) '(0 1 2 3 4)) '(0 1 2 3 0))
(test 204 (interpret-OSIL2 '((while 1 do (a))) '(0 1 2 3 4)) 'syntax-error)
(test 204 (interpret-OSIL2 '((while =1 do (a))) '(0 1 2 3 4)) 'syntax-error)
(test 204 (interpret-OSIL2 '((while ab do (e := e - 1))) '(0 1 2 3 4)) 'syntax-error)
(test 204 (interpret-OSIL2 '((while a do (a))) '(0 1 2 3 4)) 'syntax-error)
(test 204 (interpret-OSIL2 '((while do)) '(0 1 2 3 4)) 'syntax-error)
(test 204 (interpret-OSIL2 '((while a do)) '(0 1 2 3 4)) 'syntax-error)
(test 204 (interpret-OSIL2 '((while do b := 0)) '(0 1 2 3 4)) 'syntax-error)
(test 257 (interpret-OSIL2 '((while a - do (b := 0))) '(0 0 0 0 0)) 'syntax-error)
;44
(test 198 (interpret-OSIL2 '((a := b + 10 + c)) '(0 1 2 3 4)) '(13 1 2 3 4))
(test 198 (interpret-OSIL2 '((a := ab)) '(0 1 2 3 4)) 'syntax-error)
(test 198 (interpret-OSIL2 '((a)) '(0 1 2 3 4)) 'syntax-error)
;44
(test 258 (interpret-OSIL2 '((a := a -)) '(0 0 0 0 0)) 'syntax-error)
(test 259 (interpret-OSIL2 '((a := a * 0)) '(0 0 0 0 0)) 'syntax-error)
(test 260 (interpret-OSIL2 '() '(0 0 0 0 0)) '(0 0 0 0 0))
(test 261 (interpret-OSIL2 '((while -1 do (a := 1))) '(0 0 0 0 0)) '(0 0 0 0 0))
(test 262 (interpret-OSIL2 '((while -1 do a := 1)) '(0 0 0 0 0)) 'syntax-error)
(test 263 (interpret-OSIL2 '((while 1 do a := 1)) '(0 0 0 0 0)) 'syntax-error)
(test 264 (interpret-OSIL2 '(while 1 do a := 1) '(0 0 0 0 0)) 'syntax-error)
;44
(test 265 (interpret-OSIL2 '((while (a - 1) do (a := 1))) '(0 0 0 0 0)) 'syntax-error)
;45
(test 266 (interpret-OSIL2 '((while do (a := 12))) '(0 0 0 0 0)) 'syntax-error)
(test 267 (interpret-OSIL2 '((a := 1) b := 0) '(0 0 0 0 0)) 'syntax-error)
(test 268 (interpret-OSIL2 '((b := 1 (a := 0))) '(0 0 0 0 0)) 'syntax-error)
(test 269 (interpret-OSIL2 '((while f do (a := a - 1))) '(0 0 0 0 0)) 'syntax-error)
(test 270 (interpret-OSIL2 '((= := 2)) '(0 0 0 0 0)) 'syntax-error)
;46
(test 271 (interpret-OSIL2 '((a :=)) '(0 0 0 0 0)) 'syntax-error)
(test 272 (interpret-OSIL2 '((0 := 1)) '(0 0 0 0 0)) 'syntax-error)
(test 273 (interpret-OSIL2 '((:= 0)) '(0 0 0 0 0)) 'syntax-error)
(test 274 (interpret-OSIL2 '((0)) '(0 0 0 0 0)) 'syntax-error)
(test 275 (interpret-OSIL2 '((a := a - -2)) '(0 0 0 0 0)) '(2 0 0 0 0))
(test 276 (interpret-OSIL2 '((a := ())) '(0 0 0 0 0)) 'syntax-error)
(test 277 (interpret-OSIL2 '((f := 0)) '(0 0 0 0 0)) 'syntax-error)
(test 278 (interpret-OSIL2 '((a := 1) (b := 1 c)) '(0 0 0 0 0)) 'syntax-error)
(test 279 (interpret-OSIL2 '(()) '(0 0 0 0 0)) '(0 0 0 0 0))
(test 280 (interpret-OSIL2 '((a)) '(0 0 0 0 0)) 'syntax-error)
(test 281 (interpret-OSIL2 '((ab)) '(0 0 0 0 0)) 'syntax-error)
(test 282 (interpret-OSIL2 '((ab :=)) '(0 0 0 0 0)) 'syntax-error)
(test 283 (interpret-OSIL2 '((if)) '(0 0 0 0 0)) 'syntax-error)
(test 284 (interpret-OSIL2 '((if ab)) '(0 0 0 0 0)) 'syntax-error)
(test 285 (interpret-OSIL2 '((if ab then)) '(0 0 0 0 0)) 'syntax-error)
;46
(test 286 (interpret-OSIL2 '((if 1)) '(0 0 0 0 0)) 'syntax-error)
(test 287 (interpret-OSIL2 '((if 0)) '(0 0 0 0 0)) 'syntax-error)
(test 288 (interpret-OSIL2 '((if 1 then ab)) '(0 0 0 0 0)) 'syntax-error)
(test 289 (interpret-OSIL2 '((if 0 then ab)) '(0 0 0 0 0)) 'syntax-error)
;47
(test 290 (interpret-OSIL2 '((if 1 then () ab)) '(0 0 0 0 0)) 'syntax-error)
(test 291 (interpret-OSIL2 '((if 0 then () ab)) '(0 0 0 0 0)) 'syntax-error)
(test 292 (interpret-OSIL2 '((if 1 then () else ab)) '(0 0 0 0 0)) 'syntax-error)
(test 293 (interpret-OSIL2 '((if 0 then () else ab)) '(0 0 0 0 0)) 'syntax-error)
;47
(test 294 (interpret-OSIL2 '((while ab)) '(0 0 0 0 0)) 'syntax-error)
(test 295 (interpret-OSIL2 '((while ab do)) '(0 0 0 0 0)) 'syntax-error)
(test 296 (interpret-OSIL2 '((while 1 do ab)) '(0 0 0 0 0)) 'syntax-error)
(test 297 (interpret-OSIL2 '((while 1)) '(0 0 0 0 0)) 'syntax-error)
(test 298 (interpret-OSIL2 '((while 0)) '(0 0 0 0 0)) 'syntax-error)
(test 299 (interpret-OSIL2 '((while 0 do ab)) '(0 0 0 0 0)) 'syntax-error)
(test 300 (interpret-OSIL2 '((a := (0 - 2))) '(0 0 0 0 0)) 'syntax-error)

(newline)
