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
    (if (= 2 (length match-result))
        (let* ((expr (first match-result))
               (do (last match-result)))
          (parse-while-help expr do state))
        'syntax-error)))
 
(define (parse-statement statement state)
  (cond ((atom? statement) 'syntax-error)
        ((null? statement) state)
        (else
         (let ((x (first statement)))
           (cond ((list? x) ; handle blocks
                  (parse-statement (rest statement)
                                   (parse-statement x state)))
                 ((variable? x)
                  (parse-assign statement state))
                 ((eq? x 'if)
                  (parse-if statement state))
                 ((eq? x 'while)
                  (parse-while statement state))
                 (else 'syntax-error))))))

(define (interpret-OSIL2 program state)
  (if (and (list? program) (list? (first program)))
      (parse-statement program state)
      'syntax-error))

;;; for testing purposes
(define (test program state) (interpret-OSIL2 program state))
 
;;; assign
.a := 1..(assign 'a 1 '(0 0 0 0 0)).'(1 0 0 0 0).(1 0 0 0 0).e := 1..(assign 'e 1 '(0 0 0 0 0)).'(0 0 0 0 1).(0 0 0 0 1)

;;; parse-operand
.a..(parse-operand 'a '(0 1 2 3 4)).0.0.e..(parse-operand 'e '(0 1 2 3 4)).4.4.12..(parse-operand 12 '(0 1 2 3 4)).12.12.-14..(parse-operand -14 '(0 1 2 3 4)).-14.-14

;;; parse-expr
.10..(parse-expr '(10) '(0 1 2 3 4)).10.10.a..(parse-expr '(a) '(0 1 2 3 4)).0.0.10 + b - 5 + a..(parse-expr '(10 + b - 5 + a) '(0 1 2 3 4)).6.6.10 + ab..(parse-expr '(10 + ab) '(0 1 2 3 4)).'syntax-error.syntax-error.10 + b - 5 + a..(parse-expr '(10 * a) '(0 1 2 3 4)).'syntax-error.syntax-error.ab..(parse-expr '(ab) '(0 1 2 3 4)).'syntax-error.syntax-error.10 +..(parse-expr '(10 +) '(0 1 2 3 4)).'syntax-error.syntax-error

;;; parse-assign
.a := b + 10 + c..(parse-assign '(a := b + 10 + c) '(0 1 2 3 4)).'(13 1 2 3 4).(13 1 2 3 4).a := ab..(parse-assign '(a := ab) '(0 1 2 3 4)).'syntax-error.syntax-error.a..(parse-assign '(a) '(0 1 2 3 4)).'syntax-error.syntax-error

;;; parse-if
.if 1 then a := 1 else b := 0..(parse-if '(if 1 then (a := 1) else (b := 0)) '(0 1 2 3 4)).'(1 1 2 3 4).(1 1 2 3 4).if 1 then a := 1 else b := 0..(parse-if '(if -1 then (a := 1) else (b := 0)) '(0 1 2 3 4)).'(0 0 2 3 4).(0 0 2 3 4).if ab then a := 1 else b := 0..(parse-if '(if ab then (a := 1) else (b := 0)) '(0 1 2 3 4)).'syntax-error.syntax-error.if..(parse-if '(if) '(0 1 2 3 4)).'syntax-error.syntax-error.if then..(parse-if '(if then) '(0 1 2 3 4)).'syntax-error.syntax-error.if then else..(parse-if '(if then else) '(0 1 2 3 4)).'syntax-error.syntax-error.if a then (a := 1) else..(parse-if '(if a then (a := 1)) '(0 1 2 3 4)).'syntax-error.syntax-error.if then a:=1 else b := 2..(parse-if '(if then (a := 1) else (b := 2)) '(0 1 2 3 4)).'syntax-error.syntax-error.if 1 then a else b := 0..(parse-if '(if 1 then) '(0 1 2 3 4)).'syntax-error.syntax-error.if -1 then a := 1 else b..(parse-if '(if -1 then (a := 1) else (b)) '(0 1 2 3 4)).'syntax-error.syntax-error

;;; parse-while
.while e do e := e - 1..(parse-while '(while e do (e := e - 1)) '(0 1 2 3 4)).'(0 1 2 3 0).(0 1 2 3 0).while 1 do a..(parse-while '(while 1 do (a)) '(0 1 2 3 4)).'syntax-error.syntax-error.while -1 do a..(parse-while '(while =1 do (a)) '(0 1 2 3 4)).'syntax-error.syntax-error.while ab do e := e - 1..(parse-while '(while ab do (e := e - 1)) '(0 1 2 3 4)).'syntax-error.syntax-error.while a do a..(parse-while '(while a do (a)) '(0 1 2 3 4)).'syntax-error.syntax-error.while..(parse-while '(while) '(0 1 2 3 4)).'syntax-error.syntax-error.while do..(parse-while '(while do) '(0 1 2 3 4)).'syntax-error.syntax-error.while a do..(parse-while '(while a do) '(0 1 2 3 4)).'syntax-error.syntax-error.while do b := 0..(parse-while '(while do b := 0) '(0 1 2 3 4)).'syntax-error.syntax-error

;;; parse-statement
.a..(parse-statement 'a '(0 1 2 3 4)).'syntax-error.syntax-error.skip..(parse-statement '() '(0 1 2 3 4)).'(0 1 2 3 4).(0 1 2 3 4).a := 1, b := 0..(parse-statement '((a := 1) (b := 0)) '(0 1 2 3 4)).'(1 0 2 3 4).(1 0 2 3 4).a := 1, b := 0..(parse-statement '((a := 1) (ab)) '(0 1 2 3 4)).'syntax-error.syntax-error

;;; programs from project spec
.program1..(interpret-OSIL2 '(
    ( a := 5 )
    ( b := 0 )
    ( while a do
        (
            ( b := b + a )
            ( a := a - 1 )
        )))
      '(0 0 0 0 0)).'(0 15 0 0 0).(0 15 0 0 0)
;23
.program2..(interpret-OSIL2 '(
    ( a := 1 )
    ( c := 0 )
    ( e := 0 )
    ( while 10 - a do
        (
            ( if e then
                (
                    ( c := c + 1 )
                    ( e := 0 - 1 )
                )
              else
                ( )
            )
            ( a := a + 1 )
            ( e := e + 1 ))))
      '(0 0 0 0 0)).'(10 0 4 0 1).(10 0 4 0 1)
;28
.error checking in block statement, 1..(interpret-OSIL2 '((a := 0) b) '(0 1 2 3 4)).'syntax-error.syntax-error
;30
.error checking in assignment statement, 1..(interpret-OSIL2 '((a := 10ab)) '(0 1 2 3 4)).'syntax-error.syntax-error
;33
.error checking in assignment statement, 2..(interpret-OSIL2 '((a :=)) '(0 1 2 3 4)).'syntax-error.syntax-error
;34
.error checking in assignment statement, 3..(interpret-OSIL2 '((a := 10 * b)) '(0 1 2 3 4)).'syntax-error.syntax-error
;35
.error checking in assignment statement, 4..(interpret-OSIL2 '((a := 10 +)) '(0 1 2 3 4)).'syntax-error.syntax-error
;36
.error checking in if statement, 1..(interpret-OSIL2 '((if then else)) '(0 1 2 3 4)).'syntax-error.syntax-error
;37
.error checking in if statement, 2..(interpret-OSIL2 '((if 1 then (a := 0) else (b))) '(0 1 2 3 4)).'syntax-error.syntax-error
;38
.error checking in if statement, 3..(interpret-OSIL2 '((if -1 then (a) else (b := 1))) '(0 1 2 3 4)).'syntax-error.syntax-error
;39
.from the forum..(interpret-OSIL2 '((m := 1)) '(0 1 2 3 4)).'syntax-error.syntax-error
;40
.error checking in if statement, 4..(interpret-OSIL2 '(if) '(0 1 2 3 4)).'syntax-error.syntax-error
.error checking in if statement, 5..(interpret-OSIL2 '(if then) '(0 1 2 3 4)).'syntax-error.syntax-error
.error checking in if statement, 6..(interpret-OSIL2 '(if 1 then (a) else (b := 1)) '(0 1 2 3 4)).'syntax-error.syntax-error
.high-level..(interpret-OSIL2 '(a := 1) '(0 1 2 3 4)).'syntax-error.syntax-error

