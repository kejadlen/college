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
(define (parse-assign statement state side-effect)
  (let ((match-result (match '(:=) statement)))
    (if (= 2 (length match-result))
        (let* ((var (caar match-result))
               (expr (cadr match-result))
               (val (parse-expr expr state)))
          (if (integer? val)
              (if side-effect (assign var val state) state)
              'syntax-error))
    'syntax-error)))

;;; <if> -> \( if <expression> then <statement> else <statement> \)
(define (parse-then-else val then else state side-effect)
  ;; parses the then and passes the state to the else
  (let ((then-state (parse-statement then state (and side-effect (> val 0)))))
    (if (atom? then-state)
        'syntax-error
        (parse-statement
         else
         then-state
         (and side-effect (<= val 0))))))

(define (parse-if statement state side-effect)
  (let ((match-result (match '(if then else) statement)))
    (if (= 3 (length match-result))
        (let* ((expr (first match-result))
               (then (second match-result))
               (else (last match-result))
               (val (parse-expr expr state)))
        (if (integer? val)
            (parse-then-else val then else state side-effect)
            'syntax-error))
        'syntax-error)))

;;; <while> -> \( while <expression> do <statement> \)
(define (parse-while-help expr do state side-effect)
  (let ((val (parse-expr expr state))
        (new-state (parse-statement do state side-effect)))
    (cond ((or (not (integer? val)) (atom? new-state))
           'syntax-error)
          ((> val 0) (parse-while-help expr
                                       do
                                       new-state
                                       side-effect))
          (else state))))

(define (parse-while statement state side-effect)
  (let ((match-result (match '(while do) statement)))
    (if (= 2 (length match-result))
        (let* ((expr (first match-result))
               (do (last match-result)))
          (parse-while-help expr do state side-effect))
        'syntax-error)))
 
(define (parse-statement statement state side-effect)
  (cond ((atom? statement) 'syntax-error)
        ((null? statement) state)
        (else
         (let ((x (first statement)))
           (cond ((list? x) ; handle blocks
                  (parse-statement (rest statement)
                                   (parse-statement x state side-effect)
                                   side-effect))
                 ((variable? x)
                  (parse-assign statement state side-effect))
                 ((eq? x 'if)
                  (parse-if statement state side-effect))
                 ((eq? x 'while)
                  (parse-while statement state side-effect))
                 (else 'syntax-error))))))

(define (interpret-OSIL2 program state)
  (parse-statement program state #t))

;;; for testing purposes
(define (test program state) (interpret-OSIL2 program state))

;;; assign
'(1 0 0 0 0)Should be(assign 'a 1 '(0 0 0 0 0))Test...'(0 0 0 0 1)Should be(assign 'e 1 '(0 0 0 0 0))Test...'(0 0 1 0 0)Should be(assign 'c 1 '(0 0 0 0 0))Test...
 
;;; parse-operand
0Should be(parse-operand 'a '(0 1 2 3 4))Test...4Should be(parse-operand 'e '(0 1 2 3 4))Test...12Should be(parse-operand '12 '(0 1 2 3 4))Test...-14Should be(parse-operand '-14 '(0 1 2 3 4))Test...

;;; parse-expr
10Should be(parse-expr '(10) '(0 1 2 3 4))Test...0Should be(parse-expr '(a) '(0 1 2 3 4))Test...6Should be(parse-expr '(10 + b - 5 + a) '(0 1 2 3 4))Test...'syntax-errorShould be(parse-expr '(10 + ab) '(0 1 2 3 4))Test...'syntax-errorShould be(parse-expr '(10 * a) '(0 1 2 3 4))Test...'syntax-errorShould be(parse-expr '(ab) '(0 1 2 3 4))Test...'syntax-errorShould be(parse-expr '(10 +) '(0 1 2 3 4))Test...

;;; parse-assign
'(13 1 2 3 4)Should be(parse-assign '(a := b + 10 + c) '(0 1 2 3 4) #t)Test...'(0 1 2 3 4)Should be(parse-assign '(a := b + 10 + c) '(0 1 2 3 4) #f)Test...'syntax-errorShould be(parse-assign '(a := ab) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-assign '(a) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-assign '(a) '(0 1 2 3 4) #t)Test...

;;; parse-if
'(1 1 2 3 4)Should be(parse-if '(if 1 then (a := 1) else (b := 0)) '(0 1 2 3 4) #t)Test...'(0 0 2 3 4)Should be(parse-if '(if -1 then (a := 1) else (b := 0)) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-if '(if ab then (a := 1) else (b := 0)) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-if '(if) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-if '(if then) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-if '(if then else) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-if '(if a then (a := 1) else) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-if '(if then (a := 1) else (b := 2)) '(0 1 2 3 4) #t)Test...
'syntax-errorShould be(parse-if '(if 1 then (a) else (b := 0)) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-if '(if -1 then (a) else (b := 0)) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-if '(if 1 then (a := 1) else (b)) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-if '(if -1 then (a := 1) else (b)) '(0 1 2 3 4) #t)Test...

;;; parse-while
'(0 1 2 3 0)Should be(parse-while '(while e do (e := e - 1)) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-while '(while 1 do (a)) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-while '(while -1 do (a)) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-while '(while ab do (e := e - 1)) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-while '(while a do (a)) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-while '(while) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-while '(while do) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-while '(while a do) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-while '(while do (b := 0)) '(0 1 2 3 4) #t)Test...

;;; parse-statement
'syntax-errorShould be(parse-statement 'a '(0 1 2 3 4) #t)Test...'(0 1 2 3 4)Should be(parse-statement '() '(0 1 2 3 4) #t)Test...'(1 0 2 3 4)Should be(parse-statement '((a := 1) (b := 0)) '(0 1 2 3 4) #t)Test...'syntax-errorShould be(parse-statement '((a := 1) (ab)) '(0 1 2 3 4) #t)Test...

;;; programs from project spec
'(0 15 0 0 0)Should be(test '(
    ( a := 5 )
    ( b := 0 )
    ( while a do
        (
            ( b := b + a )
            ( a := a - 1 )
        )))
      '(0 0 0 0 0))
Test...'(10 0 4 0 1)Should be(test '(
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
      '(0 0 0 0 0))Test...