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
