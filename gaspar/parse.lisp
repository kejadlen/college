;; Load grammar
;(load "grammar.lisp")

;; This commented out block was code used for testing. It also shows the format
;; the grammar must use. This is equivalent to the following grammar:
"
	<s> = when did <name> last email me
		==> <name> lasted emailed you on \"print date of last email or never\"
	<s> = what did <name> say on <date>
		==> <name> said \"dump email contents or print did not email\"
	<s> = what did <name> attach on <date>
		==> <name> attached \"print file name or nothing\"
	<s> = who else did <name> email on <date>
		==> <name> also emailed \"print to: and cc: list from header or none\"
	<s> = what account did <name> email from on <date>
		==> <name> emailed from \"print email address from header or none\"
	<name> = {any valid string}
	<date> = {monday, tuesday, wednesday, thursday, friday, saturday, sunday}
	<date> = <month> <day>
	<month> = {jan, march, april, may, june, july, aug, sept, oct, nov, dec}
	<day> = {1,2,3,4,5,6,7,8,9,10,11, ..., 29,30,31}
"
(defvar *grammar* '(
		(s (when did (? name) last email me)
			((name) "last emailed you on \"print date of last email or never\""))
		(s (what did (? name) say on (? date))
			((name) said \"dump email contents or print did not email\"))
		(s (what did (? name) attach on (? date))
			((name) attached \"print file name or nothing\"))
		(s (who else did (? name) email on (? date))
			((name) "also emailed \"print to: and cc: list from header or none\""))
		(s (what account did (? name) email from on (? date))
			((name) emailed from \"print email address from header or did not email\"))
		(name nil) ; matches any single word
		;; The date variable can also be represented in one list:
		;; (date ((monday) (tuesday) (wednesday) ((? month) (? day))))
		(date ((monday) (tuesday) (wednesday))) ; matches from a set of phrases
		(date (((? month) (? day)))) ; a single phrase can include multiple variables
		(month ((jan) (march) (april) (may)))
		(day ((1) (2) (3) (4) (5) (6) (7)))
		))

;; Set the program to output in lowercase, because it's nicer than uppercase.
(setf *print-case* :downcase)

(defun assistant ()
	"A phrase is entered at the ?> prompt. This phrase is sent to be parsed and
	an indicated action is outputed. The loop terminates on receiving an (exit)
	as input from the user."
	(format t "?> ")
	(loop for input = (read) until (equal input '(exit)) do 
		(parse-and-output input)
		(format t "~%?> "))
	(values))

(defun parse-and-output (input)
	"Passes the results from parsing to be printed."
	(multiple-value-call #'print-result (parse-sentence input)))

(defun parse-sentence (input)
	"Tests the input on each sentence in the grammar. On receiving a successful
	parse, returns the result. If no matches are found, then nil nil is returned."
	(loop with (result) for i in *grammar* do
		(if (equal (first i) 's) ; match sentences in grammar
			(if (and
					(setf result (parse-phrase (second i) input))
					(null (second result))) ; make sure the entire input has matched
				(return-from parse-sentence (values (first result) (third i))))))
	(values nil nil))

(defun parse-phrase (seeking input)
	"Iterates through each symbol in SEEKING, matching against each word in INPUT.
	On mismatches, returns nil. Otherwise, returns a list of what has been matched
	and the remaining INPUT. Once a symbol is matched or a word is used, SEEKING
	and INPUT are appropriately changed.
	
	To do: return using values instead of a list"
	(let ((seen nil)) ; holds the matched symbols and words
		(loop for i in seeking do
			(let ((current-symbol (first seeking))
						(current-word (first input))
						(result nil))
				(setf seeking (rest seeking)) ; don't need the current symbol anymore
				(cond
					((atom current-symbol)
						(setf input (rest input)) ; don't need the current word anymore
						(if (equal current-symbol current-word) 
							(setf seen (append seen (list current-word)))
							(return (setf seen nil))))
					((varp current-symbol)
						(setf result
							;; This loop iterates through the grammar for the state in the
							;; CURRENT-SYMBOL and matches against those states. The match for
							;; the CURRENT-SYMBOL is returned if it exists. Otherwise, nil is
							;; returned.
							(loop with (x) for j in *grammar* thereis
								(if (equal (first j) (second current-symbol)) ; only match the state in CURRENT-SYMBOL
									;; A nil in the state means that any word can be matched
									;; against the CURRENT-SYMBOL.
									(if (null (second j))
										(return (list (list current-word) (rest input)))
										;; Try matching against the available options for the
										;; CURRENT-SYMBOL.
										(loop for k in (second j) thereis
											(parse-phrase k input))))))
						(if result
							;; HACK -- add matched words to SEEN and set the INPUT to what's
							;; left in the input
							(and
								(setf seen (append seen (list (append (rest current-symbol) (first result)))))
								(setf input (second result)))
							(return (setf seen nil)))))))
		(if seen (list seen input) nil)))

(defun print-result (result output)
	"Prints the output, replacing the variables with the matched variables in
	RESULT. If result is nil, then nothing was matched against the input and a
	syntax error message is printed. Recursively prints one word from the output
	at a time."
	(cond
		((null result) (format t "syntax error"))
		(T
			(let ((current (first output)))
				(cond
					((null current) nil) ; nothing else to print -- might not be needed
					((listp current) (print-var (first current) result)) ; print the indicated variable
					((atom current) (princ current)))) ; print the word
			(princ " ")
			(if output (print-result result (rest output)))))) ; print the rest of the output

(defun print-var (var result)
	"Goes through RESULT to find VAR, printing the words which match VAR."
	(if result
		(let ((current (first result)))
			(cond
				((and (listp current) (equal (first current) var)) ; find variable VAR
					(print-flattened-var (rest current))) ; found, so print VAR 
				((listp current) ; check inside the variable
					(if (not (print-var var current)) (print-var var (rest result))))
				(T (print-var var (rest result))))))) ; not found, look through the rest of RESULT

(defun print-flattened-var (var)
	"Print the words in a variable."
	(let ((current (first var)))
		(if (listp current)
			(print-flattened-var (rest current)) ; variable contains further variables - print those
			(princ current))
		(if (rest var)
			(and (princ " ") (print-flattened-var (rest var)))))) ; there is more to the variable to print

(defun varp (var)
	"Returns T if VAR is a variable."
	(and (listp var) (equal (first var) '?)))
