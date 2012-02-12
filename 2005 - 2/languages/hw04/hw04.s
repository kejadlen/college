;;; 1. Chapter 15, Problem 6
;;; This function returns lis if s is a top-level member of lis, and the empty set otherwise.

;;; 2. Chapter 15, Programming Exercise 13
;;; 3.
(define (my-sublist lst start end)
  (cond ((> start 0) (my-sublist (rest lst) (- start 1) (- end 1)))
        ((= start 0) (my-sublist (reverse lst) -1 (- end 1)))
        ((> (- (length lst) end) 0) (my-sublist (rest lst) -1 (+ end 1)))
        (else (reverse lst))))

;;; 4. The worst case scenario is O(n), or linear with the number of elements in the list. This is because my-sublist cannot perform any more cdrs (rests) than there are elements in the array. Worst case scenarios involve reducing the list to one element (since it acts inclusively on start and exclusively on end), so only (- (length lst) 1) rest operations can be performed.

;;; Although reverse may use car or cdr, I am not sure if it does, or how many it uses. I believe reverse should be O(n) anyway, as it can be accomplished by using an inject (fold-right?) method. Even if intermediate student Scheme doesn't support it, I imagine that behind the scenes, it would be performed that way. But even if it is O(n), my-sublist would still only be O(2n).
