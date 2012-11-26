;;; Advanced Functional Programming, Assignment 2 - Lisp
;;; Authors:
;;; - Carl Carenvall <caca7037@student.uu.se>
;;; - Emil Wall <emwa3503@student.uu.se>


;;
;; Dictionary structure with purely functional lookup and update functions
;;
;; A dictionary of type mydict is a binary search tree node with an ordering
;; function (cmp) a key-value store and two children nodes. The cmp function
;; should output 'LT for less than, T for equal and 'GT for greater than. Leaf
;; children are represented as nil.
;;
(defstruct mydict key value left right cmp)

;;
;; Comparison function used as default by create-dictionary
;;
(defun compare (a b)
    "Returns LT for less than, T for equal and GT for greater than"
    (cond
        ((< a b) 'LT)
        ((= a b) T)
        ((> a b) 'GT)))

;;
;; Creates a new empty dictionary with test being the ordering function to be
;; used for keys. If test is not given, it is defined by <, = and > (compare).
;;
(defun create-dictionary (&key test)
    "Returns the empty dict with test (or compare) as ordering function"
    (if (eq test nil) ; then
        (make-mydict :cmp #'compare) ; else
        (make-mydict :cmp test)))

;;
;; Finds value that key is mapped to in dict, returns default if it does not
;; exist, or nil if no default value is given.
;;
(defun lookup (key dict &key default)
    "Returns dict[key], or default/nil if no such value exists"
    (let
        ((key2 (mydict-key dict))
         (value (mydict-value dict))
         (left (mydict-left dict))
         (right (mydict-right dict))
         (cmp (mydict-test dict)))
        (cond
            ((eq (apply cmp '(key key2)) 'LT)
                (if left (lookup key left :key default) default))
            ((eq (apply cmp '(key key2)) 'T)
                value)
            ((eq (apply cmp '(key key2)) 'GT)
                (if right (lookup key right :key default) default)))))

;;
;; Creates a new dictionary where key maps to value, regardless of if it
;; was present before.
;;
(defun update (key value dict)
    "Returns dict with (key, value) destructively inserted"
    nil)  ; TODO implement
    ;(setf (GETHASH key dict) value))

;;
;; fold the key-value pairs of the dictionary using the function fun, which
;; should take three arguments: key, value and sofar (in this order) - the
;; accumulated value so far. initial is the initial value for sofar. The order
;; of application is (or at least should be) irrelevant.
;;
(defun fold (fun dict initial)
    "Returns fun(k1, v2, fun(k2, v2, ...initial...)) for key-values in dict"
    nil)  ; TODO implement

;;
;; Returns a new dictionary that is more balanced (if needed).
;; This could be run when needed as part of update as well.
;;
(defun rebalance (dict)
    "Returns dict with depth of each child differing with at most 1."
    nil)  ; TODO implement

;;
;; Returns the keys of the dictionary in a list.
;; The order of the keys is not relevant.
;; 
(defun keys (dict)
    "Returns list of keys in dict"
    nil)  ; TODO implement

;;
;; Determines if dict1 and dict2 contain the same set of keys.
;; Care has been taken to make it efficient, i.e., no unnecessary large
;; intermediate data structures are constructed. samekeys should use the test
;; function, which should be the same for the two dictionaries and can be
;; assumed to be reflexive for its two arguments.
;;
(defun samekeys (dict1 dict2)
    "Returns T if dict1 has the same keys as dict2, nil otherwise"
    nil)  ; TODO implement

;;
;; Evaluates body once for each key-value pair in dict. key and value are
;; bound to each key-value pair in dict.
;;
(defmacro with-keys ((key value dict) body)
    "Returns result of evaluating body on each key-value pair in dict"
    nil)  ; TODO implement

;;
;; Evaluates expr and then tries to match the result against pattern_i.
;; If it succeeds body_i is evaluated with the free variables in pattern_i,
;; bound during the evaluation. A pattern is a general S-expression,
;; where a symbol is a variable and can match anything. A quoted structure
;; must match exactly (and thus contains no variables).
;;
;; Note that a variable can occur several times in a pattern and must then
;; have the same value. Since 'sexpr expands to (quote sexpr), the symbol
;; quote can not be used as a variable. This is ok, as is disallowing t
;; and nil as variables.
;;
(defmacro match-pattern (expr &rest patternlist)  ; TODO list arguments as (expr ((pattern_1 body_1) (pattern_2 body_2) .. (pattern_n body_n)))
    "Returns result of evaluationg the first body with pattern matching expr"
    nil)  ; TODO implement


