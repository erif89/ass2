;;; Advanced Functional Programming, Assignment 2 - Lisp
;;; Authors:
;;; - Carl Carenvall <caca7037@student.uu.se>
;;; - Emil Wall <emwa3503@student.uu.se>


;;
;; Dictionary structure with purely functional lookup and update functions
;;
;; A dictionary of type treedict is a binary search tree with an ordering
;; function (cmp) and a root treenode (tree), defined below. The cmp function
;; is used for node key comparison and should output LT for less than,
;; T for equal and GT for greater than.
;;
(defstruct treedict tree cmp)

;;
;; A treenode is a key-value store with two children treenodes. Leaf children
;; are represented as nil.
;;
(defstruct treenode key value left right)

;;
;; Comparison function for reals
;;
(defun intcompare (a b)
    "Returns LT for less than, T for equal and GT for greater than"
    (cond
        ((< a b) 'LT)
        ((= a b) T)
        ((> a b) 'GT)))

;;
;; Comparison function for strings, used as default by create-dictionary
;;
(defun strcompare (a b)
    "Returns LT for less than, T for equal and GT for greater than"
    (cond
        ((string< a b) 'LT)
        ((string= a b) T)
        ((string> a b) 'GT)))

;;
;; Creates a new empty dictionary with compare being the ordering function to
;; be used for key comparisons. If compare is not given, strcompare is used.
;;
(defun create-dictionary (&key compare)
    "Returns the empty dict with compare (or strcompare) as ordering function"
    (if (null compare)
        (make-treedict :tree (make-treenode) :cmp #'strcompare)
        (make-treedict :tree (make-treenode) :cmp compare)))

;;
;; Finds value that key is mapped to in dict, returns default if it does not
;; exist, or nil if no default value is given.
;;
(defun lookup (key dict &key default)
    "Returns dict[key], or default/nil if no such value exists"
    (let
        ((tree (treedict-tree dict))
         (cmp (treedict-cmp dict)))
        (lookuphelper key tree :default default :cmp cmp)))

;;
;; Help function for lookup
;;
(defun lookuphelper (key node &key default cmp)
    "Returns value associated with key in node subtree, or default/nil"
    (let
        ((key2 (treenode-key node))
         (value (treenode-value node))
         (left (treenode-left node))
         (right (treenode-right node)))
        (cond
            ((null key2)
                default)
            ((eq (funcall cmp key key2) 'T)
                value)
            ((eq (funcall cmp key key2) 'LT)
                (if left (lookup key left :key default) default))
            ((eq (funcall cmp key key2) 'GT)
                (if right (lookup key right :key default) default)))))

;;
;; Creates a new dictionary where key maps to value, regardless of if it
;; was present before.
;;
(defun update (key value dict &key cmp)
    "Returns dict with (key, value) destructively inserted"
    (let
        ((key2 (treedict-key dict))
         (value2 (treedict-value dict))
         (left (treedict-left dict))
         (right (treedict-right dict))
         (cmp (treedict-cmp dict)))
        (cond
            ((null key2)                                ; Empty tree
                (make-treedict :key key :value value :cmp cmp))
            ((eq (funcall cmp key key2) 'T)             ; Keys match
                (make-treedict :key key :value value
                 :left left :right right :cmp cmp))
            ((eq (funcall cmp key key2) 'LT)            ; Update left subtree
                (if left
                    (make-treedict :key key2 :value value2 :right right
                    :left (update key value right) :cmp cmp)
                    (make-treedict :key key2 :value value2 :right right
                     :left (make-treedict :key key :value value :cmp cmp)
                     :cmp cmp)))
            ((eq (funcall cmp key key2) 'GT)            ; Update right subtree
                (if right
                    (make-treedict :key key2 :value value2 :left left
                     :right (update key value right) :cmp cmp)
                    (make-treedict :key key2 :value value2 :left left
                     :right (make-treedict :key key :value value :cmp cmp)
                     :cmp cmp))))))

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
;; intermediate data structures are constructed. samekeys should use the
;; compare function, which should be the same for the two dictionaries and can
;; be assumed to be reflexive for its two arguments.
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



    
;;
;; Testfunction
;;

(defun test-dict ()
    (cond
        ( (not (compare -1 1) 'LT) (format t "compare less than failed (-1 1)") )
    )
)