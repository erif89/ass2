;;; Advanced Functional Programming, Assignment 2 - Lisp
;;; Authors:
;;; - Carl Carenvall <caca7037@student.uu.se>
;;; - Emil Wall <emwa3503@student.uu.se>
;;;
;;; To run, you need lisp-unit.lisp. Here's the procedure:
;;; 1. (load "lisp-unit.lisp")
;;; 2. (use-package :lisp-unit)
;;; 3. (load "assignment2.lisp")
;;; 4. (run-tests)
;;;
;;; Note that you need to restart the REPL if you accidently loaded
;;; assignment2.lisp before step 1 or 2.
;;;


;;
;; Dictionary with purely functional lookup and update functions.
;;
;; A dictionary is a binary search tree with an ordering function (cmp)
;; in the root node. The cmp function is used for node key comparison and
;; should output LT for less than, EQ for equal and GT for greater than.
;;
;; A dictionary root node is represented as '(key value left right size #'cmp)
;; whereas all the children nodes are either nil (empty) or
;; '(key value left right size), with size >= 1.
;;
;; An empty dictionary has size 0, and the key, value, left and right items are
;; undefined but present, possibly as nil.
;;

;;
;; Comparison function for strings, used as default by create-dictionary
;;
(defun strcompare (a b)
  "Returns LT for less than, T for equal and GT for greater than"
  (cond
    ((string< a b) 'LT)
    ((string= a b) 'EQ)
    ((string> a b) 'GT)))

;;
;; Comparison function for numbers
;;
(defun numcompare (a b)
  "Returns LT for less than, T for equal and GT for greater than"
  (cond
    ((< a b) 'LT)
    ((= a b) 'EQ)
    ((> a b) 'GT)))

;;
;; Creates a new empty dictionary with compare being the ordering function to
;; be used for key comparisons. If compare is not given, strcompare is used.
;;
(defun create-dictionary (&key compare)
  "Returns the empty dict with compare (or strcompare) as ordering function"
  (if (null compare)
      (list nil nil nil nil 0 #'strcompare)
      (list nil nil nil nil 0 compare)))

(defun isempty-dictionary (dict)
  (or (null dict) (= 0 (fifth dict))))

(defun isroot-dictionary (dict)
  (functionp (sixth (dict))))

;;
;; Finds value that key is mapped to in dict, returns default if it does not
;; exist, or nil if no default value is given.
;;
(defun lookup (key dict &key default)
  "Returns dict[key], or default/nil if no such value exists"
  (if (isempty-dictionary dict)
      default
      (lookuphelper key dict default (sixth dict))))

;;
;; Help function for lookup, recurse over the tree in search of key.
;;
(defun lookuphelper (key node default cmp)
  "Returns value associated with key in node subtree, or default/nil"
  (let ((key2 (first node))
        (value (second node))
        (left (third node))
        (right (fourth node)))
    (cond
      ((null key2)
        default)
      ((eq (funcall cmp key key2) 'EQ)  ; TODO use case or let here
        value)
      ((eq (funcall cmp key key2) 'LT)
        (if left (lookuphelper key left default cmp) default))
      ((eq (funcall cmp key key2) 'GT)
        (if right (lookuphelper key right default cmp) default)))))

;;
;; Creates a new dictionary where key maps to value, regardless of if it
;; was present before.
;;
(defun update (key value dict)
  "Returns dict with (key, value) destructively inserted"
    `(,@(if (isempty-dictionary dict)
            (list key value (third dict) (fourth dict) 1)
            (updatehelper key value dict (sixth dict)))
      ,(sixth dict)))

;;
;; Help function for update. cmp is used for key comparisons in the key.
;;
(defun updatehelper (key value node cmp)
  "Returns node with (key, value) destructively inserted"
  (let ((key2 (first node))
        (value2 (second node))
        (left (third node))
        (right (fourth node))
        (size (fifth node)))
    (cond  ; TODO use case or let here
      ((eq (funcall cmp key key2) 'EQ)       ; Keys match
        `(,key ,value ,left ,right ,size))
      ((eq (funcall cmp key key2) 'LT)      ; Update left subtree
        (list key2 value2
          (if (isempty-dictionary left) ; End of recursion if subtree is empty
              (list key value nil nil 1)
              (updatehelper key value left cmp))
          right
          (+ size 1)))
      ((eq (funcall cmp key key2) 'GT)      ; Update right subtree
        (list key2 value2
          left
          (if (isempty-dictionary right)
              (list key value nil nil 1)
              (updatehelper key value right cmp))
          (+ size 1))))))

;;
;; fold the key-value pairs of the dictionary using the function fun, which
;; should take three arguments: key, value and sofar (in this order) - the
;; accumulated value so far. initial is the initial value for sofar. The order
;; of application is (or at least should be) irrelevant.
;;
;; Applies fun to key-values in preorder walk of dict.
;;
(defun fold (fun dict initial)
  "Returns fun(k1, v2, fun(k2, v2, ...initial...)) for key-values in dict"
  (if (> (fifth dict) 0)
      (let ((left (third dict))
            (right (fourth dict))
            (res (funcall fun (first dict) (second dict) initial)))
        (cond
          ((and left right) (fold fun right (fold fun left res)))
          (left (fold fun left res))
          (right (fold fun right res))
          (t res)))
      initial))

;;
;; Returns a new dictionary that is more balanced (if needed).
;; This could be run when needed as part of update as well.
;;
(defun rebalance (dict)
  "Returns dict with depth of each child differing with at most 1."
  (let ((tree (treedict-tree dict)))
    (if tree
        (make-treedict :tree (rebalancehelper tree) :cmp (treedict-cmp dict))
        dict)))

;;
;; Help function to rebalance. Returns node reordered to be "height-balanced".
;;
;; A well-formed binary tree is said to be "height-balanced" if (1) it is
;; empty, or (2) its left and right children are height-balanced and the
;; height of the left tree is within 1 of the height of the right tree.
;;
(defun rebalancehelper (node)
  (let ((key (treenode-key node))
        (value (treenode-value node))
        (size (treenode-size node))
        (left (treenode-left node))
        (right (treenode-right node)))
    (cond
      ((or (not (or left right)) (< size 3)) node) ; leaf
      ((not left) ; right skewed
        (rebalancehelper (rotate (make-treenode :key key :value value :size size
          :right (rebalancehelper right)) t)))
      ((not right) ; left skewed
        (rebalancehelper (rotate (make-treenode :key key :value value :size size
          :left (rebalancehelper left)) nil)))
      ((< (abs (- (log (treenode-size left) 2)
                  (log (treenode-size right) 2)))
           2) ; balanced
        (make-treenode :key key :value value :size size
          :left (rebalancehelper left) :right (rebalancehelper right)))
      (t (rebalancehelper (rotate
            (make-treenode :key key :value value :size size
              :left (rebalancehelper left) :right (rebalancehelper right))
          (< (treenode-size right) (treenode-size left)))))))) ; skewed

;;
;; Performs rotation of binary tree node.
;; Left if rotate-left, right otherwise.
;; Precondition: right child is not nil if rotate-left, and vice versa.
;;
(defun rotate (node rotate-left)
  (let ((left (if rotate-left
                  (third node)
                  (third node)))
        (right (if rotate-left
                  (third node)
                  (third node))))
    (let ((lsize (if left (fifth left) 0))
          (rleft (third right))
          (rright (third right)))
      (let ((newvalue (list (first node) (second node) left rleft
                        (+ (+ lsize 1) (if rleft (fifth rleft) 0)))))
        (list (first right) (second right) ; key, value
          (if rotate-left newvalue rright) ; left
          (if rotate-left rright newvalue) ; right
          (+                               ; size
            (+ lsize (if rleft (fifth rleft) 0))
            (+ (if rright (fifth rright) 0) 2)))))))

;;
;; Returns the keys of the dictionary in a list.
;; The order of the keys is not relevant.
;;
(defun keys (dict)
  "Returns list of keys in dict"
  (keyshelper (treedict-tree dict) nil))

;;
;; Help function for keys, recurse over the tree to build a list of all keys.
;;
(defun keyshelper (node acc)
  "Returns value associated with key in node subtree, or default/nil"
  (if (null node) acc
      (let ((key (treenode-key node))
            (left (treenode-left node))
            (right (treenode-right node)))
        (cons key  (keyshelper left (keyshelper right acc))))))

;;
;; Determines if dict1 and dict2 contain the same set of keys.
;; Care has been taken to make it efficient, i.e., no unnecessary large
;; intermediate data structures are constructed. samekeys should use the
;; compare function, which should be the same for the two dictionaries and can
;; be assumed to be reflexive for its two arguments.
;;
(defun samekeys (dict1 dict2)
  "Returns T if dict1 has the same keys as dict2, nil otherwise"
  (let ((root1 (treedict-tree dict1))
        (root2 (treedict-tree dict2)))
    (if (or (null root1) (null root2))
        (and (null root1) (null root2))
        (and (= (treenode-size root1) (treenode-size root2))
             (samekeyshelper (treedict-cmp dict1)
               (buildstack root1 nil)
               (buildstack root2 nil))))))

;;
;; Used to get path to leftmost leaf of a treenode
;;
(defun buildstack (node stack)
  "Returns stack appended with the leftmost children of node"
  (if node (buildstack (treenode-left node) (cons node stack)) stack))

;;
;; Used by samekeyshelper, stack with its top element popped and possibly
;; the leftmost children of the top's right subtree added.
;;
(defun popnode (stack)
  "Returns stack with its top element popped and possibly new nodes added"
  (when stack
    (if (treenode-right (car stack))
        (buildstack (treenode-right (car stack)) (cdr stack))
        (cdr stack))))

;;
;; Help function for samekeys, compares keys using inorder walks.
;;
(defun samekeyshelper (cmp stack1 stack2)
  "Returns t if nodes and right subtrees in stacks have same keys, else nil"
  (let ((empty1 (null (car stack1)))
        (empty2 (null (car stack2))))
  (unless (or (and empty1 (car stack2)) (and (car stack1) empty2))
    (or (and empty1 empty2)
        (and (eq (funcall cmp (treenode-key(car stack1))
                              (treenode-key(car stack2))) 'EQ)
             (samekeyshelper cmp (popnode stack1) (popnode stack2)))))))

(defun allpairs (dict)
  "Returns list of key value pairs in dict"
  (allpairshelper (treedict-tree dict) nil))

;;
;; Help function for allpairs, recurse over the tree to build a list of all key
;; value pairs.
;;
(defun allpairshelper (node acc)
  "Returns value associated with key in node subtree, or default/nil"
  (if (null node) acc
      (let ((key (treenode-key node))
            (val (treenode-value node))
            (left (treenode-left node))
            (right (treenode-right node)))
        (cons (cons key (cons val nil))
              (allpairshelper left (allpairshelper right acc))))))

;;
;; Evaluates body once for each key-value pair in dict. key and value are
;; bound to each key-value pair in dict.
;;
(defmacro with-keys (args &rest body)
  "Returns result of evaluating body on each key-value pair in dict"
  (do ((pairs (allpairs (car(cddr args)))))
    `(loop for p in pairs do
      (let ((key (gensym))
            (value (gensym)))
      ` (do ((key ,(car p))
             (value ,(cdr p)))
          (body))))))

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
(defmacro match-pattern (expr &rest patternlist)
  "Returns result of evaluationg the first body with pattern matching expr"
  (cons 'cond (loop for p in patternlist collect
    (progn `((equal ,expr ,(list 'QUOTE (car p)))
             ,(cadr p))))))

;;
;; Test functions
;;

(define-test numcompare
  (assert-equal 'GT (numcompare 1 -1))
  (assert-equal 'GT (numcompare 12345 5432))
  (assert-equal 'EQ  (numcompare 1 1))
  (assert-equal 'EQ  (numcompare 12345 12345))
  (assert-equal 'LT (numcompare -1 1))
  (assert-equal 'LT (numcompare 5432 12345))
  (assert-equal 'EQ (numcompare 0 0))
  (assert-equal 'EQ (numcompare 0.0 0/1))
  (assert-equal 'LT (numcompare -3.14 -3.0))
  (assert-equal 'GT (numcompare 3.14 3.0))
  (assert-equal 'GT (numcompare 1/8 2/17))
  (assert-equal 'EQ (numcompare 1/8 0.125))
  (assert-equal 'EQ (numcompare 1e10 (+ 1e10 1))) ; limited float precision
  (assert-equal 'LT (numcompare 1e10 (+ 1e10 1000)))
  (assert-error 'error (numcompare 0 "")) ; "" is not a number
  (assert-error 'error (numcompare nil nil)) ; nil is not a number
  )

(define-test strcompare
  (assert-equal 'GT (strcompare "ABC" "AAA"))
  (assert-equal 'GT (strcompare "ZYX" "ONM"))
  (assert-equal 'EQ  (strcompare "ABC" "ABC"))
  (assert-equal 'EQ  (strcompare "TTT" "TTT"))
  (assert-equal 'LT (strcompare "AAA" "ABC"))
  (assert-equal 'LT (strcompare "ONM" "ZYX"))
  (assert-equal 'LT (strcompare "ABC" "abc"))
  (assert-equal 'EQ  (strcompare "" ""))
  (assert-equal 'LT (strcompare " A" "Qwerty"))
  (assert-equal 'GT (strcompare
                     "haveidonethislongenoughnoworisitagoodideatoaddmoretests"
                     "haveidonethislongenoughnoworisitagoodideatoaddmoretest"))
  (assert-equal 'LT (strcompare "" " "))
  (assert-equal 'GT (strcompare " " ""))
  (assert-equal 'GT (strcompare "_" " "))
  (assert-equal 'EQ (strcompare "  " "  "))
  (assert-equal 'EQ (strcompare "1234" "1234"))
  (assert-equal 'LT (strcompare "123" "1234"))
  (assert-equal 'GT (strcompare "321" "1234"))
  (assert-equal 'LT (strcompare "" ()))
  (assert-equal 'LT (strcompare "ABC" ()))
  (assert-equal 'GT (strcompare nil ""))
  (assert-equal 'LT (strcompare nil "Q"))
  (assert-error 'error (strcompare 0 "")) ; 0 is not a string
  )

(define-test create-dictionary
  (assert-true nil)
  ; (let ((dict )
        ; (dict2 )
        ; (dict3 (create-dictionary)))
    ; (assert-equal nil (treedict-tree (create-dictionary)))
    ; (assert-equal nil (treedict-tree
      ; (create-dictionary :compare #'numcompare)))
    ; (assert-equal #'numcompare (treedict-cmp
      ; (create-dictionary :compare #'numcompare)))
    ; (assert-equal #'strcompare (treedict-cmp (create-dictionary)))
    ; (assert-equal #'strcompare (treedict-cmp
      ; (create-dictionary :compare nil)))
  ; )
)

(define-test lookup
  (let ((dict `("2" "two"
                ("1" "one" nil nil 1)
                ("3" "three" nil nil 1)
                3 ,#'strcompare))
        (dict2 `("4" "four"
                 ("2" "two"
                   ("1" "one" nil nil 1)
                   ("3" "three" nil nil 1) 3)
                 ("5" "five"
                   nil
                   ("6" "six" nil nil 1) 2)
                 6 ,#'strcompare)))
    (assert-equal nil (lookup "1" `("2" "one" nil nil 1 ,#'strcompare)))
    (assert-equal nil (lookup "1" `("one" "1" nil nil 1 ,#'strcompare)))
    (assert-equal "one" (lookup "1" `("1" "one" nil nil 1 ,#'strcompare)))
    (assert-equal "one" (lookup "1" dict))
    (assert-equal "two" (lookup "2" dict))
    (assert-equal "three" (lookup "3" dict))
    (assert-equal "one" (lookup "1" dict2))
    (assert-equal "three" (lookup "3" dict2))
    (assert-equal "six" (lookup "6" dict2))
    (assert-equal "two" (lookup "2" dict2))
    (assert-equal "five" (lookup "5" dict2))
    (assert-equal "four" (lookup "4" dict2))
  )
)

(define-test update
  (let ((dict `("2" "two"
                ("1" "one" nil nil 1)
                ("3" "three" nil nil 1)
                3 ,#'strcompare))
        (dict2 `("4" "four"
                 ("2" "two"
                   ("1" "one" nil nil 1)
                   ("3" "three" nil nil 1) 3)
                 ("5" "five"
                   nil
                   ("6" "six" nil nil 1) 2)
                 6 ,#'strcompare)))
    (assert-equal
      (write-to-string (update "a" "b" (update "a" "b" (create-dictionary))))
      (write-to-string (update "a" "b" (create-dictionary))))
    (assert-equal
      (write-to-string dict)
      (write-to-string (update "3" "three" (update "1" "one"
        (update "2" "two" (create-dictionary))))))
    (assert-equal
      (write-to-string dict2)
      (write-to-string (update "3" "three" (update "1" "one" (update "2" "two"
        (update "6" "six" (update "5" "five"
        (update "4" "four" (create-dictionary)))))))))
  ; (assert-equal
    ; (write-to-string (make-treedict
     ; :tree (make-treenode :key 1 :value "one" :size 4
      ; :right (make-treenode :key 2 :value "two" :size 3
       ; :right (make-treenode :key 3 :value "three" :size 2
        ; :right (make-treenode :key 4 :value "four" :size 1))))
     ; :cmp #'numcompare))
    ; (write-to-string (update 4 "four" (update 3 "three" (update 2 "two"
     ; (update 1 "one" (create-dictionary :compare #'numcompare)))))))
  ; (assert-equal
    ; (write-to-string (make-treedict
     ; :tree (make-treenode :key 2 :value "two" :size 4
      ; :left (make-treenode :key 1 :value "one" :size 1)
      ; :right (make-treenode :key 3 :value "three" :size 2
       ; :right (make-treenode :key 4 :value "four" :size 1)))
     ; :cmp #'numcompare))
    ; (write-to-string (update 4 "four" (update 3 "three" (update 1 "one"
     ; (update 2 "two" (create-dictionary :compare #'numcompare)))))))
  ; (assert-equal
    ; (write-to-string (make-treedict
     ; :tree (make-treenode :key 3 :value "three" :size 4
      ; :left (make-treenode :key 1 :value "one" :size 2
       ; :right (make-treenode :key 2 :value "two" :size 1))
      ; :right (make-treenode :key 4 :value "four" :size 1))
     ; :cmp #'numcompare))
    ; (write-to-string (update 4 "four" (update 2 "two" (update 1 "one"
     ; (update 3 "three" (create-dictionary :compare #'numcompare)))))))
  )
)

(define-test create_with_numkey
  (let ((dict (create-dictionary :compare #'numcompare))
        (dict2 (update 1 "one" (update 2 "two"
          (create-dictionary :compare #'numcompare))))
        (dict3 (update 2 "two" (update 1 "one"
          (create-dictionary :compare #'numcompare))))
        (dict4 (update 3 "three" (update 2 "two" (update 1 "one"
          (create-dictionary :compare #'numcompare)))))
        (dict5 (update 4 "four" (update 3 "three" (update 2 "two"
          (update 1 "one" (create-dictionary :compare #'numcompare))))))
        (dict6 (update 2 "two" (update 3 "three" (update 4 "four"
          (update 1 "one" (create-dictionary :compare #'numcompare)))))))
    (assert-equal "one" (lookup 1 (update 1 "one" dict)))
    (assert-equal 0 (list-length (treedict-tree dict)))
    (assert-false (treedict-tree dict))
    (assert-equal "one" (lookup 1 dict2))
    (assert-equal "two" (lookup 2 dict2))
    (assert-equal "one" (lookup 1 dict3))
    (assert-equal "two" (lookup 2 dict3))
    (assert-equal "one" (lookup 1 dict4))
    (assert-equal "two" (lookup 2 dict4))
    (assert-equal "three" (lookup 3 dict4))
    (assert-equal "one" (lookup 1 dict5))
    (assert-equal "two" (lookup 2 dict5))
    (assert-equal "three" (lookup 3 dict5))
    (assert-equal "four" (lookup 4 dict5))
    (assert-equal "four" (lookup 4 dict6))
    (assert-equal "three" (lookup 3 dict6))
    (assert-equal "two" (lookup 2 dict6))
    (assert-equal "one" (lookup 1 dict6))
  )
)

(defun sum3 (a b c) (+ a (+ b c)))

(defun sumpos (a b c)
  "Return b+c if a > 0, c otherwise"
  (if (> a 0) (+ b c) c))

(defun concat3 (a b c) (concatenate 'string a b c))

(define-test fold
  (let ((dict (create-dictionary :compare #'numcompare))
        (dict2 (update 1 1 (create-dictionary :compare #'numcompare)))
        (dict3 (update "key" "a" (create-dictionary)))
        (dict4 (update "aaa" "bb" (update "key" "a" (create-dictionary))))
        (dict5 (update "1" "23" (update "a" "b" (update "c" "d"
          (create-dictionary))))))
    (assert-equal 0 (fold #'sum3 dict 0)) ; empty dict
    (assert-equal "A" (fold #'sum3 dict "A")) ; empty dict, incompatible type
    (assert-equal 2 (fold #'sum3 dict2 0))
    (assert-equal 4 (fold #'sum3 dict2 2))
    (assert-equal 1 (fold #'sumpos dict2 0))
    (assert-error 'error (fold #'sum3 dict3 "")) ; '+' not applicable to strings
    (assert-equal "keya" (fold #'concat3 dict3 ""))
    (assert-equal "keyadd" (fold #'concat3 dict3 "dd"))
    (assert-equal "aaabbkeya" (fold #'concat3 dict4 ""))
    (assert-equal 10 (length (fold #'concat3 dict4 "q"))
    (assert-equal "123abcdefg" (fold #'concat3 dict5 "efg")))
  )
)

(define-test rebalance
  (let ((dict (update 3 "three" (update 2 "two" (update 1 "one"
          (create-dictionary :compare #'numcompare)))))
        (dict2 (update 3 "three" (update 1 "one" (update 2 "two"
          (create-dictionary :compare #'numcompare)))))
        (dict3 (update 4 "four" (update 3 "three" (update 2 "two"
          (update 1 "one" (create-dictionary :compare #'numcompare))))))
        (dict4 (update 2 "two" (update 4 "four" (update 1 "one"
          (update 3 "three" (create-dictionary :compare #'numcompare))))))
        (dict5 (make-treedict :cmp #'numcompare :tree (make-treenode
          :key 4 :value 4 :size 6
          :left (make-treenode :key 2 :value 2 :size 3
            :left nil
            :right (make-treenode :key 3 :value 3 :size 2
              :left (make-treenode :key 1 :value 1 :size 1)
              :right nil))
          :right (make-treenode :key 5 :value 5 :size 2
            :left nil
            :right (make-treenode :key 6 :value 6 :size 1)))))
        ; (dict6 (make-treedict :cmp #'numcompare :tree (make-treenode
          ; :key 4 :value 4 :size 6
          ; :left (make-treenode :key 2 :value 2 :size 3
            ; :left (make-treenode :key 1 :value 1 :size 1)
            ; :right (make-treenode :key 3 :value 3 :size 1))
          ; :right (make-treenode :key 5 :value 5 :size 2
            ; :left nil
            ; :right (make-treenode :key 6 :value 6 :size 1)))))
            )
    ; (assert-equal 1 (treenode-key (treedict-tree dict)))
    ; (assert-equal 2 (treenode-key (treedict-tree (rebalance dict))))
    ; (assert-equal 2 (treenode-key (treedict-tree (rebalance dict2))))
    ; (assert-equal 1 (treenode-key (treedict-tree dict3)))
    ; (assert-equal 3 (treenode-key (treedict-tree (rebalance dict3))))
    (assert-false (equal (write-to-string dict2) (write-to-string dict)))
    (assert-equal (write-to-string dict2) (write-to-string (rebalance dict)))
    (assert-true (equal (write-to-string dict2) (write-to-string (rebalance dict))))
    (assert-false (equal (write-to-string dict2) (write-to-string dict)))
    (assert-equal (write-to-string dict2) (write-to-string (rebalance dict2)))
    (assert-equal (write-to-string dict4) (write-to-string (rebalance dict3)))
    (assert-equal (write-to-string dict4) (write-to-string (rebalance dict4)))
    ; (assert-equal (write-to-string dict6) (write-to-string (rebalance (rebalance dict5))))
    ; (assert-equal (write-to-string dict6) (write-to-string (rebalance dict5)))
    ; (assert-equal (write-to-string dict6) (write-to-string (rebalance dict6)))
  )
)

(define-test rotate
  (let ((dict (update "b" "b" (update "a" "a" (create-dictionary))))
        (dict2 (update "a" "a" (update "b" "b" (create-dictionary)))))
  ; (assert-equal
    ; (write-to-string (treedict-tree dict2))
    ; (write-to-string (rotate (treedict-tree dict) t))) ;rotate left
  ; (assert-equal
    ; (write-to-string (treedict-tree dict))
    ; (write-to-string (rotate (treedict-tree dict2) nil))) ; rotate right
  )
)

(define-test keys
  (let ((dict (create-dictionary :compare #'numcompare))
        (dict2 (update 1 "one" (update 2 "two"
          (create-dictionary :compare #'numcompare))))
        (dict3 (update 2 "two" (update 1 "one"
          (create-dictionary :compare #'numcompare))))
        (dict4 (update 3 "three" (update 2 "two" (update 1 "one"
          (create-dictionary :compare #'numcompare)))))
        (dict5 (update 4 "four" (update 3 "three" (update 2 "two"
          (update 1 "one" (create-dictionary :compare #'numcompare))))))
        (dict6 (update 93 "two" (update 118 "three" (update 7 "four"
          (update 42 "one" (create-dictionary :compare #'numcompare)))))))
    (assert-equal nil  (keys dict))
    (assert-equal nil (set-difference '(1) (keys (update 1 "one" dict))))
    (assert-equal nil (set-difference '(1 2) (keys dict2)))
    (assert-equal nil (set-difference '(1 2) (keys dict3)))
    (assert-equal nil (set-difference '(1 2 3) (keys dict4)))
    (assert-equal nil (set-difference '(1 2 3 4) (keys dict5)))
    (assert-equal nil (set-difference '(7 93 42 118) (keys dict6)))
  )
)


(define-test samekeys
  (let ((dict (create-dictionary :compare #'numcompare))
        (dict2 (update 1 "one" (update 2 "two"
          (create-dictionary :compare #'numcompare))))
        (dict3 (update 2 "two" (update 1 "one"
          (create-dictionary :compare #'numcompare))))
        (dict4 (update 3 "three" (update 2 "two" (update 1 "one"
          (create-dictionary :compare #'numcompare)))))
        (dict5 (update 4 "four" (update 3 "three" (update 2 "two"
          (update 1 "one" (create-dictionary :compare #'numcompare))))))
        (dict6 (update 1 "two" (update 3 "three" (update 2 "four"
          (update 4 "one" (create-dictionary :compare #'numcompare))))))
        (dict7 (update 42 "indeed" (update 13 "yup" (update 9 "nine"
          (update 1 "two" (update 3 "three" (update 2 "four"
          (update 4 "one" (create-dictionary :compare #'numcompare)))))))))
        (dict8 (update 4 "indeed" (update 1 "yup" (update 9 "nine"
          (update 13 "two" (update 3 "three" (update 2 "four"
          (update 42 "one" (create-dictionary :compare #'numcompare)))))))))
        (dict9 (update 4 "indeed" (update 41 "yup" (update 9 "nine"
          (update 13 "two" (update 3 "three" (update 2 "four"
          (update 1 "one" (create-dictionary :compare #'numcompare)))))))))
        ; (dict10 (make-treedict :cmp #'numcompare :tree (make-treenode
          ; :key 4 :value 4 :size 6
          ; :left  (make-treenode :key 2 :value 2 :size 3
            ; :left  (make-treenode :key 1 :value 1 :size 1)
            ; :right (make-treenode :key 3 :value 3 :size 2))
          ; :right (make-treenode :key 5 :value 5 :size 2
            ; :left  nil
            ; :right (make-treenode :key 6 :value 6 :size 1)))))
        ; (dict11 (make-treedict :cmp #'numcompare :tree (make-treenode
          ; :key 5 :value 5 :size 6
          ; :left  (make-treenode :key 3 :value 3 :size 3
            ; :left  (make-treenode :key 2 :value 2 :size 2
              ; :left (make-treenode :key 1 :value 1 :size 1)
              ; :right nil)
            ; :right (make-treenode :key 4 :value 4 :size 1))
          ; :right (make-treenode :key 6 :value 6 :size 1))))
        ; (dict12 (make-treedict :cmp #'numcompare :tree (make-treenode
          ; :key 5 :value 5 :size 6
          ; :left  (make-treenode :key 4 :value 4 :size 4
            ; :left  (make-treenode :key 3 :value 3 :size 3
              ; :left  (make-treenode :key 2 :value 2 :size 2
                ; :left (make-treenode :key 1 :value 1 :size 1)
                ; :right nil)
              ; :right nil)
            ; :right nil)
          ; :right (make-treenode :key 6 :value 6 :size 1))))
        ; (dict13 (make-treedict :cmp #'numcompare :tree (make-treenode
          ; :key 6 :value 6 :size 6
          ; :left (make-treenode :key 4 :value 4 :size 5
            ; :left (make-treenode :key 2 :value 2 :size 2
              ; :left (make-treenode :key 1 :value 1 :size 1)
              ; :right (make-treenode :key 3 :value 3 :size 1))
            ; :right (make-treenode :key 5 :value 5 :size 1))
          ; :right nil)))
        ; (dict14 (make-treedict :cmp #'numcompare :tree (make-treenode
          ; :key 2 :value 2 :size 6
          ; :left (make-treenode :key 1 :value 1 :size 1)
          ; :right (make-treenode :key 4 :value 4 :size 4
            ; :left (make-treenode :key 3 :value 3 :size 2)
            ; :right (make-treenode :key 5 :value 5 :size 2
              ; :left nil
              ; :right (make-treenode :key 6 :value 6 :size 1)))))))
    (assert-true (samekeys dict dict))
    (assert-false (samekeys dict dict2))
    (assert-false (samekeys dict2 dict))
    (assert-true (samekeys dict2 dict3))
    (assert-true (samekeys dict3 dict2))
    (assert-true (samekeys dict5 dict6))
    (assert-true (samekeys dict6 dict5))
    (assert-true (samekeys dict8 dict7))
    (assert-true (samekeys dict7 dict8))
    (assert-true (samekeys dict9 dict9))
    ; (assert-true (samekeys dict10 dict10))
    ; (assert-true (samekeys dict10 dict11))
    ; (assert-true (samekeys dict10 dict12))
    ; (assert-true (samekeys dict10 dict13))
    ; (assert-true (samekeys dict10 dict14))
    ; (assert-true (samekeys dict11 dict12))
    ; (assert-true (samekeys dict11 dict13))
    ; (assert-true (samekeys dict12 dict11))
    ; (assert-true (samekeys dict13 dict12))
    ; (assert-true (samekeys dict14 dict11))
    ; (assert-true (samekeys dict13 dict14))
    ; (assert-false (samekeys dict7 dict9))
    ; (assert-false (samekeys dict8 dict9))
    ; (assert-false (samekeys dict9 dict7))
    ; (assert-false (samekeys dict4 dict5))
    ; (assert-false (samekeys dict5 dict4))
  )
)

(define-test with-keys
  ; (let ((dict (make-treedict :cmp #'numcompare :tree (make-treenode
          ; :key 4 :value 4 :size 6
          ; :left  (make-treenode :key 2 :value 2 :size 3
            ; :left  (make-treenode :key 1 :value 1 :size 1)
            ; :right (make-treenode :key 3 :value 3 :size 2))
          ; :right (make-treenode :key 5 :value 5 :size 2
            ; :left  nil
            ; :right (make-treenode :key 6 :value 6 :size 1))))))
    ; (assert-equal 42 (with-keys dict (+ key value))) ; 1+1+2+2+3+3+4+4+5+5+6+6
    ; (assert-equal '(2 4 6 8 10 12) (with-keys dict (+ key value)))
    ; (labels ((my_comp (a b) (cond ((< a b) 'LT) ((= a b) 'EQ) (t 'GT))))
      ; (let ((d (update 2 4 (update 1 2
            ; (create-dictionary :compare #'my_comp)))))
        ; (with-keys (k v d) (format t "~D~%" (+ k v)))))
  )
)

(define-test match-pattern
  (assert-equal 3 (match-pattern 2
                      (1 (+ 10))
                      (2 (+ 1))))
  (assert-equal '(1 2 3) (match-pattern '(2 3)
                      ('(2 3) (cons 1))
                      ('(42 13) (cons 100))))
  (assert-equal '(1 2 3) (match-pattern '(2 3)
                      ('(X 3) (cons 1))
                      ('(42 13) (cons 100))))
  (assert-equal '(1 2 3) (match-pattern '(2 3)
                      ('(X X) (cons 0))
                      ('(X 3) (cons 1))
                      ('(42 13) (cons 100))))
  (assert-equal '(1 2 3) (match-pattern '(2 3)
                      ('(X X) (cons 0))
                      ('(2 X) (cons 1))
                      ('(42 13) (cons 100))))
  (assert-equal '(0 2 3) (match-pattern '(2 3)
                      ('(X Y) (cons 0))
                      ('(X 3) (cons 1))
                      ('(42 13) (cons 100))))
  (assert-equal '(0 2 2) (match-pattern '(2 2)
                      ('(X X) (cons 0))
                      ('(Y 3) (cons 1))
                      ('(42 13) (cons 100))))
  (assert-equal '(0 2 2) (match-pattern '(2 2)
                      ('(X X) (cons 0))
                      ('(X 3) (cons 1))
                      ('(42 13) (cons 100))))
  (assert-equal '(:THREE (FOO BAR) FOO BAZ)
    (match-pattern '((foo bar) foo baz)
                   ('(car . car) `(:cons ,car ,car))
                   ('(one two three) `(:three ,one ,two ,three))))
)
