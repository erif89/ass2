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
(defstruct treenode key value size left right)

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
;; Comparison function for numbers
;;
(defun numcompare (a b)
  "Returns LT for less than, T for equal and GT for greater than"
  (cond
    ((< a b) 'LT)
    ((= a b) T)
    ((> a b) 'GT)))

;;
;; Creates a new empty dictionary with compare being the ordering function to
;; be used for key comparisons. If compare is not given, strcompare is used.
;;
(defun create-dictionary (&key compare)
  "Returns the empty dict with compare (or strcompare) as ordering function"
  (if (null compare)
      (make-treedict :cmp #'strcompare)
      (make-treedict :cmp compare)))

;;
;; Finds value that key is mapped to in dict, returns default if it does not
;; exist, or nil if no default value is given.
;;
(defun lookup (key dict &key default)
  "Returns dict[key], or default/nil if no such value exists"
  (let ((tree (treedict-tree dict))
        (cmp (treedict-cmp dict)))
    (if tree
        (lookuphelper key tree default cmp)
        default)))

;;
;; Help function for lookup, recurse over the tree in search of key.
;;
(defun lookuphelper (key node default cmp)
  "Returns value associated with key in node subtree, or default/nil"
  (let ((key2 (treenode-key node))
        (value (treenode-value node))
        (left (treenode-left node))
        (right (treenode-right node)))
    (cond
      ((null key2)
        default)
      ((eq (funcall cmp key key2) 'T)
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
  (let ((tree (treedict-tree dict))
        (cmp (treedict-cmp dict)))
    (make-treedict
     :tree (if tree
               (updatehelper key value tree cmp)
               (make-treenode :key key :value value :size 1))
     :cmp cmp)))

;;
;; Help function for update. cmp is used for key comparisons in the key.
;;
(defun updatehelper (key value node cmp)
  "Returns node with (key, value) destructively inserted"
  (let ((key2 (treenode-key node))
        (value2 (treenode-value node))
        (size (treenode-size node))
        (left (treenode-left node))
        (right (treenode-right node)))
    (cond
      ((eq (funcall cmp key key2) 'T)       ; Keys match
        (make-treenode :key key :value value
         :left left :right right :size size))
      ((eq (funcall cmp key key2) 'LT)      ; Update left subtree
        (if left  ; End of recursion if subtree is empty
          (make-treenode :key key2 :value value2 :size (+ size 1)
           :left (updatehelper key value left cmp)
           :right right)
          (make-treenode :key key2 :value value2 :size (+ size 1)
           :left (make-treenode :key key :value value :size 1)
           :right right)))
      ((eq (funcall cmp key key2) 'GT)      ; Update right subtree
        (if right
          (make-treenode :key key2 :value value2 :size (+ size 1)
           :left left
           :right (updatehelper key value right cmp))
          (make-treenode :key key2 :value value2 :size (+ size 1)
           :left left
           :right (make-treenode :key key :value value :size 1)))))))

;;
;; fold the key-value pairs of the dictionary using the function fun, which
;; should take three arguments: key, value and sofar (in this order) - the
;; accumulated value so far. initial is the initial value for sofar. The order
;; of application is (or at least should be) irrelevant.
;;
(defun fold (fun dict initial)
  "Returns fun(k1, v2, fun(k2, v2, ...initial...)) for key-values in dict"
  (let ((tree (treedict-tree dict)))
    (if tree
        (foldhelper fun tree initial)
        initial)))

;;
;; Help function to fold. Applies fun to key-values in preorder walk of node.
;;
(defun foldhelper (fun node sofar)
  "Returns fun(k1, v2, fun(k2, v2, ...sofar...)) for key-values in node"
  (let ((left (treenode-left node))
        (right (treenode-right node))
        (res (funcall fun (treenode-key node) (treenode-value node) sofar)))
    (cond
      ((and left right) (foldhelper fun right (foldhelper fun left res)))
      (left (foldhelper fun left res))
      (right (foldhelper fun right res))
      (t res))))

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
      ((not (or left right)) node) ; leaf
      ((not left) ; right skewed
        (rotate (make-treenode :key key :value value :size size
          :right (rebalancehelper right)) nil))
      ((not right) ; left skewed
        (rotate (make-treenode :key key :value value :size size
          :left (rebalancehelper left)) t))
      ((< (abs (- (treenode-size left) (treenode-size right))) 2) ; balanced
        (make-treenode :key key :value value :size size
          :left (rebalancehelper left) :right (rebalancehelper right)))
      (t (rotate (make-treenode :key key :value value :size size
                  :left (rebalancehelper left) :right (rebalancehelper right))
          (< (treenode-size right) (treenode-size left))))))) ; skewed

;;
;; Performs rotation of binary tree node.
;; Left if rotate-left, right otherwise.
;; Precondition: right child is not nil if rotate-left, and vice versa.
;;
(defun rotate (node rotate-left)
  (let ((key (treenode-key node))
        (value (treenode-value node))
        (size (treenode-size node))
        (left (if rotate-left (treenode-left node) (treenode-right node)))
        (right (if rotate-left (treenode-right node) (treenode-left node))))
    (let ((rkey (treenode-key right))
          (rvalue (treenode-value right))
          (rsize (treenode-size right))
          (lsize (if left (treenode-size left) 0))
          (rleft (treenode-left right))
          (rright (treenode-right right)))
      (make-treenode :key rkey :value rvalue :size (+ 
          (+ lsize (if rleft (treenode-size rleft) 0))
          (+ (if rright (treenode-size rright) 0) 2))
        :left (make-treenode :key key :value value
          :size (+ (+ lsize 1) (if rleft (treenode-size rleft) 0))
          :left left :right rleft)
        :right rright))))

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
  (cond
    ((null node) acc)
    ('T (let ((key (treenode-key node))
              (left (treenode-left node))
              (right (treenode-right node)))
          (cons key  (keyshelper left (keyshelper right acc)))))))
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
       (root2 (treedict-tree dict2))
       (size1 (treenode-size (treedict-tree dict1)))
       (size2 (treenode-size (treedict-tree dict2)))
       (cmp1   (treedict-cmp dict1))
       (cmp2   (treedict-cmp dict2)))
    (cond
    ((and (eq cmp1 cmp2) (= size1 size2)) (samekeyshelper root1 root2 cmp1))
    ('T nil
  ))))

;;
;; Help function for keys, recurse over the tree to build a list of all keys.
;;
(defun samekeyshelper (node1 node2 cmp)
  "Returns value associated with key in node subtree, or default/nil"
  (cond
    ((and (null node1) (null node2)) 'T)
    ((or (null node1) (null node2)) nil)
    ('T 
      (let ((key1 (treenode-key node1))
           (key2 (treenode-key node2)))
        (cond
          ((eq (funcall cmp key1 key2) 'LT) (and (samekeyshelper node1 (treenode-left node2) cmp)
                                                 (samekeyshelper (treenode-right node1) node2 cmp)))
          ((eq (funcall cmp key1 key2) 'GT) (and (samekeyshelper (treenode-left node1) node2 cmp)
                                                 (samekeyshelper node1 (treenode-right node2) cmp)))
          ((eq (funcall cmp key1 key2) 'T)  
            (if (eq (samekeyshelper (treenode-left node1) (treenode-left node2) cmp) 'T) 'T nil))
          )))))
          

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
;; Test functions
;;

(define-test numcompare
  (assert-equal 'GT (numcompare 1 -1))
  (assert-equal 'GT (numcompare 12345 5432))
  (assert-equal 'T  (numcompare 1 1))
  (assert-equal 'T  (numcompare 12345 12345))
  (assert-equal 'LT (numcompare -1 1))
  (assert-equal 'LT (numcompare 5432 12345))
  (assert-equal 'T (numcompare 0 0))
  (assert-equal 'T (numcompare 0.0 0/1))
  (assert-equal 'LT (numcompare -3.14 -3.0))
  (assert-equal 'GT (numcompare 3.14 3.0))
  (assert-equal 'GT (numcompare 1/8 2/17))
  (assert-equal 'T (numcompare 1/8 0.125))
  (assert-equal 'T (numcompare 1e10 (+ 1e10 1))) ; limited float precision
  (assert-equal 'LT (numcompare 1e10 (+ 1e10 1000)))
  (assert-error 'error (numcompare 0 "")) ; "" is not a number
  (assert-error 'error (numcompare nil nil)) ; nil is not a number
  )
  
(define-test strcompare
  (assert-equal 'GT (strcompare "ABC" "AAA"))
  (assert-equal 'GT (strcompare "ZYX" "ONM"))
  (assert-equal 'T  (strcompare "ABC" "ABC"))
  (assert-equal 'T  (strcompare "TTT" "TTT"))
  (assert-equal 'LT (strcompare "AAA" "ABC"))
  (assert-equal 'LT (strcompare "ONM" "ZYX"))
  (assert-equal 'LT (strcompare "ABC" "abc"))
  (assert-equal 'T  (strcompare "" ""))
  (assert-equal 'LT (strcompare " A" "Qwerty"))
  (assert-equal 'GT (strcompare
                     "haveidonethislongenoughnoworisitagoodideatoaddmoretests"
                     "haveidonethislongenoughnoworisitagoodideatoaddmoretest"))
  (assert-equal 'LT (strcompare "" " "))
  (assert-equal 'GT (strcompare " " ""))
  (assert-equal 'GT (strcompare "_" " "))
  (assert-equal 'T (strcompare "  " "  "))
  (assert-equal 'T (strcompare "1234" "1234"))
  (assert-equal 'LT (strcompare "123" "1234"))
  (assert-equal 'GT (strcompare "321" "1234"))
  (assert-equal 'LT (strcompare "" ()))
  (assert-equal 'LT (strcompare "ABC" ()))
  (assert-equal 'GT (strcompare nil ""))
  (assert-equal 'LT (strcompare nil "Q"))
  (assert-error 'error (strcompare 0 "")) ; 0 is not a string
  )

(define-test create-dictionary
  (let ((dict )
        (dict2 )
        (dict3 (create-dictionary)))
    (assert-equal nil (treedict-tree (create-dictionary)))
    (assert-equal nil (treedict-tree
      (create-dictionary :compare #'numcompare)))
    (assert-equal #'numcompare (treedict-cmp 
      (create-dictionary :compare #'numcompare)))
    (assert-equal #'strcompare (treedict-cmp (create-dictionary)))
    (assert-equal #'strcompare (treedict-cmp 
      (create-dictionary :compare nil)))
  )
)

(define-test update
  (assert-equal
    (write-to-string (update "a" "b" (update "a" "b" (create-dictionary))))
    (write-to-string (update "a" "b" (create-dictionary))))
  (assert-equal
    (write-to-string (make-treedict
     :tree (make-treenode :key 1 :value "one" :size 4
      :right (make-treenode :key 2 :value "two" :size 3
       :right (make-treenode :key 3 :value "three" :size 2
        :right (make-treenode :key 4 :value "four" :size 1))))
     :cmp #'numcompare))
    (write-to-string (update 4 "four" (update 3 "three" (update 2 "two"
     (update 1 "one" (create-dictionary :compare #'numcompare)))))))
  (assert-equal
    (write-to-string (make-treedict
     :tree (make-treenode :key 2 :value "two" :size 4
      :left (make-treenode :key 1 :value "one" :size 1)
      :right (make-treenode :key 3 :value "three" :size 2
       :right (make-treenode :key 4 :value "four" :size 1)))
     :cmp #'numcompare))
    (write-to-string (update 4 "four" (update 3 "three" (update 1 "one"
     (update 2 "two" (create-dictionary :compare #'numcompare)))))))
  (assert-equal
    (write-to-string (make-treedict
     :tree (make-treenode :key 3 :value "three" :size 4
      :left (make-treenode :key 1 :value "one" :size 2
       :right (make-treenode :key 2 :value "two" :size 1))
      :right (make-treenode :key 4 :value "four" :size 1))
     :cmp #'numcompare))
    (write-to-string (update 4 "four" (update 2 "two" (update 1 "one"
     (update 3 "three" (create-dictionary :compare #'numcompare)))))))
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
  (assert-true nil)
)

(define-test rotate
  (let ((dict (update "b" "b" (update "a" "a" (create-dictionary))))
        (dict2 (update "a" "a" (update "b" "b" (create-dictionary)))))
  (assert-equal
    (write-to-string (treedict-tree dict2))
    (write-to-string (rotate (treedict-tree dict) t))) ;rotate left
  (assert-equal
    (write-to-string (treedict-tree dict))
    (write-to-string (rotate (treedict-tree dict2) nil))) ; rotate right
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
        (dict7 (update 1 "two" (update 3 "three" (update 2 "four"
          (update 4 "one" (create-dictionary :compare #'numcompare)))))))
    (assert-true (samekeys dict2 dict3))
    (assert-true (samekeys dict3 dict2))
    (assert-true (samekeys dict5 dict6))
    (assert-true (samekeys dict6 dict5))
    (assert-false (samekeys dict4 dict5))
    (assert-false (samekeys dict5 dict4))
  )
)

(define-test with-keys
  (assert-true nil)
)

(define-test match-pattern
  (assert-true nil)
)
