(load "logic.scm")

; Set up some basic mother/father - child relationships
(: '((mother_child trude sally)) '(()))
(: '((father_child tom sally)) '(()))
(: '((father_child tom erica)) '(()))
(: '((father_child mike tom)) '(()))
(: '((mother_child amy tom)) '(()))

; We're defining a sibling as someone who has a parent in common
(: '((sibling _ _) (parent_child _ _) (parent_child _ _)) '((X Y) (Z X) (Z Y)))

; A parent is either a father or a mother
(: '((parent_child _ _) (father_child _ _)) '((X Y) (X Y)))
(: '((parent_child _ _) (mother_child _ _)) '((X Y) (X Y)))

; A person X is the ancestor Y if the child Z of X is the ancestor of Y
; also, a person is considered their own ancestor
(: '((anc_desc _ _) (parent_child _ _) (anc_desc _ _)) '((X Y) (X Z) (Z Y)))
(: '((anc_desc _ _)) '((X X)))

; Here's an example to show cuts. No queries will ever say that nick is human.
(: '((human john)) '())
(: '((human eric) (!)) '())
(: '((human nick)) '())

; 1 is the factorial of 0,
; and F is the factorial of N if: N1 is N minus 1, F1 is the factorial of N1,
;    and F is N * F1
(: '((factorial 0 1)) '(()))
(: '((factorial _ _) (+ _ 1 _) (factorial _ _) (* _ _ _))
   '((N F) (N1 N) (N1 F1) (N F1 F)))

; Show me all the humans!
; (Will only produce john and eric, as there is a cut beyond that point
(let ((H (? '(human _) '(Human))))
  (if H (begin (display H) (newline)))
  (assert #f))

; Show me all the descendents of mike!
(let ((D (? '(anc_desc mike _) '(MikeDescendent))))
  (if D (begin (display D) (newline)))
  (assert #f))

; What's the factorial of 10?
(display (? '(factorial 10 _) '(Factorial)))