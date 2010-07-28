; Logic programming tools in Scheme
; (c) 2007 G. Nicholas D'Andrea

; We need to initialize cut methods and error catching methods
(define amb-cut #f)

; amb-catch should be #f unless something is currently catching errors.
(define amb-catch #f)

; amb - the ambiguous operator.
;   Accepts one argument: a list. Returns one of the values in the list.
;
;   amb hates being called with an empty list, so it will (essentially) predict
;   the future and pick the first value that avoids that.
(define amb
  (let* ((amb-fail (lambda () 
                     (cond
                       (amb-catch (amb-catch))
                       (else
                        (error "amb tree exhausted")))))
         (orig-amb-fail amb-fail))
    (lambda (L)
      (let ((prev-amb-fail amb-fail))
        (call-with-current-continuation
         (lambda (break)
           (map (lambda (V)
                  (call-with-current-continuation
                   (lambda (continuation)
                     (set! amb-cut (lambda ()
                                     (set! amb-fail orig-amb-fail)))
                     (set! amb-fail (lambda () 
                                      (set! amb-fail prev-amb-fail)
                                      (continuation V)))
                     (break V))))
                L)
           (amb-fail)))))))


(let ((x (amb '(5 6))))
  (cond ((= x 5) (amb '()))
        (else #t))
  (display x))

; assert returns true or false based on if it's possible to make a predicate 
;   true.
(define assert
  (lambda (pred)
    (let ((failed #f)
          (prev-amb-catch amb-catch))
      (call-with-current-continuation
       (lambda (continuation)
         (set! amb-catch (lambda () (set! failed #t) (continuation 'catch)))
         (if (not pred) (amb '()))))
      (set! amb-catch prev-amb-catch)
      (not failed))))       

; Compares two items with underscores as wild
(define compare
  (lambda (i j)
    (cond
      ((equal? '_ i) #t)
      ((equal? '_ j) #t)
      ((and (null? i) (null? j)) #t)
      ((and (list? i) (list? j))
       (and (compare (car i) (car j))
            (compare (cdr i) (cdr j))))
      ((equal? i j) #t)
      (else #f))))

; Returns a list of the items in j that correspond to the underscores (_) in i
(define match-up
  (lambda (i j)
    (cond
      ((null? i) '())
      ((equal? '_ (car i)) (append (list (car j)) (match-up (cdr i) (cdr j))))
      ((list? (car i))
       (append (match-up (car i) (car i)) (match-up (cdr i) (cdr j))))
      (else (match-up (cdr i) (cdr j))))))

; Returns a rule that compares successfully to the query
(define lookup
  (lambda (query env)
    (cond
      ((null? env) #f)
      (else
       (let* ((rule (amb env)))
         (if (assert (compare (car (deitemize query))
                              (car (deitemize (car rule)))))
             rule
             #f))))))

; Set up the environment
(define env '())

; Convert query to be stored in the env var.
(define itemize
  (lambda (terms vars)
    (if (null? vars) (set! vars '(())))
    (cond
      ((null? terms) '())
      ((equal? '_ (car terms))
       (cons `(var ,(car vars)) (itemize (cdr terms) (cdr vars))))
      ((list? (car terms))
       (cons (itemize (car terms) (car vars)) (itemize (cdr terms) (cdr vars))))
      (else (cons `(value ,(car terms)) (itemize (cdr terms) vars))))))

; Add a rule to the environment
(define add-to-env
  (lambda (env clauses variables)
    (append env (list (itemize clauses variables)))))

; Shorthand
(define :
  (lambda (clauses variables)
    (set! env (add-to-env env clauses variables))))

; Build pairs one at a time and make sure they work
(define build-variable-pairs
  (lambda (values variables pairs)
    (cond
      ((null? values) pairs)
      ((null? variables) pairs)
      (else
       (build-variable-pairs (cdr values)
                             (cdr variables)
                             (add-to-pairs (list (car variables) (car values))
                                           pairs))))))

; Add a pair to a list of pairs
(define add-to-pairs
  (lambda (new-pair pairs-existing)
    (cond
      ((null? pairs-existing) (list new-pair))
      (else
       (let ((pair-there (assoc (car new-pair) pairs-existing)))
         (cond
           (pair-there (if (equal? (cdr pair-there) (cdr new-pair))
                           pairs-existing
                           #f))
           (else (append pairs-existing (list new-pair)))))))))

; Convert env-style rules to query-style
(define deitemize
  (lambda (terms)
    (cond
      ((null? terms) '(() ()))
      (else
       (let ((recurse (deitemize (cdr terms))))
         (cond
           ((equal? 'value (caar terms))
            (list (cons (cadar terms) (car recurse)) (cadr recurse)))
           ((equal? 'var (caar terms))
            (list (cons '_ (car recurse)) (cons (cadar terms) (cadr recurse))))
           (else
            (let ((recurse-car (deitemize (car terms))))
              (list (cons (car recurse-car) (car recurse))
                    (cons (cadr recurse-car) (cadr recurse)))
              ))))))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) l)
      ((equal? l old) new)
      ((atom? l) l)
      (else
       (cons (subst* new old (car l))
             (subst* new old (cdr l)))))
    ))

; Plug in the variables we know into the query
(define plugin-vars
  (lambda (vars-defined query)
    (for-each (lambda (var)
                (set! query (subst* (cadr var) `(var ,(car var)) query)))
              vars-defined)
    query))

; Get all the pairs except the ones we won't know (cause we're requesting those)
(define pairs-except-requested-vars
  (lambda (values variables requested-vars)
    (cond
      ((null? values) '())
      ((and (equal? (car (car values)) 'var)
            (assoc (cadr (car values)) requested-vars))
       (pairs-except-requested-vars (cdr values)
                                    (cdr variables)
                                    requested-vars))
      (else
       (add-to-pairs (list (car variables) (car values))
                     (pairs-except-requested-vars (cdr values)
                                                  (cdr variables)
                                                  requested-vars))))))

; Get the pairs for a particular rule and query
(define values-except-requested-vars
  (lambda (rule query requested-vars)
    (let* ((d (deitemize (car rule)))
           (d-rule (car d))
           (d-vars (cadr d))
           (match (match-up d-rule query)))
      (pairs-except-requested-vars match d-vars requested-vars)
      )))

; Add a bunch of pairs to a list of pairs
(define add-pairs-to-list
  (lambda (pairs list)
    (cond
      ((null? pairs) list)
      (else
       (add-to-pairs (car pairs) (add-pairs-to-list (cdr pairs) list))))))

; Recurse over each horn clause
(define recurse-?
  (lambda (clauses private-vars)
    (cond
      ((null? clauses) private-vars)
      (else
       (let* ((d (deitemize (plugin-vars private-vars (car clauses))))
              (clause (car d))
              (variables (cadr d))
              (pairs (? clause variables)))
         (cond
           (pairs
            (let* ((pairs (if (equal? #t pairs) '() pairs))
                   (private-vars (add-pairs-to-list pairs private-vars)))
              (if (assert private-vars)
                  (recurse-? (cdr clauses) private-vars)
                  #f)))
           (else #f)))))))

; Make a query
; The basic operation is this:
;  - Lookup the query in the environment, and maintain a list of the
;      requested variables
;  - We also need to keep track of variables within the rule in the env.
;      (Some might be provided for us)
;  - Then go through and evaluate each part of the horn clause, adding
;      those requested values to the private variables list
;  - Then, fill in the requested variables with what we know.
(define ?
  (lambda (query variables)
    ; Account for reserved predicates
    (cond
      ; Cuts
      ((equal? (car query) '!)
       (amb-cut) #t)
      ; And support basic math operations
      ((equal? (car query) '+)
       (cond
         ((eq? (length variables) 0)
          (eq? (cadddr query) (+ (cadr query) (caddr query))))
         ((eq? (length variables) 1)
          (cond
            ((equal? '_ (cadr query))
             (list (list (car variables)
                         `(value ,(- (cadddr query) (caddr query))))))
            ((equal? '_ (caddr query))
             (list (list (car variables)
                         `(value ,(- (cadddr query) (cadr query))))))
            ((equal? '_ (cadddr query))
             (list (list (car variables)
                         `(value ,(+ (cadr query) (caddr query))))))))
         (else #f)))
      ((equal? (car query) '*)
       (cond
         ((eq? (length variables) 0)
          (eq? (cadddr query) (* (cadr query) (caddr query))))
         ((eq? (length variables) 1)
          (cond
            ((equal? '_ (cadr query))
             (list (list (car variables)
                         `(value ,(/ (cadddr query) (caddr query))))))
            ((equal? '_ (caddr query))
             (list (list (car variables)
                         `(value ,(/ (cadddr query) (cadr query))))))
            ((equal? '_ (cadddr query))
             (list (list (car variables)
                         `(value ,(* (cadr query) (caddr query))))))))
         (else #f)))
      ; Otherwise, evaluate it.
      (else
       ;first we get a matching rule for our query
       (let ((rule (lookup (itemize query variables) env)))  
         (cond
           (rule
            ;then we build our assoclist of requested variables
            ;and the private variables
            (let* ((requested-vars
                    (build-variable-pairs (match-up query (car rule))
                                          variables '())) 
                   (private-vars
                    (values-except-requested-vars rule
                                                  (itemize query variables)
                                                  requested-vars)))
              (assert private-vars)
              (let ((private-vars (recurse-? (cdr rule) private-vars)))
                (assert private-vars)
                (cond
                  (private-vars
                   (if (null? requested-vars) #t
                       (plugin-vars private-vars requested-vars)))
                  (else #f))
                )))
           (else #f)))))))
