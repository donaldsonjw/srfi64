
(define-syntax test-begin
  (syntax-rules ()
    ((test-begin suite-name)
     (%test-begin suite-name #f))
    ((test-begin suite-name count)
     (%test-begin suite-name count))))

(define-syntax test-group
  (syntax-rules ()
    ((test-group suite-name . body)
     (let ((r (test-runner-current)))
       ;; Ideally should also set line-number, if available.
       (test-result-alist! r (list (cons 'test-name suite-name)))
       (if (%test-should-execute r)
	   (dynamic-wind
	       (lambda () (test-begin suite-name))
	       (lambda () . body)
	       (lambda () (test-end  suite-name))))))))

(define-syntax test-group-with-cleanup
  (syntax-rules ()
    ((test-group-with-cleanup suite-name form cleanup-form)
     (test-group suite-name
		    (dynamic-wind
			(lambda () #f)
			(lambda () form)
			(lambda () cleanup-form))))
    ((test-group-with-cleanup suite-name cleanup-form)
     (test-group-with-cleanup suite-name #f cleanup-form))
    ((test-group-with-cleanup suite-name form1 form2 form3 . rest)
     (test-group-with-cleanup suite-name (begin form1 form2) form3 . rest))))


(define-syntax test-result-ref
  (syntax-rules ()
    ((test-result-ref runner pname)
     (test-result-ref runner pname #f))
    ((test-result-ref runner pname default)
     (let ((p (assq pname (test-result-alist runner))))
       (if p (cdr p) default)))))


(define-syntax %test-evaluate-with-catch
    (syntax-rules ()
      ((%test-evaluate-with-catch test-expression)
       (with-handler (lambda (ex)
                        (test-result-set! (test-runner-current) 'actual-error ex)
                        #f)
                     test-expression))))


(define-syntax %test-comp2body
  (syntax-rules ()
		((%test-comp2body r comp expected expr)
		 (let ()
		   (if (%test-on-test-begin r)
		       (let ((exp expected))
			 (test-result-expected-value! r exp)
			 (let ((res (%test-evaluate-with-catch expr)))
			   (test-result-actual-value! r res)
			   (%test-on-test-end r (comp exp res)))))
		   (%test-report-result)))))

(define-syntax %test-comp1body
  (syntax-rules ()
    ((%test-comp1body r expr)
     (let ()
       (if (%test-on-test-begin r)
	   (let ()
	     (let ((res (%test-evaluate-with-catch expr)))
	       (test-result-actual-value! r res)
	       (%test-on-test-end r res))))
       (%test-report-result)))))

(define-syntax test-end
   (syntax-rules ()
      ((test-end)
       (%test-end #f '()))
      ((test-end suite-name)
       (%test-end suite-name '()))))

(define-syntax test-assert
   (syntax-rules ()
      ((test-assert tname test-expression)
       (let* ((r (test-runner-get))
              (name tname))
          (test-result-alist! r '((test-name . tname)))
          (%test-comp1body r test-expression)))
      ((test-assert test-expression)
       (let* ((r (test-runner-get)))
          (test-result-alist! r '())
          (%test-comp1body r test-expression)))))

(define-syntax %test-comp2
   (syntax-rules ()
      ((%test-comp2 comp tname expected expr)
       (let* ((r (test-runner-get))
              (name tname))
          (test-result-alist! r (list (cons 'test-name tname)))
          (%test-comp2body r comp expected expr)))
      ((%test-comp2 comp expected expr)
       (let* ((r (test-runner-get)))
          (test-result-alist! r '())
          (%test-comp2body r comp expected expr)))))

;; alias for test-equal
(define-syntax test
   (syntax-rules ()
      ((test . rest)
       (%test-comp2 equal? . rest))))

(define-syntax test-equal
   (syntax-rules ()
      ((test-equal . rest)
       (%test-comp2 equal? . rest))))

(define-syntax test-eqv
   (syntax-rules ()
      ((test-eqv . rest)
       (%test-comp2 eqv? . rest))))

(define-syntax test-eq
   (syntax-rules ()
      ((test-eq . rest)
       (%test-comp2 eq? . rest))))

(define-syntax test-approximate
   (syntax-rules ()
      ((test-approximate tname expected expr error)
       (%test-comp2 (%test-approximate= error) tname expected expr))
      ((test-approximate expected expr error)
       (%test-comp2 (%test-approximate= error) expected expr))))


(define-syntax %test-error
   (syntax-rules ()
      ((%test-error r etype expr)
       (%test-comp1body r (with-handler
                             (lambda (ex)
                                (if (or (and (boolean? etype) etype)
                                        (isa? ex etype))
                                    #t #f))
                             (test-result-set! r 'actual-value expr)
                             #f)))))


(define-syntax test-error
    (syntax-rules ()
      ((test-error name etype expr)
       (let ((r (test-runner-get)))
         (test-result-alist! r `((test-name . ,name)))
         (%test-error r etype expr)))
      ((test-error etype expr)
       (let ((r (test-runner-get)))
         (test-result-alist! r '())
         (%test-error r etype expr)))
      ((test-error expr)
       (let ((r (test-runner-get)))
         (test-result-alist! r '())
         (%test-error r #t expr)))))


(define-syntax test-with-runner
  (syntax-rules ()
    ((test-with-runner runner form ...)
     (let ((saved-runner (test-runner-current)))
       (dynamic-wind
           (lambda () (test-runner-current runner))
           (lambda () form ...)
           (lambda () (test-runner-current saved-runner)))))))


;;; Predicates
(define-syntax test-match-nth
  (syntax-rules ()
    ((test-match-nth n)
     (test-match-nth n 1))
    ((test-match-nth n count)
     (%test-match-nth n count))))

(define-syntax test-match-all
  (syntax-rules ()
    ((test-match-all pred ...)
     (%test-match-all (%test-as-specifier pred) ...))))


(define-syntax test-match-any
  (syntax-rules ()
    ((test-match-any pred ...)
     (%test-match-any (%test-as-specifier pred) ...))))

(define-syntax test-skip
  (syntax-rules ()
    ((test-skip pred ...)
     (let ((runner (test-runner-get)))
       (%test-runner-skip-list! runner
				  (cons (test-match-all (%test-as-specifier pred)  ...)
					(%test-runner-skip-list runner)))))))

(define-syntax test-expect-fail
  (syntax-rules ()
    ((test-expect-fail pred ...)
     (let ((runner (test-runner-get)))
       (%test-runner-fail-list! runner
				  (cons (test-match-all (%test-as-specifier pred)  ...)
					(%test-runner-fail-list runner)))))))


