;last modified 2017-01-07

(defvar *cliiscm-translators* (make-hash-table))

(defvar *it*)

(defvar *inside-quote-p* nil)

(defmacro def-cliiscm-translator (name params &rest body)
  `(setf (gethash ',name *cliiscm-translators*)
         (lambda ,params ,@body)))

(defun translate-exp (e)
  (cond ((consp e)
         ;(format t "translating ~s~%" (car e))
         (cond ((and (not *inside-quote-p*) (eq (car e) 'quote) (null (cadr e)))
                'null)
               ((setq *it* (gethash (car e) *cliiscm-translators*))
                (apply *it* (cdr e)))
               (t (mapcar #'translate-exp e))))
        ((and (keywordp e) (not *inside-quote-p*))
         (list 'quote e))
        ((and (null e) (not *inside-quote-p*))
         'false)
        ;((null e) 'null)
        ((null e)
         '())
        (t e)))

(defun prune-begins (res)
  (if (/= (length res) 1) res
      (let ((a (car res)))
        (cond ((atom a) res)
              ((eq (car a) 'begin) (prune-begins (cdr a)))
              (t res)))))

(defun translate-implicit-progn (ee)
  (let ((n (length ee)) res)
    (dotimes (i n)
      (let ((e-i (translate-exp (elt ee i))))
        (unless (and (atom e-i) (< i (1- n)))
          (setq res (append res (list e-i))))))
    (prune-begins res)))

(defun translate-progn (ee)
  (let ((ee-i (translate-implicit-progn ee)))
    (case (length ee-i)
      (0 `false)
      (1 (car ee-i))
      (t `(begin ,@ee-i)))))

(def-cliiscm-translator quote (x)
  (let ((*inside-quote-p* t))
    `(quote ,(translate-exp x))))

(def-cliiscm-translator funcall (&rest funkall)
  (mapcar #'translate-exp funkall))

(def-cliiscm-translator defstruct (name &rest fields)
  `(defstruct ,name
     ,@(mapcar (lambda (fld)
                 (if (consp fld) `(,(let ((*inside-quote-p* t)) (translate-exp (car fld)))
                                    ,(translate-exp (cadr fld)))
                     (let ((*inside-quote-p* t)) (translate-exp fld))))
               fields)))

(def-cliiscm-translator defmacro (name params &rest body)
  (unless (member name *defs-to-ignore*)
    `(define-syntax ,name
       (lambda (%so)
         (datum->syntax %so
           (let ((%so-d (syntax->datum %so)))
             (apply
               ,(translate-exp `(lambda ,params ,@body))
               (cdr %so-d))))))))

(def-cliiscm-translator lambda (params &rest body)
  (let* ((i (- (length params) 1))
         (new-params '())
         (opts nil))
    (loop
      (when (< i 0) (return))
      (let ((param (elt params i)))
        (case param
          (&optional (setq opts new-params)
                     (setq new-params '(&rest %lambda-rest-arg)))
          (&rest (unless (= (length new-params) 1)
                   (error 'lambda ""))
                 (push param new-params))
          (t (push param new-params)))
        (decf i)))
    (let ((last-i (- (length new-params) 1)))
      (when (and (> last-i 0) (eq (elt new-params (- last-i 1)) '&rest))
        (if (= last-i 1)
            (setq new-params (elt new-params last-i))
            (let ((new-new-params (butlast new-params 2)))
              (setf (cdr (nthcdr (- last-i 2) new-new-params)) (elt new-params last-i))
              (setq new-params new-new-params)))))
    ;(format t "opts= ~s; body= ~s~%" opts body)
    (if opts
        (let ((opts-len (length opts)))
          `(lambda ,new-params
             (let ((%lambda-rest-arg-len (length %lambda-rest-arg))
                   ,@(mapcar (lambda (opt)
                               (if (consp opt) opt
                                   `(,opt false)))
                             opts))
               ,@(let (s)
                   (dotimes (i opts-len)
                     (setq s
                           (append s
                                   (list `(when (< ,i %lambda-rest-arg-len)
                                            (set! ,(let ((opt (elt opts i)))
                                                     (if (consp opt) (car opt) opt))
                                              (list-ref %lambda-rest-arg ,i)))))))
                   s)
               ,@(translate-implicit-progn body))))
        `(lambda ,new-params
           ,@(translate-implicit-progn body)))))

(def-cliiscm-translator destructuring-bind (vars exp &rest body)
  (translate-exp `(apply (lambda ,vars ,@body) ,exp)))

(defvar *fluid-vars* '())

(defun special-var-p (x)
  (let* ((x-s (symbol-name x))
        (n (length x-s)))
    (and (char= (char x-s 0) #\*)
         (char= (char x-s (- n 1)) #\*))))

(defun nil-to-false (x)
  (if (not x) 'false x))

(defun translate-let-binding (x v)
  (cond ((special-var-p x)
         (push x *fluid-vars*)
         (let ((x-prime (intern (concatenate 'string "%FLUID-VAR-" (symbol-name x)))))
           `(,x-prime ,(nil-to-false v))))
        (t `(,x ,(nil-to-false v)))))

(def-cliiscm-translator let (vars &rest body)
  (if (and (= (length body) 1)
           (consp (car body)) (eq (car (car body)) 'defun))
      (translate-exp `(define ,(cadr (car body))
                        (let ,vars
                          (lambda ,(caddr (car body))
                            ,@(cdddr (car body))))))
      (let ((*fluid-vars* '()))
        `(let ,(mapcar
                 (lambda (x) (if (consp x)
                                 (translate-let-binding (car x) (translate-exp (cadr x)))
                                 (translate-let-binding x 'false)))
                 vars)
           ,(if *fluid-vars*
                `(fluid-let ,(mapcar (lambda (x)
                                       `(,x ,(intern (concatenate 'string "%FLUID-VAR-"
                                                       (symbol-name x)))))
                                     *fluid-vars*)
                            ,@(translate-implicit-progn body))
                (translate-progn body))))))

(def-cliiscm-translator let* (vars &rest body)
  (if vars (translate-exp `(let (,(car vars)) (let* ,(cdr vars) ,@body)))
      (translate-progn body)))

(def-cliiscm-translator flet (vars &rest body)
  `(let ,(mapcar (lambda (x)
                   `(,(car x) ,(translate-exp `(lambda ,(cadr x) ,@(cddr x)))))
                 vars)
     ,@(translate-implicit-progn body)))

(def-cliiscm-translator labels (vars &rest body)
  `(letrec ,(mapcar (lambda (x)
                      `(,(car x) ,(translate-exp `(lambda ,(cadr x) ,@(cddr x)))))
                    vars)
     ,@(translate-implicit-progn body)))

(def-cliiscm-translator defun (fname params &rest body)
  (unless (member fname *defs-to-ignore*)
    `(define ,fname
       ,(translate-exp `(lambda ,params ,@body)))))

(def-cliiscm-translator setq (&rest ee)
  (let ((assignments ee) s last-x)
    (loop
      (when (null assignments) (return `(begin ,@s ,last-x)))
      (setq last-x (car assignments))
      (setq s (append s (list `(set! ,last-x ,(translate-exp (cadr assignments))))))
      (pop assignments)
      (pop assignments))))

(def-cliiscm-translator progn (&rest ee)
  (translate-progn ee))

(def-cliiscm-translator function (x)
  x)

; make a single call to string-append or append?

(def-cliiscm-translator concatenate (tipe &rest ee)
  `(let ((%type ,tipe)
         (%ee (list ,@(mapcar #'translate-exp ee))))
     (let ((%res (if (eq? %type 'string) "" null)))
       (let %concatenate-loop ((%ee %ee))
         (if (null? %ee) %res
             (let ((%a (car %ee)))
               (unless (not %a)
                 (set! %res
                   (if (eq? %type 'string)
                       (string-append %res (if (string? %a) %a (list->string %a)))
                       (append %res (if (string? %a) (string->list %a) %a)))))
               (%concatenate-loop (cdr %ee)))))
       %res)))

(def-cliiscm-translator declare (&rest ee)
  `false)

(def-cliiscm-translator the (tipe val)
  val)

(def-cliiscm-translator prog1 (e &rest ee)
  `(let ((%prog1-first-value ,(translate-exp e)))
     ,@(translate-implicit-progn ee)
     %prog1-first-value))

(def-cliiscm-translator loop (&rest ee)
  `(let* ((%loop-returned false)
          (%loop-result 0)
          (return (lambda %args
                    (set! %loop-returned true)
                    (set! %loop-result (and (pair? %args) (car %args))))))
     (let %loop ()
       ;(set! %loop-result (+ %loop-result 1))
       ;(when (> %loop-result 10000) (error "inf loop?" ',ee))
       ,@(mapcar (lambda (e) 
                   (translate-exp `(%unless %loop-returned ,e))) ee)
       (if %loop-returned %loop-result (%loop)))))

(def-cliiscm-translator dotimes (i-n &rest ee)
  (let ((i (translate-exp (car i-n)))
        (res (caddr i-n)))
    `(let ((%dotimes-n ,(translate-exp (cadr i-n)))
           (,i 0))
       ,(translate-exp `(loop
                          (if (>= ,i %dotimes-n)
                            ,(if (null res) `(return)
                                 `(return ,(translate-exp res))))
                          ,@ee
                          (set! ,i (+ ,i 1)))))))

(def-cliiscm-translator dolist (x-l &rest ee)
  (let ((x (car x-l)))
    `(let ((%dolist-l ,(translate-exp (cadr x-l)))
           (,x false))
       ,(translate-exp `(loop
                          (if (null? %dolist-l) (return))
                          (set! ,x (car %dolist-l))
                          (set! %dolist-l (cdr %dolist-l))
                          ,@ee)))))

(def-cliiscm-translator defparameter (x v)
  (unless (member x *defs-to-ignore*)
    `(define ,x ,(translate-exp v))))

(def-cliiscm-translator defvar (x &optional v)
  (unless (member x *defs-to-ignore*)
    (if (null v)
        `(define ,x false)
        `(define ,x ,(translate-exp v)))))

(def-cliiscm-translator incf (x &optional v)
  (let ((lhs (translate-exp x)))
    `(let ((%tmp (+ ,lhs ,(translate-exp (or v 1)))))
       ,(translate-exp `(setf ,x %tmp)))))

(def-cliiscm-translator decf (x &optional v)
  (let ((lhs (translate-exp x)))
    `(let ((%tmp (- ,lhs ,(translate-exp (or v 1)))))
       ,(translate-exp `(setf ,x %tmp)))))

(def-cliiscm-translator setf (&rest ee)
  (let ((assignments ee) s last-lhs)
    (loop
      (when (null assignments) (return `(begin ,@s ,(translate-exp last-lhs))))
      (setq
        s
        (append s
                (list
                  (let ((lhs (car assignments)) (rhs (cadr assignments)))
                    (setq last-lhs lhs)
                    (cond ((consp lhs)
                           (let* ((getter (car lhs))
                                  (setter (gethash getter *setf-setters*)))
                             (unless setter
                               (format t "missing setter for ~s~%" getter))
                             `(,setter ,@(mapcar #'translate-exp (cdr lhs))
                                       ,(translate-exp rhs))))
                          (t `(set! ,(translate-exp lhs) ,(translate-exp rhs))))))))
      (pop assignments)
      (pop assignments))))

(defun translate-case-tags (tags)
  (if (consp tags) tags
      (list tags)))

(def-cliiscm-translator cond (&rest clauses)
  `(cond ,@(let (output-clauses
                  (n (length clauses)))
             (dotimes (i n)
               (let* ((clause (elt clauses i))
                      (clause-test (car clause))
                      output-clause)
                 (cond ((and (= i (1- n)) (eq clause-test 'true))
                        (setq output-clause
                              `(else ,@(translate-implicit-progn
                                         (cdr clause)))))
                       (t
                         (setq output-clause
                               `(,(translate-exp clause-test)
                                  ,@(translate-implicit-progn (cdr clause))))))
                 (setq output-clauses (append output-clauses (list output-clause)))))
             output-clauses)))

(def-cliiscm-translator if (test then-branch &optional else-branch)
  `(if ,(translate-exp test)
       ,(translate-exp then-branch)
       ,(translate-exp else-branch)))

(def-cliiscm-translator when (test &rest clauses)
  `(cond (,(translate-exp test)
           ,@(translate-implicit-progn clauses))
         (else false)))

(def-cliiscm-translator %unless (test &rest clauses)
  `(unless ,(translate-exp test)
     ,@(translate-implicit-progn clauses)))

(def-cliiscm-translator unless (test &rest clauses)
  `(cond ((not ,(translate-exp test))
          ,@(translate-implicit-progn clauses))
         (else false)))

(def-cliiscm-translator case (tag &rest clauses)
  ;(format t "doing translate of case~%")
  `(case ,(translate-exp tag)
     ,@(let (output-clauses
              (n (length clauses)))
         (dotimes (i n)
           (let* ((clause (elt clauses i))
                  (clause-tag (car clause))
                  (clause-action (cdr clause))
                  output-clause)
           (cond ((and (= i (- n 1)) (eq clause-tag 'true))
                  (setq output-clause
                        `(else ,@(translate-implicit-progn clause-action))))
                 (t
                   (setq output-clause
                         `(,(translate-case-tags clause-tag)
                            ,@(translate-implicit-progn clause-action)))))
           (setq output-clauses (append output-clauses (list output-clause)))))
         output-clauses)))

(def-cliiscm-translator ecase (tag &rest clauses)
  (let ((clauses-plus (append clauses (list `(true (error 'ecase "0xdeadc0de"))))))
    (translate-exp `(case ,tag ,@clauses-plus))))

(defun translate-typecase-tag (tipe)
  ;(format t "tipe= ~s~%" tipe)
  (case tipe
    (character `(char? %tag))
    (number `(number? %tag))
    (string `(string? %tag))
    (else `true)))

(def-cliiscm-translator typecase (tag &rest clauses)
  `(let ((%tag ,(translate-exp tag)))
     (cond ,@(let (output-clauses
                    (n (length clauses)))
               (dotimes (i n)
                 (let* ((clause (elt clauses i))
                        (clause-tag (car clause))
                        (clause-action (cdr clause))
                        output-clause)
                   (cond ((and (= i (- n 1)) (eq clause-tag 'true))
                          (setq output-clause
                                `(else ,@(translate-implicit-progn
                                           clause-action))))
                         (t
                           (setq output-clause
                                 `(,(translate-typecase-tag clause-tag)
                                    ,@(translate-implicit-progn clause-action)))))
                   (setq output-clauses (append output-clauses (list output-clause)))))
               output-clauses))))

(def-cliiscm-translator format (where how &rest whats)
  `(let ((%where ,(translate-exp where)))
     (cond ((or (eqv? %where false) (null? %where))
            (format ,(translate-exp how) ,@(mapcar #'translate-exp whats)))
           ((eqv? %where true)
            (printf ,(translate-exp how) ,@(mapcar #'translate-exp whats)))
           (else
            (fprintf %where ,(translate-exp how) ,@(mapcar #'translate-exp whats))))))

(def-cliiscm-translator load (f &rest ee)
  `(let* ((%f ,(translate-exp f))
          (%ee (list ,@(mapcar #'translate-exp ee)))
          (%if-does-not-exist ':error)
          (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
     (when %if-does-not-exist-from-user
       (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
     (cond ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
           (else (load %f)))))

(def-cliiscm-translator open (f &rest ee)
  `(let* ((%f ,(translate-exp f))
          (%ee (list ,@(mapcar #'translate-exp ee)))
          (%direction (memv ':direction %ee))
          (%if-exists (memv ':if-exists %ee))
          (%if-does-not-exist ':error)
          (%if-does-not-exist-from-user (memv ':if-does-not-exist %ee)))
     (when %direction
       (set! %direction (cadr %direction)))
     (when %if-exists
       (set! %if-exists (cadr %if-exists)))
     (when %if-does-not-exist-from-user
       (set! %if-does-not-exist (cadr %if-does-not-exist-from-user)))
     (cond ((eqv? %direction ':output)
            (when (and (eqv? %if-exists ':supersede) (file-exists? %f))
              (delete-file %f))
            (open-output-file %f))
           ((and (not %if-does-not-exist) (not (file-exists? %f))) false)
           (else (open-input-file %f)))))

(def-cliiscm-translator close (port)
  `(let ((%close-port-arg ,(translate-exp port)))
     ((if (input-port? %close-port-arg) close-input-port close-output-port)
      %close-port-arg)))

#|
(def-cliiscm-translator probe-file (f)
  `(let ((%probed-file ,(translate-exp f)))
     (if (file-exists? %probed-file) %probed-file false)))
|#

(def-cliiscm-translator with-open-file (open-args &rest body)
  (let ((wof-port (car open-args)))
    (translate-exp
      `(let* ((,wof-port (open ,@(cdr open-args)))
              (%with-open-file-res (progn ,@body)))
         (when ,wof-port
           ((if (input-port? ,wof-port)
                close-input-port
                close-output-port) ,wof-port))
         %with-open-file-res))))

#|
(def-cliiscm-translator with-output-to-string (wots-arg &rest body)
  (let ((wots-port (car wots-arg)))
    (unless (eq wots-port '*standard-output*)
      (error 'with-output-to-string "has to be to *standard-output*"))
    `(with-output-to-string (lambda () ,@(mapcar #'translate-exp body)))))
|#

(def-cliiscm-translator position (v s &rest ee)
  `(let ((%position-v ,(translate-exp v))
          (%position-s ,(translate-exp s))
          (%ee (list ,@(mapcar #'translate-exp ee))))
     (let ((%position-from-end (memv ':from-end %ee)))
       (when %position-from-end
         (set! %position-from-end (cadr %position-from-end)))
       (if (string? %position-s)
           ((if %position-from-end string-reverse-index string-index)
            %position-s %position-v)
           (list-position %position-v %position-s)))))

(def-cliiscm-translator nth (i ll)
  (translate-exp `(list-ref ,ll ,i)))

(def-cliiscm-translator assoc (v s &rest ee)
  `(assoc ,(translate-exp v) ,(translate-exp s)))

(def-cliiscm-translator member (v s &rest ee)
  `(member ,(translate-exp v) ,(translate-exp s)))

(def-cliiscm-translator delete (v s &rest ee)
  `(remove ,(translate-exp v) ,(translate-exp s)))

(def-cliiscm-translator floor (p &optional q)
  (if (null q)
      `(inexact->exact (floor ,p))
      `(quotient ,p ,q)))

(def-cliiscm-translator with-input-from-string (bdg &rest body)
  `(call-with-input-string
     ,(translate-exp (cadr bdg))
     (lambda (,(car bdg)) ,@(mapcar #'translate-exp body))))

(def-cliiscm-translator read (&rest ee)
  (if (null ee)
      `(read)
      `(let ((%read-res (read ,(translate-exp (car ee)))))
         (when (eof-object? %read-res)
             (set! %read-res ,(translate-exp (caddr ee))))
         %read-res)))

(def-cliiscm-translator read-char (&rest ee)
  (if (null ee)
      `(read-char)
      `(let* ((%read-char-port ,(translate-exp (car ee)))
              (%read-char-res (if %read-char-port
                                  (read-char %read-char-port)
                                  (read-char))))
         (when (eof-object? %read-char-res)
             (set! %read-char-res ,(translate-exp (caddr ee))))
         %read-char-res)))

(def-cliiscm-translator read-line (&rest ee)
  (if (null ee)
      `(read-line)
      `(let ((%read-line-res (read-line ,(translate-exp (car ee)))))
         (when (eof-object? %read-line-res)
             (set! %read-line-res ,(translate-exp (caddr ee))))
         %read-line-res)))

(def-cliiscm-translator push (v s)
  `(let ((%push-new-stack (cons ,(translate-exp v) ,(translate-exp s))))
     ,(translate-exp `(setf ,s %push-new-stack))))

(def-cliiscm-translator pushnew (v s &rest ee)
  `(let ((%push-added-value ,(translate-exp v))
         (%push-old-stack ,(translate-exp s)))
     (cond ((member %push-added-value %push-old-stack) %push-old-stack)
           (else ,(translate-exp
                    `(setf ,s (cons %push-added-value %push-old-stack)))))))

(def-cliiscm-translator pop (s)
  `(let* ((%pop-old-stack ,(translate-exp s))
          (%pop-top-value (car %pop-old-stack)))
     ,(translate-exp `(setf ,s (cdr %pop-old-stack)))
     %pop-top-value))

(def-cliiscm-translator length (s)
  `(let ((%length-arg ,(translate-exp s)))
     ((if (string? %length-arg) string-length length) %length-arg)))

(def-cliiscm-translator intern (s &optional p)
  (if (eq p :keyword)
      `(string->symbol (string-append ":" ,(translate-exp s)))
      `(string->symbol ,(translate-exp s))))
