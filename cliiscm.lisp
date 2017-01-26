":"; if test -z "$LISP"; then export LISP=ecl; fi
":"; if test "$LISP" = abcl; then exec abcl --load $0 --batch
":"; elif test "$LISP" = clisp; then exec clisp $0 -q
":"; elif test "$LISP" = clozure; then exec ccl -l $0 -b
":"; elif test "$LISP" = ecl; then exec ecl -shell $0
":"; elif test "$LISP" = mkcl; then exec mkcl -shell $0
":"; else test "$LISP" = sbcl; exec sbcl --script $0
":"; fi

#+sbcl
(declaim (sb-ext:muffle-conditions style-warning))

(setq *print-case* :downcase)

(defvar *cliiscm-version* "20170126") ;last change

(defvar *reading-source-file-p*)
(defvar *disallowed-calls*)
(defvar *source-file*)
(defvar *source-file-translated-p*)
(defvar *postprocessing*)

(defvar *defs-to-ignore*)

(defvar *setf-setters* (make-hash-table))

#+sbcl
(sb-alien:define-alien-routine system sb-alien:int (command sb-alien:c-string))

#+sbcl
(defun cliiscm-system (cmd) (system cmd))

#-sbcl
(defun cliiscm-system (cmd)
  #+abcl (ext:run-shell-command cmd)
  #+allegro (excl:shell cmd)
  #+clisp (ext:shell cmd)
  #+clozure (ccl::os-command cmd)
  #+cmucl (ext:run-program "sh" (list "-c" cmd) :output t)
  #+ecl (si:system cmd)
  #+mkcl (mkcl:system cmd))

(load (merge-pathnames "cliiscm-aliases" *load-pathname*))

(load (merge-pathnames "cliiscm-translators" *load-pathname*))

(defvar *files-to-be-ported*
  (with-open-file (i "cliiscm-files-to-be-ported.lisp" :direction :input)
    (let (s)
      (loop
        (let ((f (read i nil :eof-object)))
          (when (eq f :eof-object) (return))
          (push (string-downcase (symbol-name f)) s)))
      s)))

(defun translate-toplevel-source-exp-to-port (e o)
  (let ((res e))
    (when *reading-source-file-p*
      (setq res (translate-exp (nsublis *cliiscm-read-aliases* e))))
    (when (consp res)
      (pprint (nsublis *cliiscm-write-pre-aliases* res) o)
      (terpri o))))

(defun translate-port-to-port (i o)
  (loop
    (let ((x (read i nil :eof-object)))
      (when (eq x :eof-object) (return))
      (translate-toplevel-exp-to-port x o))))

(defun translate-file-to-port (f o)
  (with-open-file (i f :direction :input)
    (translate-port-to-port i o)))

(defun translate-source-file (o)
  (unless *source-file-translated-p*
    (setq *source-file-translated-p* t)
    (let ((*reading-source-file-p* t))
      (format o "~%;Translated from Common Lisp source ~a by CLiiScm v. ~a, ~a.~%~%"
              *source-file* *cliiscm-version*
              #+abcl :abcl
              #+clisp :clisp
              #+clozure :clozure
              #+cmucl :cmucl
              #+ecl :ecl
              #+mkcl :mkcl
              #+sbcl :sbcl
              )
      (translate-file-to-port *source-file* o))))

(defun defmacro-to-define-syntax (e)
  (destructuring-bind (mname params &rest body) (cdr e)
  `(define-syntax ,mname
     (lambda (%so)
       (datum->syntax %so
         (let ((%so-d (syntax->datum %so)))
           (apply (lambda ,params ,@body) (cdr %so-d))))))))

(defun note-down-defs (x)
  (when (consp x)
    (case (car x)
      ((define define-syntax defmacro)
       (let ((ad (cadr x)))
         (when (consp ad) (setq ad (car ad)))
         (push ad *defs-to-ignore*)))
      (begin
        (mapc #'node-down-defs (cdr x))))))

;(trace note-down-defs)

(defun translate-toplevel-exp-to-port (x o)
  (when (consp x)
    (let ((a (car x)))
      ;(format t "toplevel a = ~s~%" a)
      (cond (*reading-source-file-p*
              (cond ((eq a 'defmacro)
                     (translate-toplevel-source-exp-to-port (translate-exp x) o))
                    ((member a *disallowed-calls*) nil)
                    (t (translate-toplevel-source-exp-to-port x o))))
            (t (case a
                 (cliiscm-rename
                  (dolist (y (cdr x))
                    (push (cons (car y) (cadr y)) *cliiscm-read-aliases*)))
                 (cliiscm-rename-def
                   (dolist (y (cdr x))
                     (let ((new-name (cadr y)))
                       (push (cons (car y) new-name) *cliiscm-read-aliases*)
                       (push new-name *defs-to-ignore*))) )
                 (cliiscm-ignore-def
                   (dolist (name (cdr x))
                     (push name *defs-to-ignore*)))
                 (cliiscm-uncall
                   (dolist (proc-name (cdr x))
                     (push proc-name *disallowed-calls*)))
                 (cliiscm-defsetf
                   (dolist (y (cdr x))
                     (setf (gethash (car y) *setf-setters*) (cadr y))))
                 (cliiscm-insert
                   (dolist (y (cdr x))
                     (princ y o)))
                 (cliiscm-postamble
                   (translate-source-file o))
                 (cliiscm-postprocess
                   (setq *postprocessing*
                         (append *postprocessing* (cdr x))))
                 (t (note-down-defs x)
                    (when (eq a 'defmacro)
                      (setq x (defmacro-to-define-syntax x)))
                    (pprint (nsublis *cliiscm-write-pre-aliases* x) o)
                    (terpri o))))))))

;(trace translate-toplevel-exp-to-port)

(dolist (file-to-be-ported *files-to-be-ported*)
  (let ((*source-file* file-to-be-ported)
        (*postprocessing* nil)
        (*reading-source-file-p* nil)
        (*source-file-translated-p* nil)
        (user-override-file
          (let ((f (concatenate 'string "cliiscm-" file-to-be-ported)))
            (and (probe-file f) f)))
        (target-file
          (concatenate 'string "my-" file-to-be-ported)))
    (format t "Porting ~s to ~s...~%" file-to-be-ported target-file)
    (with-open-file (o target-file :direction :output :if-exists :supersede)
      (let ((*disallowed-calls* '())
            (*defs-to-ignore* '()))
        (when user-override-file
          (translate-file-to-port user-override-file o))
        ;(format t "defs-to-ignore = ~s~%" *defs-to-ignore*)
        (translate-source-file o)))
    (cliiscm-system (concatenate 'string "sed -i -e 's/\\<nil\\>/()/g' " target-file))
    (cliiscm-system (concatenate 'string "sed -i -e 's/#\\\\Newline/#\\\\newline/g' " target-file))
    (cliiscm-system (concatenate 'string "sed -i -e 's/#\\\\Return/#\\\\return/g' " target-file))
    (cliiscm-system (concatenate 'string "sed -i -e 's/#\\\\Tab/#\\\\tab/g' " target-file))
    (cliiscm-system (concatenate 'string "sed -i -e 's/#\\\\$/#\\\\ /' " target-file))
    ;(format t "postproc= ~s~%" *postprocessing*)
    (dolist (p *postprocessing*)
      (eval p))))

(quit)
