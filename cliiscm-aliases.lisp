;name conversions

;last change 2017-01-18

(defvar *cliiscm-read-aliases-list*
  '(

    *standard-input* (current-input-port)
    *standard-output* (current-output-port)
    1+ add1
    1- sub1
    ;floor quotient
    alpha-char-p char-alphabetic?
    char string-ref
    char-code char->integer
    char-equal char-ci=?
    char-greaterp char-ci>?
    char-int char->integer
    char-lessp char-ci<?
    char-not-greaterp char-ci<=?
    char-not-lessp char-ci>=?
    char< char<?
    char<= char<=?
    char= char=?
    char> char>?
    char>= char>=?
    characterp char?
    code-char integer->char
    complexp complex?
    consp pair?
    digit-char-p char-numeric?
    dribble transcript-off
    dribble transcript-on
    elt list-ref
    eq eq?
    eql eqv?
    equal equal?
    eval eval1
    evenp even?
    every andmap
    file-write-date file-or-directory-modify-seconds
    force-output flush-output
    functionp procedure?
    get-output-stream-string get-output-string
    get-universal-time current-seconds
    integerp integer?
    lower-case-p char-lower-case?
    make-string-input-stream open-input-string
    make-string-output-stream open-output-string
    mapc for-each
    mapcar map
    minusp negative?
    mod modulo
    null null?
    numberp number?
    oddp odd?
    plusp positive?
    prin1 write
    princ display
    probe-file file-exists?
    progn begin
    realp real?
    rplaca set-car!
    rplacd set-cdr!
    some ormap
    sort sort!
    string-equal string-ci=?
    string-greaterp string-ci>?
    string-lessp string-ci<?
    string-not-greaterp string-ci<=?
    string-not-lessp string-ci>=?
    string< string<?
    string<= string<=?
    string= string=?
    string> string>?
    string>= string>=?
    stringp string?
    subseq substring
    svref vector-ref
    t true
    terpri newline
    upper-case-p char-upper-case?
    values list
    vectorp vector?
    zerop zero?

    ;quasiquote

    #+(or ecl mkcl) si:quasiquote
    #+(or ecl mkcl) quasiquote

    ;unquote

    #+(or ecl mkcl) si:unquote
    #+(or ecl mkcl) unquote

    ;unquote-splice

    #+(or ecl mkcl) si:unquote-splice
    #+(or ecl mkcl) unquote-splicing

    ;getenv

    #+allegro system::getenv
    #+allegro getenv

    #+ecl si:getenv
    #+ecl getenv

    #+mkcl mkcl:getenv
    #+mkcl getenv

    #+clisp ext:getenv
    #+clisp getenv

    #+sbcl sb-ext:posix-getenv
    #+sbcl getenv

    #+clozure ccl::getenv
    #+clozure getenv

    #+abcl ext:getenv
    #+abcl getenv

    ;system

    #+(and unix (or allegro clisp)) shell
    #+(and unix (or allegro clisp)) system

    #+(and unix clozure) ccl::os-command
    #+(and unix clozure) system

    #+(and (or unix darwin) ecl) si:system
    #+(and (or unix darwin) ecl) system

    #+abcl ext:run-shell-command
    #+abcl system

    #+mkcl mkcl:system
    #+mkcl system

    ))

(defvar *cliiscm-read-aliases* '())

(let ((ll *cliiscm-read-aliases-list*))
  (loop
    (when (null ll) (return))
    (push (cons (car ll) (cadr ll)) *cliiscm-read-aliases*)
    (pop ll)
    (pop ll)))

(defvar *cliiscm-write-pre-aliases-list*
  '(

    #+ecl si:quasiquote
    #+ecl quasiquote

    #+ecl si:unquote
    #+ecl unquote

    #+ecl si:unquote-splice
    #+ecl unquote-splicing

    ))

(defvar *cliiscm-write-pre-aliases* '())

(let ((ll *cliiscm-write-pre-aliases-list*))
  (loop
    (when (null ll) (return))
    (push (cons (car ll) (cadr ll)) *cliiscm-write-pre-aliases*)
    (pop ll)
    (pop ll)))
