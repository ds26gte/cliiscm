;name conversions

;last change 2017-01-07

(defvar *cliiscm-aliases-list*
  '(

    every andmap
    char-code char->integer
    char-int char->integer
    characterp char?
    char= char=?
    char< char<?
    char> char>?
    char<= char<=?
    char>= char>=?
    char-equal char-ci=?
    char-lessp char-ci<?
    char-greaterp char-ci>?
    char-not-greaterp char-ci<=?
    char-not-lessp char-ci>=?
    lower-case-p char-lower-case?
    upper-case-p char-upper-case?
    alpha-char-p char-alphabetic?
    digit-char-p char-numeric?
    complexp complex?
    princ display
    eq eq?
    equal equal?
    eql eqv?
    evenp even?
    probe-file file-exists?

    *standard-input* (current-input-port)
    *standard-output* (current-output-port)

    file-write-date file-or-directory-modify-seconds
    get-universal-time current-seconds

    force-output flush-output
    mapc for-each
    get-output-stream-string get-output-string

    eval eval1

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

    1+ add1
    1- sub1
    integerp integer?
    code-char integer->char
    elt list-ref
    mapcar map
    mod modulo
    minusp negative?
    terpri newline
    null null?
    numberp number?
    oddp odd?
    make-string-input-stream open-input-string
    make-string-output-stream open-output-string
    some ormap
    consp pair?
    plusp positive?
    functionp procedure?
    ;floor quotient
    realp real?
    rplaca set-car!
    rplacd set-cdr!
    sort sort!
    stringp string?
    string= string=?
    string< string<?
    string> string>?
    string<= string<=?
    string>= string>=?
    string-equal string-ci=?
    string-lessp string-ci<?
    string-greaterp string-ci>?
    string-not-greaterp string-ci<=?
    string-not-lessp string-ci>=?
    subseq substring
    char string-ref

    #+(and unix (or allegro clisp)) shell
    #+(and unix (or allegro clisp)) system

    #+(and unix clozure) ccl::os-command
    #+(and unix clozure) system

    #+(and (or unix darwin) ecl) si:system
    #+(and (or unix darwin) ecl) system

    #+abcl ext:run-shell-command
    #+abcl system

    dribble transcript-on
    dribble transcript-off
    vectorp vector?
    svref vector-ref
    values void
    prin1 write
    zerop zero?

    progn begin
    values list
    t true

    ))

(defvar *cliiscm-aliases* '())

(let ((ll *cliiscm-aliases-list*))
  (loop
    (when (null ll) (return))
    (push (cons (car ll) (cadr ll)) *cliiscm-aliases*)
    (pop ll)
    (pop ll)))
