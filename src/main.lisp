(defpackage random
  (:use :cl :cl-argparse :arrow-macros :uuid))
(in-package :random)


(define-condition invalid-number (error)
  ((str
     :initarg :str
     :reader str
    )
   (cause
     :initarg :cause
     :reader cause
    ))
  (:report
    (lambda (c stream)
      (format stream "got invalid number ~a, cause: ~a" (str c) (cause c)))))


(define-condition invalid-number-range (error)
  ((start
     :initarg :start
     :reader start
     )
   (end
     :initarg :end
     :reader end
     ))
  (:report
    (lambda (c stream)
      (format stream "got invalid number range [~a,~a] start must be less than end" (start c) (end c)))))


(defun get-parse-int (pargvs name)
  (let ((str (cl-argparse:get-value name pargvs)))
      (handler-case
        (parse-integer str)

        (parse-error
          (c)
          (error 'invalid-number :str str :cause c)))))


(defun generate-number (pargvs)
  (let ((start (get-parse-int pargvs "number-start"))
        (end (get-parse-int pargvs "number-end")))
    (if (not (< start end))
        (error 'invalid-number-range :start start :end end)
        (->> (- end start)  ;; - get the range starting from 0
             (+ 1)          ;; - strong-random's upper bound is exclusive,
                            ;;   but we want our range to be inclusive
             (crypto:strong-random)
             (+ start)      ;; - add back the starting offset
             (format t "~a~%")))))


(defun generate-uuid (pargvs)
  (let ((hex (cl-argparse:get-value "as-hex" pargvs))
        (lower (cl-argparse:get-value "as-lower" pargvs))
        (upper (cl-argparse:get-value "as-upper" pargvs))
        (id (uuid::make-v4-uuid)))
    (->> id
         ((lambda (id) (if hex (format nil "~{~x~}" (coerce (uuid:uuid-to-byte-array id) 'list)) id)))
         ((lambda (id) (if lower (format nil "~(~a~)" id) id)))
         ((lambda (id) (if upper (format nil "~:@(~a~)" id) id)))
         (format t "~a~%")
         )))


(defun create-generate-number-parser ()
  (cl-argparse:create-sub-parser (number "generate a random number")
    (cl-argparse:add-optional number
                              :short "s"
                              :long "start"
                              :help "the starting value of the range in which to generate (inclusive)"
                              :default "0"
                              :var "number-start")
    (cl-argparse:add-optional number
                              :short "e"
                              :long "end"
                              :help "the ending value of the range in which to generate (inclusive)"
                              :default "10"
                              :var "number-end")
    (cl-argparse:add-default number
                             :var "func"
                             :default #'generate-number)))


(defun create-generate-uuid-parser ()
  (cl-argparse:create-sub-parser (uuid "generate a uuid")
    (cl-argparse:add-flag uuid
                          :short "x"
                          :long "hex"
                          :help "hex encode the resulting uuid"
                          :var "as-hex")
    (cl-argparse:add-flag uuid
                          :short "l"
                          :long "lower"
                          :help "lowercase the resulting uuid"
                          :var "as-lower")
    (cl-argparse:add-flag uuid
                          :short "u"
                          :long "upper"
                          :help "uppercase the resulting uuid"
                          :var "as-upper")
    (cl-argparse:add-default uuid
                             :var "func"
                             :default #'generate-uuid)))


(defun create-parser ()
  (cl-argparse:create-main-parser (main-parser "generate random things")
    (cl-argparse:add-subparser main-parser (create-generate-number-parser))
    (cl-argparse:add-subparser main-parser (create-generate-uuid-parser))
    ))


(defun main (argvs)
  (handler-case
      (let ((pargvs (cl-argparse:parse (create-parser) (cdr argvs))))
        (funcall (cl-argparse:get-value "func" pargvs) pargvs))

    ;; cli args error
    (cl-argparse:cancel-parsing-error
      (e)
      (progn
        (format *error-output* "~a~%" e)
        (sb-ext:quit :unix-status 1)
        ))

    ;; C-c
    (sb-sys:interactive-interrupt
      ()
      (progn
        (format t "~&Aborting...~%")
        (sb-ext:quit :unix-status 1)
        ))

    ;; everything else
    (error
      (e)
      (progn
        (format *error-output* "~&Error: ~a~%" e)
        (sb-ext:quit :unix-status 1)
        ))))


;; handle any errors if they aren't cause by the catch-all handler in 'main
(setf
  *debugger-hook*
  (lambda (c old-hook)
    (declare (ignore old-hook))
    (format *error-output* "~&Unhandled error: ~a~%" c)
    (sb-ext:quit :unix-status 1)))


;; (as-> (crypto:random-data 4) v (coerce v 'list) (format nil "~(~{~x~}~)" v))

