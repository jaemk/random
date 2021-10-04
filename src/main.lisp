(defpackage random
  (:use :cl :arrow-macros))
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
      (format stream "got invalid number range [~a,~a] range must be positive and start must be less than end" (start c) (end c)))))

(define-condition too-many-format-flags (error)
  ((flag-count
     :initarg :count
     :reader flag-count))
  (:report
    (lambda (c stream)
      (format stream "too many format-flags specified (~a)" (flag-count c)))))


(defun get-parse-int (pargvs name)
  (let ((str (cl-argparse:get-value name pargvs)))
      (handler-case
        (parse-integer str)

        (parse-error
          (c)
          (error 'invalid-number :str str :cause c)))))

(defun needs-formatting-p (pargvs)
  (or
    (cl-argparse:get-value "as-hex" pargvs)
    (cl-argparse:get-value "as-uhex" pargvs)
    (cl-argparse:get-value "as-b64" pargvs)
    (cl-argparse:get-value "as-ub64" pargvs)))


(defun format-bytes (bytes pargvs)
  (let* ((as-hex (cl-argparse:get-value "as-hex" pargvs))
         (as-uhex (cl-argparse:get-value "as-uhex" pargvs))
         (as-b64 (cl-argparse:get-value "as-b64" pargvs))
         (as-ub64 (cl-argparse:get-value "as-ub64" pargvs))
         (bytes (coerce bytes 'list))
         (format-flag-count (reduce
                              (lambda (acc b) (+ acc (if b 1 0)))
                              (list as-hex as-uhex as-b64 as-ub64)
                              :initial-value 0)))
    (log:debug
      "bytes: ~a, format flags: as-hex: ~a, as-uhex ~a, as-b64 ~a, as-ub64 ~a"
      bytes as-hex as-uhex as-b64 as-ub64)
    (->
      (cond
        ((> format-flag-count 1)
          (error 'too-many-format-flags :count format-flag-count))
        (as-hex
          (format nil "~(~{~2,'0x~}~)" bytes))
        (as-uhex
          (format nil "~:@(~{~2,'0x~}~)" bytes))
        (as-b64
          (cl-base64:usb8-array-to-base64-string (coerce bytes 'vector)))
        (as-ub64
          (cl-base64:usb8-array-to-base64-string (coerce bytes 'vector) :uri t))
        (t
         (format nil "~{~a ~}" bytes))))))


(defun num-bytes-for-number (n)
    (-> n
        (+ 1)   ; - so we don't have to handle 'zero'
                ;   and so math is done correctly, e.g.
                ;   (log 256 2) = 8, but should be 9
                ;   to be 2 bytes instead of 1
        (log 2) ; - number of bits
        (/ 8)   ; - number of bytes, round up
        (ceiling)))


(defun int->bytes (n)
  (let* ((n-bytes (num-bytes-for-number n))
         (offsets (or (loop for i from 0 to (- n-bytes 1) collect (* 8 i)) '(0)))
         (bytes (map 'list (lambda (offset) (ldb (byte 8 offset) n)) offsets)))
    (log:debug "number: ~a -> ~a bytes -> offsets ~a:  ~a" n n-bytes offsets bytes)
    (coerce bytes 'vector)))


(defun generate-number (pargvs)
  (let ((start (get-parse-int pargvs "number-start"))
        (end (get-parse-int pargvs "number-end")))
    (if (or (not (< start end)) (< start 0) (< end 0))
        (error 'invalid-number-range :start start :end end)
        (->> (- end start)  ;; - get the range starting from 0
             (+ 1)          ;; - strong-random's upper bound is exclusive,
                            ;;   but we want our range to be inclusive
             (crypto:strong-random)
             (+ start)      ;; - add back the starting offset
             ((lambda (n)
                (if (needs-formatting-p pargvs)
                  (format-bytes (int->bytes n) pargvs)
                  n)))
             (format t "~a~%")))))


(defun generate-uuid (pargvs)
  (let ((id (uuid::make-v4-uuid)))
    (->>
      (if (needs-formatting-p pargvs)
        (format-bytes (uuid:uuid-to-byte-array id) pargvs)
        id)
      (format t "~a~%"))))


(defun generate-bytes (pargvs)
   (let* ((c (get-parse-int pargvs "count"))
          (bytes (crypto:random-data c)))
    (->> (format-bytes bytes pargvs)
         (format t "~a~%"))))


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
    (cl-argparse:add-default uuid
                             :var "func"
                             :default #'generate-uuid)))


(defun create-generate-bytes-parser ()
  (cl-argparse:create-sub-parser (bytes "generate random bytes")
    (cl-argparse:add-optional bytes
                              :short "n"
                              :long "count"
                              :help "the number of bytes to generate"
                              :default "10"
                              :var "count")
    (cl-argparse:add-default bytes
                             :var "func"
                             :default #'generate-bytes)))


(defun create-parser ()
  (cl-argparse:create-main-parser (main-parser "generate random things")
    (cl-argparse:add-subparser main-parser (create-generate-number-parser))
    (cl-argparse:add-subparser main-parser (create-generate-uuid-parser))
    (cl-argparse:add-subparser main-parser (create-generate-bytes-parser))
    (cl-argparse:add-flag main-parser
                          :short "x"
                          :long "hex"
                          :help "hex encode the resulting bytes, lowercase"
                          :var "as-hex")
    (cl-argparse:add-flag main-parser
                          :short "X"
                          :long "hex-upper"
                          :help "hex encode the resulting bytes, uppercase"
                          :var "as-uhex")
    (cl-argparse:add-flag main-parser
                          :short "b"
                          :long "base64"
                          :help "base64 encode the resulting bytes"
                          :var "as-b64")
    (cl-argparse:add-flag main-parser
                          :short "B"
                          :long "uri-base64"
                          :help "base64 encode the resulting bytes (uri safe)"
                          :var "as-ub64")
    (cl-argparse:add-default main-parser
                             :var "func"
                             :default #'generate-bytes)))


(defun to-log-level (s)
  (cond
    ((string-equal s "TRACE") :trace)
    ((string-equal s "DEBUG") :debug)
    ((string-equal s "INFO") :info)
    ((string-equal s "WARN") :warn)
    ((string-equal s "ERROR") :error)
    ((string-equal s "FATAL") :fatal)
    (t :error)))

(defun get-log-level ()
  (-> (sb-ext:posix-getenv "LOG_LEVEL")
      ((lambda (e) (if e (string-upcase e) nil)))
      (str:trim)
      (to-log-level)))

(defun main (argvs)
  (handler-case
    (progn
      (log:config (get-log-level))
      (log:config :sane2)
      (log:config :nofile)
      (log:debug "args: ~a" argvs)
      (let ((pargvs (cl-argparse:parse (create-parser) (cdr argvs))))
        (funcall (cl-argparse:get-value "func" pargvs) pargvs)))

    ;; cli args error
    (cl-argparse:cancel-parsing-error
      (e)
      (progn
        (format *error-output* "~a~%" e)
        (sb-ext:quit :unix-status 1)))

    ;; C-c
    (sb-sys:interactive-interrupt
      ()
      (progn
        (format t "~&Aborting...~%")
        (sb-ext:quit :unix-status 1)))

    ;; everything else
    (error
      (e)
      (progn
        (format *error-output* "~&Error: ~a~%" e)
        (sb-ext:quit :unix-status 1)))
    ))


;; handle any errors if they aren't cause by the catch-all handler in 'main
(setf
  *debugger-hook*
  (lambda (c old-hook)
    (declare (ignore old-hook))
    (format *error-output* "~&Unhandled error: ~a~%" c)
    (sb-ext:quit :unix-status 1)))

