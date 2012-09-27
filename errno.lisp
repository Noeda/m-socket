;;;; errno.lisp
;;
;; Raw, ugly, error handling stuff.
;;

(in-package #:m-socket)

(define-condition sys-error (error)
  ((msg :initarg :msg :reader msg)
   (error-code :initarg :error-code :reader error-code))
  (:documentation
    "Thrown when a system call fails. The slot MSG contains ~
     the message and ERROR-CODE contains errno or getaddrinfo error value."))

(defmethod print-object ((se sys-error) stream)
  (print-unreadable-object (se stream :type t)
    (format stream "~a ~a" (error-code se) (msg se))))

;; Will this always choose the POSIX strerror_r over the GNU one? I hope
;; so. At least it should be noticeable whenever it uses the wrong one.
(cffi:defcfun ("strerror_r" %strerror-r) :int
  (errnum errno-code)
  (strerrbuf (:pointer :char))
  (buflen size-t))

(declaim (inline %memzero %memcpy))

(defun %memzero (ptr bytes)
  (dotimes (i bytes)
    (setf (cffi:mem-aref ptr :uint8 i) 0)))

(defun %memcpy (ptr1 ptr2 bytes)
  (dotimes (i bytes)
    (setf (cffi:mem-aref ptr1 :uint8 i)
          (cffi:mem-aref ptr2 :uint8 i))))

(defun get-errno-string ()
  (cffi:with-foreign-pointer-as-string (str 256)

    (%memzero str 256)

    (%strerror-r errno str 255)
    str))

(defun get-gai-error-string (gai-code)
  ; We could use gai_strerror here. However, it appears it is definitely
  ; thread safe everywhere. There are not that many error codes so we can
  ; manually handle the common cases
  (macrolet ((check-code (&rest codes)
               `(cond
                  ,@(mapcar (lambda (x)
                              `((eql gai-code ,(first x))
                                (return-from get-gai-error-string ,(second x))))
                            codes))))
    (check-code (:eai-again "Temporary failure in name resolution. (EAI_AGAIN)")
                (:eai-badflags "Invalid value for 'ai_flags' field of addrinfo structure. ~
                                (EAI_BADFLAGS)")
                (:eai-badhints "Invalid value for 'hints' in getaddrinfo(). ~
                                (EAI_BADHINTS)")
                (:eai-fail "Non-recovable error in name resolution. ~
                            (EAI_FAIL)")
                (:eai-family "Protocol family is not supported. ~
                               (EAI_FAMILY)")
                (:eai-memory "Memory allocation failure. ~
                               (EAI_MEMORY)")
                (:eai-noname "Hostname or service name not provided or unknown. ~
                               (EAI_NONAME)")
                (:eai-overflow "Argument buffer overflow. (EAI_OVERFLOW)")
                (:eai-protocol "Resolved protocol is unknown. (EAI_PROTOCOL)")
                (:eai-service "Service name not supported for socktype. (EAI_SERVICE)")
                (:eai-socktype "'ai_socktype' not supported. (EAI_SOCKTYPE)")
                (:eai-system "System error, check errno. (EAI_SYSTEM)")
                (:eai-intr "Signal interrupted the call. (EAI_INTR)"))
    (format nil "Unknown EAI error with code ~a." gai-code)))

(defun sys-error (msg)
  (error 'sys-error
         :msg (format nil "~a ~a" msg (get-errno-string))
         :error-code errno))

(defun sys-gai-error (msg gai)
  (error 'sys-error :msg (format nil "~a ~a" msg (get-gai-error-string gai))
         :error-code gai))


