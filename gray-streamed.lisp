;;;; gray-streamed.lisp
;;
;; This file implements a gray stream wrapping around the raw sockets.
;;
;; Using gray streams is often more convenient than otherwise.
;;

(in-package #:m-socket)

(defclass socket-stream
    (trivial-gray-streams:fundamental-binary-input-stream
     trivial-gray-streams:fundamental-binary-output-stream
     trivial-gray-streams:trivial-gray-stream-mixin)
  ((socket-object :initarg :socket-object
                  :reader socket-object
                  :documentation "Given a socket stream, returns the socket ~
                  object associated with it.")
   (killed-once :initform nil :accessor killed-once)))

(defun make-socket-stream (socket)
  "Makes a stream out of a socket. This requires Gray Streams support from
the hosting Lisp implementation. The stream will be a binary stream and unit
is an unsigned byte (i.e., an octet).

Be aware that socket streams in this library don't exactly behave like
other streams if the socket is in non-blocking mode (the default). The most
notable traits are as follows.

  - READ-BYTE and READ-SEQUENCE don't block. If there's no data, then
  READ-BYTE behaves as if EOF condition was reached and READ-SEQUENCE as if
  not a single byte was read. This may have implications when, for example,
  you read lines with READ-LINE (e.g. with flexi-streams) and a full line did   not arrive at once.
  - WRITE-BYTE and WRITE-SEQUENCE throw SOCKET-ERROR if send queues are
  full or the socket is closed. You can catch SOCKET-CLOSED-ERROR or
  SOCKET-QUEUE-ERROR (these are subclasses from SOCKET-ERROR) to
  distinguish between the conditions.

  The above issues don't exist if the socket is in blocking mode. However,
  the following issues apply to both blocking and non-blocking sockets:

  - LISTEN is not implemented. You will get an error if you try to use it.
  - READ-SEQUENCE and WRITE-SEQUENCE only take simple arrays of element
  type (integer 0 255) as arguments.
  - SOCKET-CLOSED-ERROR is still raised if the socket is closed in a
  writing operation.

  You may think to use flexi-streams but be aware that the non-blocking
  behaviour may break it in some cases such as when you read lines or when
  a UTF-8 sequence is broken in the middle. Flexi-streams will think the
  stream ended and even if you catch EOF errors, you may lose data. If you
  can, use blocking sockets with flexi-streams.

  Use I/O multiplexer to know if you (may) be able to read or write to the
  socket."
  (make-instance 'socket-stream :socket-object socket))

(define-condition socket-error (error)
  ((socket-stream :initarg :socket-stream
                  :reader socket-stream))
  (:documentation
    "This condition is raised when a write operation is attempted on a ~
    stream representing a socket that has been closed or is not able to ~
    queue more data to be sent. The stream can be accessed from this ~
    condition through reader function SOCKET-STREAM."))

(define-condition socket-closed-error (socket-error)
  ()
  (:documentation
    "See SOCKET-ERROR. This condition is raised when a writing operation was ~
    attempted on a socket that is closed."))

(define-condition socket-queue-error (socket-error)
  ()
  (:documentation
    "See SOCKET-ERROR. This condition is raised when a writing operation was ~
    attempted on a socket whose send queues are full."))

(defmethod trivial-gray-streams:stream-listen ((s socket-stream))
  ; I feel bad for rubbing this kind of error message in the face of users.
  ; I was not able to figure out a trivial implementation for LISTEN here
  ; that is portable, follows the correct LISTEN behaviour and plays nice
  ; with the rest of the system.
  (error "Socket stream LISTEN is not implemented. Use I/O multiplexer ~
         system instead to check if the stream is ready."))

(defmethod close ((s socket-stream) &key abort)
  (declare (ignore abort))
  (let ((so (socket-object s)))
    (cond
      ((killed-once s) nil)
      ((funcall so :dead?) nil)
      (t (close-socket (socket-object s))
         (setf (killed-once s) t)
         t))))

(defmethod open-stream-p ((s socket-stream))
  (let ((so (socket-object s)))
    (cond
      ((killed-once s) nil)
      ((funcall so :dead?) nil)
      (t t))))

(defmethod trivial-gray-streams:stream-force-output ((s socket-stream))
  (let ((so (socket-object s)))
    (flush-send-buffers so))
  t)

(defmethod trivial-gray-streams:stream-read-byte ((s socket-stream))
  (let ((so (socket-object s))
        (temp-arr (make-array 1 :element-type '(integer 0 255)
                                :adjustable nil)))
    (declare (dynamic-extent temp-arr))
    (multiple-value-bind (ignored bytes status) (receive so temp-arr)
      (declare (ignore ignored status))
      (if (= bytes 1)
        (aref temp-arr 0)
        :eof))))

(defmethod trivial-gray-streams:stream-write-byte ((s socket-stream) i)
  (assert (and (>= i 0) (<= i 255)))

  (let ((so (socket-object s))
        (temp-arr (make-array 1 :element-type '(integer 0 255)
                                :adjustable nil)))
    (declare (dynamic-extent temp-arr))
    (setf (aref temp-arr 0) i)

    (case (send so temp-arr)
      ((:queued :partially-sent :sent) i)
      ; There doesn't seem to be a standard way to signal an error when
      ; writing to a closed stream. Or is there? Maybe I just didn't look
      ; hard enough.
      (otherwise (error 'socket-closed-error :socket-stream s)))))

(defmethod trivial-gray-streams:stream-read-sequence ((s socket-stream)
                                                      arr
                                                      start
                                                      end
                                                      &key)
  (declare (type (simple-array (integer 0 255)) arr))

  (let ((narr (if (and (= start 0)
                       (or (eq end nil)
                           (= end (length arr))))
                arr
                (make-array (- end start) :element-type '(integer 0 255)))))
    (declare (dynamic-extent narr))

    (let ((so (socket-object s)))
      (multiple-value-bind (ignored bytes status)
          (receive so narr)
        (declare (ignore ignored status))
        (if (= bytes 0)
          start
          (progn
            (unless (eq narr arr)
              (setf (subseq arr start end) narr))
            (if (< bytes (- end start))
              (trivial-gray-streams:stream-read-sequence
                s arr :start (+ start bytes) :end end)
              (+ start bytes))))))))

(defmethod trivial-gray-streams:stream-write-sequence ((s socket-stream)
                                                       arr
                                                       start
                                                       end
                                                       &key)
  (declare (type (simple-array (integer 0 255)) arr))
  (let ((narr (if (and (= start 0)
                       (or (eql end nil)
                           (= end (length arr))))
                arr
                (let ((ma
                        (make-array (- end start)
                                    :element-type '(integer 0 255))))
                  (setf (subseq ma 0 (length ma))
                        (subseq arr start (aif end it (length arr))))
                  ma))))
    (declare (dynamic-extent narr))

    (let* ((so (socket-object s)))
      (ecase (send so narr)
        ((:queued :partially-sent :sent) arr)
        (:closed (error 'socket-closed-error
                          :socket-stream s))
        (:not-sent (error 'socket-queue-error
                          :socket-stream s))))))

