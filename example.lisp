(eval-when (:compile-toplevel :execute :load-toplevel)
  (require 'asdf))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (asdf:operate 'asdf:load-op "m-socket"))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (shadow 'socket-error)
  (shadow 'shutdown)
  #+ccl(shadow 'accept-connection)
  (use-package 'm-socket))

(defparameter *listener* (make-listener-socket "0.0.0.0" 5000))
(defparameter *plex* (make-io-multiplexer *listener*))
(defparameter *streams* (make-hash-table))

(defun accept-connection ()
  (let ((new-socket (accept *listener*)))
    (when new-socket
      (setf (gethash new-socket *streams*)
            (make-socket-stream new-socket))
      (add-socket-to-io-multiplexer new-socket *plex*))))


(defun echo-back (stream)
  (let ((arr (make-array 100 :element-type '(integer 0 255))))
    (declare (dynamic-extent arr))
    (loop
      (let ((bytes (read-sequence arr stream)))
        (if (> bytes 0)
          (handler-case
            (progn
              (format t "tatti ~a~%" bytes)
              (write-sequence arr stream :end bytes)
              (force-output stream))
            (socket-error ()
              (obliterate-socket stream)))
          (progn
            (when (not (open-stream-p stream))
              (obliterate-socket stream))
            (return)))))))

(defun obliterate-socket (stream)
  (remhash (socket-object stream) *streams*)
  (close stream))

(loop
  (let ((events (wait-for-events *plex* 5.0)))
    (dolist (e events)
      (let ((socket (car e))
            (etype (cdr e)))

        (when (member :read etype)
          (if (eq socket *listener*)
            (accept-connection)
            (echo-back (gethash socket *streams*))))))))

