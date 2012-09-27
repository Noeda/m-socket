;;;; m-socket.lisp

(in-package #:m-socket)

(defstruct address
  sockaddr
  sockaddr-len
  family)

(defconstant +default-recv-buffer-size+ 8192
             "The receive buffer size of array is not supplied to RECEIVE.")
(defconstant +default-max-send-buffer-size+ (* 8 8192)
             "The number of total bytes that can be in a send buffer. Not ~
             strictly enforced; usually some bytes can go overboard, but ~
             not in great extent.")

(defun extract-addresses-from-hostname-or-ip (hostname-or-ip port)
  (cffi:with-foreign-object (results :pointer)
    (setf errno 0)
    (let ((call-result
            (%get-addr-info hostname-or-ip
                            (format nil "~a" port)
                            (cffi:null-pointer)
                            results)))
      (let ((*results (cffi:mem-ref results :pointer)))

        (unless (cffi:null-pointer-p *results)
          (trivial-garbage:finalize results
            (lambda ()
              (%free-addr-info *results))))

        ;; Do it again if signal was delivered. getaddrinfo() has somewhat
        ;; more unusual semantics with signals and I'm pretty much just
        ;; guessing what might happen.
        (when (or (and (eql call-result :eai-system)
                       (eql errno :eintr))
                  (eql call-result :eai-intr))
          (return-from extract-addresses-from-hostname-or-ip
                       (extract-addresses-from-hostname-or-ip
                         hostname-or-ip port)))

        (let ((current *results))
          (loop until (cffi:null-pointer-p current) collect
            (let* ((sockaddr-len (cffi:foreign-slot-value
                                   current
                                   'addrinfo
                                   'ai-addrlen))
                   (sockaddr (cffi:foreign-alloc :uint8
                                                 :count
                                                 sockaddr-len))
                   (addr (make-address :sockaddr sockaddr
                                       :sockaddr-len sockaddr-len
                                       :family
                                       (cffi:foreign-slot-value
                                         current
                                         'addrinfo
                                         'ai-family))))
              (trivial-garbage:finalize addr
                (lambda ()
                  (cffi:foreign-free sockaddr)))
              (%memcpy sockaddr
                       (cffi:foreign-slot-value
                         current
                         'addrinfo
                         'ai-addr)
                       sockaddr-len)
              (setf current (cffi:foreign-slot-value
                              current
                              'addrinfo
                              'ai-next))
              addr)))))))

(defstruct send-packet
  arr
  len
  offset)

(defun socket-raw (socket)
  "Returns the raw file descriptor in socket object SOCKET.

The socket system assumes nobody else messes with the descriptor. Use with
care."
  (funcall socket :sock))

(defun %make-socket-object (raw-socket)
  ; what happens here what we lexically capture all the state required in
  ; an adlambda (which is an anaphoric dlambda, see m-util:dlambda).
  (let* ((recv-array)             ; Array that is used in receive if it is
                                  ; not supplied
         (raw-recv-buffer)        ; CFFI array cached in similar way
         (raw-recv-buffer-len 0)  ; Its length
         (raw-send-buffers)       ; A list of CFFI send buffers.
         (send-ready t)           ; T when flagged as writable
         (kill-when-buffers-empty nil) ; Should we die when buffers empty?
         (shutdown-when-buffers-empty nil) ; Should be shutdown when b e?
         (shutdown-how :shut-wr)           ; How should we shutdown then?
         (has-been-shutdown nil)           ; Are we already shutdown?
         (blocking nil)                    ; Is the socket blocking?
         (multiplexers (trivial-garbage:make-weak-hash-table
                         :weakness :key))  ; Table of I/O multiplexers we are
                                           ; in. (needed to automatically
                                           ; remove us from them if we die)
         (buffered-send-bytes 0)           ; How many bytes are currently
                                           ; buffered?
         (ob
           ; my god this is a long adlambda. I'd refactor it to separate
           ; functions if it didn't 'just work'. Kids, don't take any
           ; notes.
           (adlambda
             (:dead? () (= raw-socket -1))
             (:sock () raw-socket)
             (:maybe-push-arr (arr len offset)
               (if (< buffered-send-bytes +default-max-send-buffer-size+)
                 (progn
                   (self :push-arr arr len offset)
                   t)
                 nil))
             (:blocking? () blocking)
             (:set-blocking? (v)
                             (if v
                               (%set-to-blocking raw-socket)
                               (%set-to-nonblocking raw-socket)))
             (:remove-from-multiplexers ()
               (with-hash-table-iterator* (plex dummy multiplexers)
                 (remove-socket-from-io-multiplexer #'self plex))
               (clrhash multiplexers))
             (:multiplexers () multiplexers)
             (:unregister-multiplexer (plex)
               (remhash plex multiplexers))
             (:register-multiplexer (plex)
               (setf (gethash plex multiplexers) t))
             (:buffers-empty? () (null raw-send-buffers))
             (:decrease-send-buf (amount)
               (decf buffered-send-bytes amount))
             (:send-ready? () send-ready)
             (:set-send-ready () (setq send-ready t))
             (:set-not-send-ready () (setq send-ready nil))
             (:check-killings ()
               (when (null raw-send-buffers)
                 (when kill-when-buffers-empty
                   (raw-close-socket #'self))
                 (when shutdown-when-buffers-empty
                   (raw-shutdown-socket #'self))))
             (:flush-send-buffers ()
               (self :check-killings)
               (when (>= raw-socket 0)
                 (let* ((bufs (nreverse raw-send-buffers)))
                   (block buf-loop
                     (loop while bufs do
                       (let ((buf (pop bufs)))
                         (setf buf (send-raw-buf #'self buf))
                         (when buf
                           (push buf bufs)
                           (return-from buf-loop)))))
                   (setf raw-send-buffers (nreverse bufs))
                   (self :check-killings))))
             (:push-arr (arr len offset)
               (push (make-send-packet :arr arr
                                       :len len
                                       :offset offset)
                     raw-send-buffers)
               (incf buffered-send-bytes (- len offset)))
             (:recv-array ()
               (aif recv-array
                    recv-array
                    (progn
                      (setf recv-array (make-array
                                         +default-recv-buffer-size+
                                         :element-type '(integer 0 255)
                                         :adjustable nil))
                      recv-array)))
             (:raw-recv-buffer (at-least-length)
               (if (or (not raw-recv-buffer)
                       (< raw-recv-buffer-len at-least-length))
                 (let ((new-recv-buffer (cffi:foreign-alloc
                                          :uint8
                                          :count at-least-length)))
                   (trivial-garbage:finalize new-recv-buffer
                                             (lambda ()
                                               (cffi:foreign-free
                                                 new-recv-buffer)))
                   (setf raw-recv-buffer new-recv-buffer)
                   (setf raw-recv-buffer-len at-least-length)))
               raw-recv-buffer)
             (:raw-recv-buffer-len () raw-recv-buffer-len)
             (:kill-when-buffers-empty ()
               (setq kill-when-buffers-empty t)
               (when (self :buffers-empty?)
                 (self :kill)))
             (:shutdown-when-buffers-empty (how)
               (setq shutdown-when-buffers-empty t)
               (setq shutdown-how how)
               (when (self :buffers-empty?)
                 (self :shutdown)))
             (:shutdown ()
               (unless has-been-shutdown
                 (%shutdown raw-socket shutdown-how)
                 (setq has-been-shutdown t)))
             (:kill ()
               (self :remove-from-multiplexers)
               (%close raw-socket)
               (setq raw-socket -1)))))
    (trivial-garbage:finalize ob
          (lambda ()
            (unless (= raw-socket -1)
              (with-hash-table-iterator* (plex dummy multiplexers)
                (remove-socket-from-io-multiplexer-by-filedesc raw-socket plex))
              (%close raw-socket))))
    ob))

(defun %make-socket (family type protocol)
  (let* ((sock (%socket family type protocol))
         (sock-object (%make-socket-object sock)))
    (unless (= sock -1)
      (cffi:with-foreign-object (true-boolean :int)
        (setf (cffi:mem-ref true-boolean :int) 1)
        (%set-sock-opt sock :sol-socket :so-reuseaddr
                       true-boolean
                       (cffi:foreign-type-size :int)))
      (return-from %make-socket sock-object))
    nil))

(defun blocking? (socket)
  "Given a socket, returns if it is in blocking mode. By default all
sockets are in non-blocking mode.

Blocking is setf-able. You can write something like

    (setf (blocking? *socket*) t)

to set socket to blocking mode. Blocking mode affects the behaviour of
RECEIVE and SEND calls."
  (funcall socket :blocking?))

(defun set-blocking? (socket v)
  (funcall socket :set-blocking? v))

(defsetf blocking? set-blocking?)

(defmacro signal-proof (form)
  "Assuming FORM directly calls some syscall, checks if it sets errno to
EINTR. If the errno is EINTR, it repeats FORM. Good thing to realize for
you is probably that if FORM calls some procedure that has side-effects
then those are repeated as well."
  (with-gensyms (block-sym)
    `(block ,block-sym
        (loop do
          (setf errno :success)
          (let ((result ,form))
            (unless (eql errno :eintr)
              (return-from ,block-sym result)))))))

(defun %set-to-nonblocking (raw-sock)
  (let ((old-flags (%fcntl raw-sock :f-getfl 0)))
    (when (< old-flags 0)
      (error "I could not get socket flags with fcntl(): ~a" errno))
    (let ((result (%fcntl raw-sock :f-setfl (logior old-flags +o-nonblock+))))
      (when (< result 0)
        (error "I could not set socket flags with fcntl(): ~a" errno))
      t)))

(defun %set-to-blocking (raw-sock)
  (let ((old-flags (%fcntl raw-sock :f-getfl 0)))
    (when (< old-flags 0)
      (error "I could not get socket flags with fcntl(): ~a" errno))
    (let ((result (%fcntl raw-sock :f-setfl (logand old-flags
                                                    (lognot +o-nonblock+)))))
      (when (< result 0)
        (error "I could not set socket flags with fcntl(): ~a" errno))
      t)))

(defun flush-send-buffers (socket)
  "Attempts to send all queued data on the socket SOCKET. Always returns T.
If there was data and some of it were sent, I/O multiplexer will report
WRITE status for the socket when more data can be sent.

This call will not block, even if SOCKET is in blocking mode.

I/O multiplexer automatically calls this when it detects a socket is writable."
  (let ((old-blocking (blocking? socket)))
    (unwind-protect
      (progn
        (setf (blocking? socket) nil)
        (funcall socket :flush-send-buffers))
      (setf (blocking? socket) old-blocking)))
  t)


(defun make-listener-socket (listen-address port)
  "Makes a new listener socket that listens on address LISTEN-ADDRESS at
  port PORT.

  LISTEN-ADDRESS is a string. If it is not a numeric IP address then it is
  possible this call will block until the name can be resolved."
  (let ((addresses
          (extract-addresses-from-hostname-or-ip listen-address port))
        (errors (make-hash-table)))

    (when (null addresses)
      (error "No suitable addresses could be resolved for [~a]:~a"
             listen-address port))

    (dolist (address addresses)
      (multiple-value-bind (sockaddr sockaddr-len family)
          (values (address-sockaddr address)
                  (address-sockaddr-len address)
                  (address-family address))
        (setf errno :success)
        (let* ((sock (%make-socket family :sock-stream 0))
               (raw-sock (funcall sock :sock)))
          (when (and sock
                     (= 0 (signal-proof (%bind raw-sock sockaddr sockaddr-len)))
                     (= 0 (signal-proof (%listen raw-sock 100)))
                     (%set-to-nonblocking raw-sock))
            (return-from make-listener-socket sock))
          (unless (eql errno :success)
            (t! (gethash errno errors))))))
    (error "I cannot listen on this address. ([~a]:~a, errors ~a)"
           listen-address port
           (hash-table-keys-to-list errors))))

(defun accept (listener-socket)
  "Given a listening socket, returns a new socket if a connection was
  accepted and NIL if there were no new connections."
  (let ((new-sock (signal-proof (%accept
                                  (funcall listener-socket :sock)
                                  (cffi:null-pointer)
                                  (cffi:null-pointer)))))
    (when (= new-sock -1)
      (when (or (eql errno :ewouldblock)
                (eql errno :eagain))
        (return-from accept nil))
      (sys-error "accept() failed:"))
    (let ((socket-object (%make-socket-object new-sock)))
      (%set-to-nonblocking new-sock)
      socket-object)))

(defun close-socket (socket)
  "Closes socket. If socket has any data in send buffers, then those are
  sent before the socket is really closed. The sending happens in conjution
  with I/O multiplexer so this call will not block to send them."
  (funcall socket :kill-when-buffers-empty)
  t)

(defun raw-close-socket (socket)
  (funcall socket :kill)
  t)

(defun raw-shutdown-socket (socket)
  (funcall socket :shutdown)
  t)

(defun receive (socket &optional arr)
  "Given a socket, returns new data as an array. If the optional argument
  ARR is given, it is assumed to be an array and the data will be put there.

  Returns three values: the array, number of bytes read (which may be less
  than the size of the array) and the new status of the socket, which can be
  either :ALIVE or :CLOSED.

  If socket is in blocking mode, then this function will block if no data
  is immediately available. This is no guarantee that any specific amount of
  data will be read; only guarantee is that you won't be busylooping if you
  call RECEIVE repeatedly in a loop while in blocking mode.

  If ARR is given, it should be a simple array with element type of
  (integer 0 255) (or equivalent)."
  (when (and arr (= (length arr) 0))
    (return-from receive (values arr 0 :empty-array)))

  (let* ((raw-socket (funcall socket :sock))
         (arr (aif arr it (funcall socket :recv-array)))
         (raw-buffer (funcall socket :raw-recv-buffer (length arr)))
         (raw-buffer-len (funcall socket :raw-recv-buffer-len))
         (result (signal-proof
                   (%recv raw-socket
                          raw-buffer
                          (min raw-buffer-len
                               (length arr))
                          0))))
    (declare (type (simple-array (integer 0 255)) arr))

    (when (= result -1)
      (when (or (eql errno :ewouldblock)
                (eql errno :eagain))
        (return-from receive (values arr 0 :alive)))
      (raw-close-socket socket)
      (return-from receive (values arr 0 :closed)))

    (when (= result 0)
      (raw-close-socket socket)
      (return-from receive (values arr 0 :closed)))

    (dotimes (i result)
      (setf (aref arr i)
            (cffi:mem-aref raw-buffer :uint8 i)))

    (values arr result :alive)))

(declaim (inline convert-to-cffi-array))

(defun convert-to-cffi-array (arr)
  "Converts array ARR to CFFI array of same length. The element type of ARR
should be (integer 0 255) for best efficiency. This function has inline
turned on.

A finalizer that frees the CFFI array is attached to the resulting array."
  ; TODO: is there a faster way to magically turn a CFFI array into Lisp
  ; array? Maybe use larger units than :UINT8?
  (let ((new-array (cffi:foreign-alloc :uint8 :count (length arr))))
    (trivial-garbage:finalize new-array
                              (lambda ()
                                (cffi:foreign-free new-array)))
    (dotimes (i (length arr))
      (setf (cffi:mem-aref new-array :uint8 i)
            (aref arr i)))
    new-array))

(defun send (socket arr)
  "Sends the contents of ARR to socket. ARR must be a simple array with elements of octet type (i.e., (integer 0 255)).

  Returns one of the five possible statuses:

  :QUEUED   The contents of ARR were queued and will be sent later, when
            possible.
  :NOT-SENT Nothing of ARR were queued or sent. This usually means send
            buffers are full.
  :CLOSED   The socket connection was or is closed. The data may or may not
            have been sent.
  :PARTIALLY-SENT   Some of the data was immediately sent and the rest were
                    queued.
  :SENT     All of the data were sent immediately.

  If the return value is either :QUEUED, :PARTIALLY-SENT or :SENT, it means
  all the data is or will be sent eventually and you don't need to take any
  special action. In case of :NOT-SENT, you have to try to resend the
  contents later if you can.

  If the socket is in blocking mode, then this call will block until some data
  is sent. There is no guarantee all of ARR will be sent; the only guarantee
  is that you won't be busylooping if you call SEND in a loop. If there
  were data in the queues, then it will block until those queues are empty
  as well.

  The library queues a certain amount of data for you, if it can't be sent
  directly (the OS kernel also does this to some extent). These queues are
  flushed at I/O multiplexer wait calls. Currently, the queue size is 65535
  bytes but it can temporarily be slightly larger."
  (if (blocking? socket)
    (prog2
      ; when blocking, the flush part ensures the queue is empty at the
      ; end.
      (funcall socket :flush-send-buffers)
      (funcall
        (alambda ()
          (ecase (send-proper socket arr)
            (:not-sent (self))
            ((:queued :partially-sent :sent) :sent)
            (:closed :closed))))
      (funcall socket :flush-send-buffers))
    (send-proper socket arr)))

(defun send-proper (socket arr)
  (declare (type (simple-array (integer 0 255)) arr))

  (when (funcall socket :dead?)
    (return-from send-proper :closed))

  (when (= (length arr) 0)
    (return-from send-proper (if (funcall socket :buffers-empty?)
                        :sent
                        :queued)))

  (let ((raw-socket (funcall socket :sock))
        (arr-len (length arr))
        (arr (convert-to-cffi-array arr)))

    (funcall socket :flush-send-buffers)

    (unless (funcall socket :buffers-empty?)
      (return-from send-proper (if (funcall socket
                                            :maybe-push-arr arr arr-len 0)
                          :queued
                          :not-sent)))

    (let ((result (%send raw-socket arr arr-len 0)))
      (when (= result -1)
        (when (or (eql errno :eagain)
                  (eql errno :ewouldblock))
          (funcall socket :set-not-send-ready)
          (return-from send-proper (if (funcall socket
                                                :maybe-push-arr arr arr-len 0)
                              :queued
                              :not-sent)))
        (raw-close-socket socket)
        (return-from send-proper :closed))

      (funcall socket :set-send-ready)

      ; Send can indeed return 0 but it is not an error or a signal that
      ; 'it would block'.
      (when (= result 0)
        (funcall socket :check-killings)
        (return-from send-proper (if (funcall socket :dead?)
                                   :closed
                                   (if (funcall socket
                                                :maybe-push-arr arr arr-len 0)
                                     :queued
                                     :not-sent))))

      ; Whole thing was sent if this is true.
      (when (= result arr-len)
        (funcall socket :check-killings)
        (return-from send-proper (if (funcall socket :dead?)
                                   :closed
                                   :sent)))

      ; Partially sent
      (funcall socket :push-arr arr arr-len result)
      :partially-sent)))

(defun send-raw-buf (socket buf)
  (let ((raw-socket (funcall socket :sock)))
    (with-slots ((arr arr) (len len) (offset offset)) buf

      (when (= (- len offset) 0)
        (return-from send-raw-buf nil))

      (let ((result (%send raw-socket (cffi:inc-pointer arr offset)
                           (- len offset) 0)))
        (when (= result -1)
          (when (or (eql errno :eagain)
                    (eql errno :ewouldblock))
            (funcall socket :set-not-send-ready)
            (return-from send-raw-buf buf))
          (raw-close-socket socket)
          (return-from send-raw-buf buf))

        (funcall socket :set-send-ready)

        (when (= result 0)
          (return-from send-raw-buf (send-raw-buf socket buf)))

        (funcall socket :decrease-send-buf result)
        (when (= result (- len offset))
          (return-from send-raw-buf nil))
        (setf offset (+ offset result))
        (send-raw-buf socket buf)))))

(defun shutdown (socket &optional (how :shut-wr))
  "Shuts down socket. Think of BSD socket shutdown() function. HOW
  describes how to shut it down. :SHUT-WR indicates you are not going to
  write anything more to the socket. :SHUT-RD indicates you are not going
  to read anything more from the socket. :SHUT-RDWR indicates both.

  Shutting down the socket is sometimes necessary to ensure all data was
  sent through and acknowledged."
  (funcall socket :shutdown-when-buffers-empty how)
  t)


