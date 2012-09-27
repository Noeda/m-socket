;;;; io-multiplex-epoll.lisp
;;
;; I/O multiplexer using epoll(). epoll() is Linux specific
;; so all of this is Linux only.
;;
;; Accompanied by linux-grovel-file.lisp
;;

(in-package #:m-socket)

(cffi:defcfun (%epoll-create "epoll_create") :int
  (size :int))

(cffi:defcfun (%epoll-ctl "epoll_ctl") :int
  (epfd :int)
  (op epoll-op)
  (fd :int)
  (event (:pointer epoll-event)))

(cffi:defcfun (%epoll-wait "epoll_wait") :int
  (epfd :int)
  (events (:pointer epoll-event))
  (maxevents :int)
  (timeout :int))

(defconstant +event-cache-size+ 1000)

(defun make-io-multiplexer (&rest initial-sockets)
  (let ((epoll-fd (%epoll-create +event-cache-size+)))
    (when (= epoll-fd -1)
      (sys-error "I could not create an epoll() file:"))

    (let* ((event-structures
             (let ((struct (cffi:foreign-alloc 'epoll-event
                                               :count +event-cache-size+)))
               (trivial-garbage:finalize struct
                                         (lambda ()
                                           (cffi:foreign-free struct)))
               struct))
           (mapping (make-hash-table))
           (epoll-object
             (dlambda
               (:mapping () mapping)
               (:events ()
                 event-structures)
               (:kill ()
                 (%close epoll-fd)
                 (setq epoll-fd -1))
               (:epoll () epoll-fd))))
      (trivial-garbage:finalize epoll-object
                                (lambda ()
                                  (%close epoll-fd)))
      (mapc (lambda (s)
              (add-socket-to-io-multiplexer s epoll-object))
            (flatten initial-sockets))
      epoll-object)))

(defun add-socket-to-io-multiplexer (socket plex)
  (let ((sock-fd (funcall socket :sock))
        (epoll-fd (funcall plex :epoll)))
    (cffi:with-foreign-object (ev 'epoll-event)
      (cffi:with-foreign-slots ((events data) ev epoll-event)
        (setf events '(:epollin :epollout :epollerr
                       :epollrdhup :epollhup :epollet))
        (cffi:with-foreign-slots ((fd) data epoll-data)
          (setf fd sock-fd))
        (aif-nonzero (%epoll-ctl epoll-fd
                                 :epoll-ctl-add 
                                 sock-fd
                                 ev)
          (sys-error (format
                       nil
                       "I could not add a file descriptor (~a) to epoll (~a):"
                       sock-fd epoll-fd)))
        (funcall socket :register-multiplexer plex)
        (setf (gethash sock-fd (funcall plex :mapping)) socket)
        t))))

(defun remove-socket-from-io-multiplexer-by-filedesc (raw-socket plex)
  (remhash raw-socket (funcall plex :mapping))
  t)

(defun remove-socket-from-io-multiplexer (socket plex)
  (let ((sock-fd (funcall socket :sock))
        (epoll-fd (funcall plex :epoll)))
    (unless (= sock-fd -1) ; By checking for -1 we prevent a sys-error; it is
                           ; a common occurence to close the socket before
                           ; removing it from the multiplexer and we
                           ; should gracefully handle that case.
      (aif-nonzero (%epoll-ctl epoll-fd
                               :epoll-ctl-del
                               sock-fd
                               (cffi:null-pointer))
        (sys-error (format
                     nil
                     "I could not delete a file descriptor (~a) from ~
                     epoll (~a):"
                     sock-fd epoll-fd))))
    (funcall socket :unregister-multiplexer plex)
    (remhash sock-fd (funcall plex :mapping))
    t))

(defun map-ev-type (ev-type)
  (ecase ev-type
    (:epollin :read)
    (:epollout :write)
    (:epollrdhup :read)
    (:epollpri :read)
    (:epollhup :read)
    (:epollerr :read)))

(defun wait-for-events (plex timeout)
  ; epoll_wait() might be interrupted by a signal handler; however, the
  ; cost of returning without any events before timeout is usually small.
  ; I hope.
  (let ((result (%epoll-wait (funcall plex :epoll)
                             (funcall plex :events)
                             +event-cache-size+
                             (floor (* (max 0.0 timeout) 1000)))))
    (when (= result -1)
      (when (eql errno :eintr)
        (return-from wait-for-events nil))
      (sys-error "I could not check for new events with epoll_wait():"))

    ; TODO: use a cons cache instead of creating a fresh one each time
    (let* ((mapping (funcall plex :mapping))
           (events (funcall plex :events)))
      (loop for x from 0 below result collect
        (let* ((event (cffi:mem-aref events 'epoll-event x))
               (fd (cffi:foreign-slot-value
                    (cffi:foreign-slot-value event
                                             'epoll-event
                                             'data)
                    'epoll-data
                    'fd)))
          (let ((ob (gethash fd mapping)))
            (unless ob (error "Unexpected file descriptor (~a) in epoll." fd))
            (let ((events (cffi:foreign-slot-value
                            event 'epoll-event 'events)))
              (funcall ob :check-killings)
              (when (member :epollout events)
                (funcall ob :set-send-ready)
                (flush-send-buffers ob))
              (cons ob
                    (remove-duplicates
                      (mapcar (lambda (x) (map-ev-type x)) events)
                      :test #'eql)))))))))


