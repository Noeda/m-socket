;;;; package.lisp

(defpackage #:m-socket
  (:use #:cl #:m-util)
  (:export
    #:make-io-multiplexer
    #:add-socket-to-io-multiplexer
    #:remove-socket-from-io-multiplexer
    #:wait-for-events

    #:make-listener-socket
    #:accept
    #:close-socket
    #:shutdown

    #:socket-raw

    #:receive
    #:send

    #:flush-send-buffers

    #:blocking?

    ; gray streams
    #:socket-closed-error
    #:socket-queue-error
    #:socket-error
    #:socket-object
    #:make-socket-stream)
  (:documentation
    "See README.md in m-socket source directory."))


