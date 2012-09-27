;;; The purpose of this file is to simply attach documentation to I/O
;;; multiplexer functions.

(in-package #:m-socket)

(setf (documentation 'wait-for-events 'function)
      "Given an I/O multiplexer at PLEX, examines the state of all sockets
added to the multiplexer and returns a list of pairs with each pair
having car as the socket that became ready and cdr as the type of
readiness.

TIMEOUT is in seconds and this function clamps it between 0 and infinity.

The types of readiness are :WRITE and :READ. :WRITE means you probably can
call M-SOCKET:SEND on the socket and some of your data will go through.
:READ means you probably can call M-SOCKET:RECEIVE on the socket and
receive some data or in the case of a listening socket, accept a new
connection. It is in some cases possible that the events lie.

This call may also flush send buffer queues.

Sockets that are disconnected or closed are automatically removed from the
I/O multiplexer.")

(setf (documentation 'make-io-multiplexer 'function)
      "Creates a new I/O multiplexer object.

The purpose of an I/O multiplexer is to efficiently check the status of
many sockets at once. The backend of I/O multiplexer uses epoll on Linux
and kqueue on FreeBSD.")

(setf (documentation 'add-socket-to-io-multiplexer 'function)
      "Adds socket SOCKET to I/O multiplexer PLEX.

If the socket is already in the multiplexer, an error may be thrown.")

(setf (documentation 'remove-socket-from-io-multiplexer 'function)
      "Removes socket SOCKET from I/O multiplexer PLEX.

If the socket isn't already in the multiplexer, an error may be thrown.")


