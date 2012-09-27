;;;; socket-cffi-bindings.lisp
;;
;; This file contains raw bindings to CFFI.
;;
;; Data structures and constants are grovelled, see
;; unix-grovel-file.lisp
;;

(in-package #:m-socket)

(cffi:defcfun (%socket "socket") :int
  (domain address-family)
  (type socktype)
  (protocol :int))

(cffi:defcfun (%close "close") :int
  (filedesc :int))

(cffi:defcfun (%bind "bind") :int
  (filedesc :int)
  (addr :pointer)
  (addrlen socklen-t))

(cffi:defcfun (%listen "listen") :int
  (filedesc :int)
  (backlog :int))

(cffi:defcfun (%get-addr-info "getaddrinfo") gai-code
  (hostname :string)
  (servname :string)
  (hints (:pointer addrinfo))
  (res (:pointer (:pointer addrinfo))))

(cffi:defcfun (%free-addr-info "freeaddrinfo") :void
  (res (:pointer addrinfo)))

(cffi:defcfun (%fcntl "fcntl") :int
  (filedesc :int)
  (cmd fcntl-cmd)
  (flags :int))

(cffi:defcfun (%accept "accept") :int
  (filedesc :int)
  (address :pointer)
  (address-len (:pointer socklen-t)))

(cffi:defcfun (%recv "recv") ssize-t
  (socket :int)
  (buffer :pointer)
  (buffer-len size-t)
  (flags :int))

(cffi:defcfun (%send "send") ssize-t
  (socket :int)
  (buffer :pointer)
  (buffer-len size-t)
  (flags :int))

(cffi:defcfun (%set-sock-opt "setsockopt") :int
  (socket :int)
  (level sockopt-level)
  (option-name sockopt-option)
  (option-value :pointer)
  (option-len socklen-t))

(cffi:defcfun (%shutdown "shutdown") :int
  (socket :int)
  (how shutdown-how))

