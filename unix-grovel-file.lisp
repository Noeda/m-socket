;; unix-grovel-file.lisp
; This is a groveller file. See CFFI documentation on grovelling

(in-package #:m-socket)

(include "stddef.h"
         "unistd.h"
         "fcntl.h"
         "errno.h"
         "netinet/in.h"
         "sys/socket.h"
         "netdb.h")

(ctype size-t "size_t")
(ctype ssize-t "ssize_t")
(ctype socklen-t "socklen_t")

(constantenum (address-family :base-type :int)
  ((:af-inet "AF_INET" "PF_INET"))
  ((:af-inet6 "AF_INET6" "PF_INET6")))

(cstruct in-addr "struct in_addr")

(cstruct sockaddr-in "struct sockaddr_in"
  (sin-family "sin_family" :type :int16)
  (sin-port "sin_port" :type :uint16)
  (sin-addr "sin_addr" :type :uint32))

(cstruct sockaddr-in6 "struct sockaddr_in6"
  (sin6-family "sin6_family" :type :int16)
  (sin6-port "sin6_port" :type :uint16)
  (sin6-addr "sin6_addr" :type :uint8 :count 16))

(constantenum (socktype :base-type :int)
  ((:any "NULL"))
  ((:sock-stream "SOCK_STREAM"))
  ((:sock-dgram "SOCK_DGRAM")))

(cstruct addrinfo "struct addrinfo"
  (ai-family "ai_family" :type address-family)
  (ai-flags "ai_flags" :type :int)
  (ai-socktype "ai_socktype" :type socktype)
  (ai-protocol "ai_protocol" :type :int)
  (ai-addrlen "ai_addrlen" :type socklen-t)
  (ai-addr "ai_addr" :type :pointer)
  (ai-canonname "ai_canonname" :type :string)
  (ai-next "ai_next" :type :pointer))


(constantenum (errno-code :base-type :int)
  ((:success "NULL"))
  ((:ebadf "EBADF"))
  ((:eintr "EINTR"))
  ((:einprogress "EINPROGRESS"))
  ((:einval "EINVAL"))
  ((:eacces "EACCES"))
  ((:eafnosupport "EAFNOSUPPORT"))
  ((:enobufs "ENOBUFS"))
  ((:enomem "ENOMEM"))
  ((:enfile "ENFILE"))
  ((:epipe "EPIPE"))
  ((:eexist "EEXIST"))
  #+linux((:emfile "EMFILE")) ; epoll
  ((:eperm "EPERM"))
  ((:eagain "EAGAIN"))
  ((:eaddrinuse "EADDRINUSE"))
  ((:eaddrnotavail "EADDRNOTAVAIL"))
  ((:enetdown "ENETDOWN"))
  ((:enetunreach "ENETUNREACH"))
  ((:ewouldblock "EWOULDBLOCK")))

(constantenum (gai-code :base-type :int)
  ((:success "NULL"))
  ((:eai-again "EAI_AGAIN"))
  ((:eai-badflags "EAI_BADFLAGS"))
  ((:eai-fail "EAI_FAIL"))
  ((:eai-family "EAI_FAMILY"))
  ((:eai-memory "EAI_MEMORY"))
  ((:eai-noname "EAI_NONAME"))
  ((:eai-system "EAI_SYSTEM"))
  ((:eai-overflow "EAI_OVERFLOW"))
  ((:eai-protocol "EAI_PROTOCOL") :optional t)
  ((:eai-socktype "EAI_SOCKTYPE"))
  ((:eai-badhints "EAI_BADHINTS") :optional t)
  ((:eai-service "EAI_SERVICE") :optional t)
  ((:eai-intr "EAI_INTR") :optional t))

(constantenum (fcntl-cmd :base-type :int)
  ((:f-getfl "F_GETFL"))
  ((:f-setfl "F_SETFL")))

(constantenum (sockopt-level :base-type :int)
  ((:sol-socket "SOL_SOCKET")))

(constantenum (sockopt-option :base-type :int)
  ((:so-debug "SO_DEBUG"))
  ((:so-broadcast "SO_BROADCAST"))
  ((:so-reuseaddr "SO_REUSEADDR"))
  ((:so-keepalive "SO_KEEPALIVE")))

(constantenum (shutdown-how :base-type :int)
  ((:shut-rd "SHUT_RD"))
  ((:shut-wr "SHUT_WR"))
  ((:shut-rdwr "SHUT_RDWR")))

(cvar ("errno" errno) errno-code)

(constant (+o-nonblock+ "O_NONBLOCK"))

