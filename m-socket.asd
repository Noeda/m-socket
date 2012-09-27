;;;; m-socket.asd

(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem #:m-socket
  :serial t
  :description "Socket listener and I/O multiplexer library based on CFFI."
  :author "Mikko Juola <mikjuo@gmail.com>"
  :license "ISC License"
  :depends-on ("m-util" "cffi" "trivial-garbage" "trivial-gray-streams")
  :components ((:file "package")
               #+linux(cffi-grovel:grovel-file "unix-grovel-file")
               #+freebsd(cffi-grovel:grovel-file "unix-grovel-file")
               (:file "socket-cffi-bindings")
               (:file "errno")
               (:file "m-socket")
               #+linux(cffi-grovel:grovel-file "linux-grovel-file")
               #+linux(:file "io-multiplex-epoll")
               #+freebsd(:file "io-multiplex-kqueue")
               (:file "io-multiplex-doc")
               (:file "gray-streamed")))


