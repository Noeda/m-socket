;;;; linux-grovel-file.lisp
;;
;; This file has grovelling stuff that is Linux specific.
;; (mostly epoll())
;;

(in-package #:m-socket)

(include "sys/epoll.h")

(constantenum (epoll-op :base-type :int)
  ((:epoll-ctl-add "EPOLL_CTL_ADD"))
  ((:epoll-ctl-mod "EPOLL_CTL_MOD"))
  ((:epoll-ctl-del "EPOLL_CTL_DEL")))

(cunion epoll-data "epoll_data_t"
  (ptr "ptr" :type :pointer)
  (fd "fd" :type :int)
  (u32 "u32" :type :uint32)
  (u64 "u64" :type :uint64))

(bitfield epoll-events
  ((:epollin "EPOLLIN"))
  ((:epollout "EPOLLOUT"))
  ((:epollrdhup "EPOLLRDHUP"))
  ((:epollpri "EPOLLPRI"))
  ((:epollerr "EPOLLERR"))
  ((:epollhup "EPOLLHUP"))
  ((:epollet "EPOLLET"))
  ((:epolloneshot "EPOLLONESHOT")))

(cstruct epoll-event "struct epoll_event"
  (events "events" :type epoll-events)
  (data "data" :type epoll-data))

