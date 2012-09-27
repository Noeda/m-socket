m-socket
========

Description
-----------

This Common Lisp package implements some features of TCP-based socket
system. It is based on CFFI (as opposed to using host Lisp system's
features like many other socket libraries do).  Currently you can't make
connections with it but you can listen for new connections.

Features an I/O multiplexer feature that lets you efficiently check the
status of many sockets at once. The Linux backend is based on epoll()
system call.

The user of the library is able to assume nothing ever blocks. None of the
calls (with a small exception, see below for known problems) block. If
something does block, it is a bug.

There is no global state internally inside the library, so it should be
possible to use any of the functions from any threads, as long as they
don't touch each other's sockets or I/O multiplexers at the same time.

Licensed under the ISC license. See LICENSE for copyright information.

### Example usage

There is some example code in example.lisp file. Load that file to run it.

Installation
------------

This package meant to be used through ASDF.  Put the code somewhere where
ASDF can find it. You also need to install m-util as this package depends
on it.

    (asdf:operate 'asdf:load-op "m-socket")

Known problems
--------------

  - Only works on Linux. Porting to FreeBSD shouldn't be hard; an I/O
  multiplexer based on kqueue() would have to be written. Porting to
  Windows is probably more involved for various reasons such as that
  platform using a different way to report socket errors.

  - There is an efficiency problem related to the use of system calls
  directly. The code has to convert arrays outside Lisp heap (that is, CFFI
  arrays to native Lisp arrays). This happens at every send and receive
  call. In my initial tests, it is not noticeable unless there is very
  large amounts of traffic. It might be useful to do some profiling here.

  - Conversion of hostnames to IP addresses blocks, unlike any other call
  in the library. This can happen in MAKE-LISTENER-SOCKET. Later, if
  connecting capabilities are added to the library, this problem has to
  be addressed if there's a desire for a non-blocking but convenient
  CONNECT call.

  - Tested on SBCL, Clozure CL and CLISP. More testing would be useful.

Author
------
Mikko Juola <mikjuo@gmail.com>

