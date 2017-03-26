<a name="doc-pure-sockets"></a>

pure-sockets: Pure Sockets Interface
====================================

<a name="module-sockets"></a>

Version 0.7, March 06, 2017

Albert Gräf &lt;<aggraef@gmail.com>&gt;

This is an interface to the Berkeley socket functions. It provides most of the
core functionality, so you can create sockets for both stream and datagram
based protocols and use these to transmit messages. Unix-style file sockets
are also available if the host system supports them.

Installation
------------

Get the latest source from
<https://bitbucket.org/purelang/pure-lang/downloads/pure-sockets-0.7.tar.gz>.

Run `make` to compile the module and `sudo make install` to install it in the
Pure library directory. To uninstall the module, use `sudo make uninstall`.
There are a number of other targets (mostly for maintainers), please see the
Makefile for details.

`make` tries to guess your Pure installation directory and platform-specific
setup. If it gets this wrong, you can set some variables manually. In
particular, `make install prefix=/usr` sets the installation prefix, and
`make PIC=-fPIC` or some similar flag might be needed for compilation on 64
bit systems. You can also set custom compilation options with the CFLAGS
variable, e.g.: `make CFLAGS=-O3`. Again, please see the Makefile for details.

Usage
-----

To use the operations of this module, put the following in your Pure script:

    using sockets;

With the [sockets](#module-sockets) module loaded, all the standard socket
functions are available and work pretty much like in C. The only real
difference is that, for convenience, functions taking socket addresses as
parameters (`struct_sockaddr*` pointers in Pure), are called without the
`addrlen` parameter; the size of the socket address structure will be inferred
automatically and passed to the underlying C functions. Also, there are some
convenience functions which act as wrappers around `getaddrinfo` and
`getnameinfo` to create socket addresses from symbolic information (hostname
or ip, port names or numbers) and return information about existing address
pointers, see [Creating and Inspecting Socket
Addresses](#creating-and-inspecting-socket-addresses) below.

Below is a list of the provided functions. Please see the corresponding manual
pages for details, and check the Pure scripts in the examples subdirectory for
some examples.

### Creating and Inspecting Socket Addresses

These functions are Pure-specific. The created socket addresses are malloc'ed
and free themselves automatically when garbage-collected.

<a name="sockaddr"></a>`sockaddr ()`
:   Create a pointer to an empty socket address suitable to hold the socket
    address result of routines like [`accept`](#accept),
    [`getsockname`](#getsockname), [`recvfrom`](#recvfrom), etc. which return
    a socket address.

<a name="sockaddr"></a>`sockaddr ([int family,] char *path)`
:   Create a local (a.k.a. file) socket address for the given pathname. The
    `family` parameter, if specified, must be [`AF_UNIX`](#AF_UNIX) here.
    Please note that [`AF_UNIX`](#AF_UNIX) is not supported on all platforms.
    You can check for this by testing the [`HAVE_AF_UNIX`](#HAVE_AF_UNIX)
    constant, which is a truth value specifying whether [`AF_UNIX`](#AF_UNIX)
    is available on your system.

<a name="sockaddr"></a>`sockaddr ([int family,] char *host, char *port)`, <a name="sockaddr"></a>`sockaddr ([int family,] char *host, int port)`
:   This uses `getaddrinfo` to retrieve an [`AF_INET`](#AF_INET) or
    [`AF_INET6`](#AF_INET6) address for the given hostname (or numeric IP
    address in string form) and port (specified either as an int or a string).
    If `family` is omitted, it defaults to [`AF_UNSPEC`](#AF_UNSPEC) which
    matches both [`AF_INET`](#AF_INET) and [`AF_INET6`](#AF_INET6) addresses.

<a name="sockaddrs"></a>`sockaddrs ([int family,] char *host, char *port)`, <a name="sockaddrs"></a>`sockaddrs ([int family,] char *host, int port)`
:   This works like [`sockaddr`](#sockaddr) above, but returns a list with
    *all* matching addresses.

<a name="sockaddr_family"></a>`sockaddr_family addr`
:   Returns the address family of the given address.

<a name="sockaddr_path"></a>`sockaddr_path addr`
:   Returns the pathname for [`AF_UNIX`](#AF_UNIX) addresses.

<a name="sockaddr_hostname"></a>`sockaddr_hostname addr`
:   Returns the hostname if available, the IP address otherwise.

<a name="sockaddr_ip"></a>`sockaddr_ip addr`
:   Returns the IP address.

<a name="sockaddr_service"></a>`sockaddr_service addr`
:   Returns the service (a.k.a. port) name.

<a name="sockaddr_port"></a>`sockaddr_port addr`
:   Returns the port number.

<a name="sockaddr_info"></a>`sockaddr_info addr`
:   Returns a readable description of a socket address, as a
    `(family,hostname,port)` tuple. You should be able to pass this into
    [`sockaddr`](#sockaddr) again to get the original address.

<!-- -->
### Creating and Closing Sockets

<a name="socket"></a>`socket domain type protocol`
:   Creates a socket for the given protocol family ([`AF_UNIX`](#AF_UNIX),
    [`AF_INET`](#AF_INET) or [`AF_INET6`](#AF_INET6)), socket type
    ([`SOCK_STREAM`](#SOCK_STREAM), [`SOCK_DGRAM`](#SOCK_DGRAM), etc.) and
    protocol. Note that on Linux we also support the
    [`SOCK_NONBLOCK`](#SOCK_NONBLOCK) (non-blocking) and
    [`SOCK_CLOEXEC`](#SOCK_CLOEXEC) (close-on-exec) flags which can be or'ed
    with the socket type to get sockets with the corresponding features. The
    protocol number is usually 0, denoting the default protocol, but it can
    also be any of the prescribed [`IPPROTO`](#IPPROTO) constants (a few
    common ones are predefined by this module, try `show -g IPPROTO_*` for a
    list of those).

<a name="socketpair"></a>`socketpair domain type protocol sv`
:   Create a pair of sockets. The descriptors are returned in the integer
    vector `sv` passed in the last argument.

<a name="shutdown"></a>`shutdown fd how`
:   Perform shutdown on a socket. The second argument should be one of
    [`SHUT_RD`](#SHUT_RD), [`SHUT_WR`](#SHUT_WR) and
    [`SHUT_RDWR`](#SHUT_RDWR).

<a name="closesocket"></a>`closesocket fd`
:   This is provided for Windows compatibility. On POSIX systems this is just
    `close`.

<!-- -->
### Establishing Connections

<a name="accept"></a>`accept sockfd addr`, <a name="bind"></a>`bind sockfd addr`, <a name="connect"></a>`connect sockfd addr`, <a name="listen"></a>`listen sockfd backlog`

:   <!-- -->

### Socket I/O

<a name="recv"></a>`recv fd buf len flags`, <a name="send"></a>`send fd buf len flags`, <a name="recvfrom"></a>`recvfrom fd buf len flags addr`, <a name="sendto"></a>`sendto fd buf len flags addr`

:   <!-- -->

The usual [`send`](#send)/[`recv`](#recv) flags specified by POSIX
([`MSG_EOR`](#MSG_EOR), [`MSG_OOB`](#MSG_OOB), [`MSG_PEEK`](#MSG_PEEK),
[`MSG_WAITALL`](#MSG_WAITALL)) are provided. On Linux we also support
[`MSG_DONTWAIT`](#MSG_DONTWAIT). Note that on POSIX systems you can also just
[`fdopen`](#fdopen) the socket descriptor and use the standard file I/O
operations from the [system](#module-system) module instead.

### Socket Information

<a name="getsockname"></a>`getsockname fd addr`, <a name="getpeername"></a>`getpeername fd addr`, <a name="getsockopt"></a>`getsockopt fd level name val len`, <a name="setsockopt"></a>`setsockopt fd level name val len`

:   <!-- -->

For [`getsockopt`](#getsockopt) and [`setsockopt`](#setsockopt), currently
only the [`SOL_SOCKET`](#SOL_SOCKET) level is defined (`level` argument) along
with the available POSIX socket options (`name` argument). Try `show -g SO_*`
to get a list of those. Also note that for most socket level options the `val`
argument is actually an `int*`, so you can pass a Pure int vector (with
`len = SIZEOF_INT`) for that parameter.

Example
-------

Here is a fairly minimal example using Unix stream sockets. To keep things
simple, this does no error checking whatsoever and just keeps sending strings
back and forth. More elaborate examples can be found in the examples directory
in the sources.

    using sockets, system;

    const path = "server_socket";
    extern int unlink(char *name);

    server = loop with
      loop = loop if ~null s && ~response fp s when
        // Connect to a client.
        cfd = accept fd $ sockaddr ();
        // Open the client socket as a FILE* and read a request.
        fp = fdopen cfd "r+"; s = fgets fp;
      end;
      loop = puts "server is exiting" $$ closesocket fd $$
             unlink path $$ () otherwise;
      response fp s::string = s=="quit\n" when
        // Process the request. (Here we just print the received
        // message and echo it back to the client.)
        printf "server> %s" s;
        fputs s fp;
      end;
    end when
      // Create the server socket and start listening.
      unlink path;
      fd = socket AF_UNIX SOCK_STREAM 0;
      bind fd (sockaddr path); listen fd 5;
      printf "server listening at '%s'\n" path;
    end;

    client = loop with
      // Keep reading requests from stdin.
      loop = loop if ~null s && ~request s when
        fputs "client> " stdout; s = fgets stdin;
      end;
      loop = puts "client is exiting" $$ () otherwise;
      request s::string = s=="quit\n" when
        fd = socket AF_UNIX SOCK_STREAM 0;
        connect fd (sockaddr path);
        // Send the request to the server.
        fp = fdopen fd "r+"; fputs s fp;
        // Get the reply.
        s = fgets fp;
      end;
    end;

To use this example, run the `server` function in one instance of the Pure
interpreter and the `client` function in another. Enter a line when the client
prompts you for input; it will be printed by the server. Behind the scenes,
the server also sends the line back to the client. After receiving the reply,
the client prompts for the next input line. Entering end-of-file at the client
prompt terminates the client but keeps the server running, so that you can
start another client and reconnect to the server. Entering just `quit` in the
client terminates both server and client. Here is how a typical interaction
may look like:

    > client;
    client> 1+1
    client> foo bar
    client> quit
    client is exiting
    ()

    > server;
    server listening at 'server_socket'
    server> 1+1
    server> foo bar
    server> quit
    server is exiting
    ()

Note that while the server processes requests sequentially, it accepts
connections from a new client after each request, so that you can run as many
clients as you like.
