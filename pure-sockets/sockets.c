#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/socket.h>
#ifndef _WIN32
#include <sys/un.h>
#endif
#include <netdb.h>

#include <pure/runtime.h>

#if !defined(AF_LOCAL) && defined(PF_LOCAL)
#define AF_LOCAL PF_LOCAL
#endif
#if !defined(AF_INET) && defined(PF_INET)
#define AF_INET PF_INET
#endif
#if !defined(AF_INET6) && defined(PF_INET6)
#define AF_INET6 PF_INET6
#endif
#ifndef AF_UNSPEC
#define AF_UNSPEC 0
#endif
#ifndef IPPROTO_IP
#define IPPROTO_IP 0
#endif

#define sockaddr_error pure_sym("::sockaddr_error")

static void sockaddr_err(const char *msg)
{
  if (!msg) msg = "Unknown error";
  pure_expr *err = pure_app(pure_symbol(sockaddr_error),
			    pure_cstring_dup(msg));
  pure_throw(err);
}

struct sockaddr *new_sockaddr()
{
  struct sockaddr *addr = malloc(sizeof(struct sockaddr_storage));
  if (addr) memset(addr, 0, sizeof(struct sockaddr_storage));
  return addr;
}

struct sockaddr *local_sockaddr(const char *path)
{
#ifdef AF_UNIX
  struct sockaddr_un *addr = malloc(sizeof(struct sockaddr_un));
  if (addr) {
    memset(addr, 0, sizeof(struct sockaddr_un));
    addr->sun_family = AF_UNIX;
    strncpy(addr->sun_path, path, sizeof(addr->sun_path)-1);
  }
  return (struct sockaddr*)addr;
#else
  return 0;
#endif
}

struct sockaddr *make_sockaddr(int family, const char *host, const char *port)
{
  struct addrinfo hints, *res;
  struct sockaddr *addr = 0;
  const size_t len = 0;
  int rc;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = family;
  // host "*" means INADDR_ANY
  if (strcmp(host, "*") == 0) {
    hints.ai_flags |= AI_PASSIVE;
    host = 0;
  }
  rc = getaddrinfo(host, port, &hints, &res);
  if (rc) {
    sockaddr_err(gai_strerror(rc));
    return 0;
  }
  if (!res || !res->ai_addr) goto err;
  addr = malloc(res->ai_addrlen);
  if (!addr) goto err;
  memcpy(addr, res->ai_addr, res->ai_addrlen);
  freeaddrinfo(res);
  return addr;
 err:
  if (res) freeaddrinfo(res);
  return 0;
}

// Like above, but returns *all* matching addresses.
struct sockaddr **make_sockaddrs(int family, const char *host, const char *port)
{
  struct addrinfo hints, *res, *res0;
  struct sockaddr *addr = 0, **addrs;
  size_t len = 0, count;
  int rc;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = family;
  // host "*" means INADDR_ANY
  if (strcmp(host, "*") == 0) {
    hints.ai_flags |= AI_PASSIVE;
    host = 0;
  }
  rc = getaddrinfo(host, port, &hints, &res);
  if (rc) {
    sockaddr_err(gai_strerror(rc));
    return 0;
  }
  for (count = 0, res0 = res; res && res->ai_addr; res = res->ai_next)
    count++;
  addrs = malloc((count+1)*sizeof(struct sockaddr*));
  if (!addrs) goto err;
  if (count == 0) {
    if (res0) freeaddrinfo(res0);
    addrs[0] = 0;
    return 0;
  }
  for (count = 0, res = res0; res && res->ai_addr; res = res->ai_next) {
    addr = malloc(res->ai_addrlen);
    if (!addr) {
      while (count > 0) free(addrs[--count]);
      free(addrs);
      goto err;
    }
    memcpy(addr, res->ai_addr, res->ai_addrlen);
    addrs[count++] = addr;
  }
  addrs[count++] = 0;
  freeaddrinfo(res);
  return addrs;
 err:
  if (res0) freeaddrinfo(res0);
  return 0;
}

int sockaddr_family(struct sockaddr *addr)
{
  if (!addr)
    return 0;
  else
    return addr->sa_family;
}

const char *sockaddr_path(struct sockaddr *addr)
{
  if (!addr)
    return 0;
#ifdef AF_UNIX
  else if (addr->sa_family == AF_UNIX)
    return ((struct sockaddr_un*)addr)->sun_path;
#endif
  else
    return 0;
}

int sockaddr_port(struct sockaddr *addr)
{
  if (!addr)
    return -1;
  else if (addr->sa_family == AF_INET)
    return ntohs(((struct sockaddr_in*)addr)->sin_port);
  else if (addr->sa_family == AF_INET6)
    return ntohs(((struct sockaddr_in6*)addr)->sin6_port);
  else
    return -1;
}

static char buf[1025]; // TLD

const char *sockaddr_hostname(struct sockaddr *addr)
{
  if (!addr)
    return 0;
  else if (addr->sa_family == AF_INET || addr->sa_family == AF_INET6) {
    size_t len = (addr->sa_family == AF_INET)?
      sizeof(struct sockaddr_in):sizeof(struct sockaddr_in6);
    int rc = getnameinfo(addr, len, buf, sizeof(buf), 0, 0, 0);
    if (rc) {
      sockaddr_err(gai_strerror(rc));
      return 0;
    }
    return buf;
  } else
    return 0;
}

const char *sockaddr_ip(struct sockaddr *addr)
{
  if (!addr)
    return 0;
  else if (addr->sa_family == AF_INET || addr->sa_family == AF_INET6) {
    size_t len = (addr->sa_family == AF_INET)?
      sizeof(struct sockaddr_in):sizeof(struct sockaddr_in6);
    int rc = getnameinfo(addr, len, buf, sizeof(buf), 0, 0, NI_NUMERICHOST);
    if (rc) {
      sockaddr_err(gai_strerror(rc));
      return 0;
    }
    return buf;
  } else
    return 0;
}

const char *sockaddr_service(struct sockaddr *addr)
{
  if (!addr)
    return 0;
  else if (addr->sa_family == AF_INET || addr->sa_family == AF_INET6) {
    size_t len = (addr->sa_family == AF_INET)?
      sizeof(struct sockaddr_in):sizeof(struct sockaddr_in6);
    int rc = getnameinfo(addr, len, 0, 0, buf, sizeof(buf), 0);
    if (rc) {
      sockaddr_err(gai_strerror(rc));
      return 0;
    }
    return buf;
  } else
    return 0;
}

socklen_t sockaddr_len(struct sockaddr *addr)
{
  if (!addr)
    return 0;
  else if (addr->sa_family == 0)
    return sizeof(struct sockaddr_storage);
#ifdef AF_UNIX
  else if (addr->sa_family == AF_UNIX)
    return sizeof(struct sockaddr_un);
#endif
  else if (addr->sa_family == AF_INET)
    return sizeof(struct sockaddr_in);
  else if (addr->sa_family == AF_INET6)
    return sizeof(struct sockaddr_in6);
  else
    return 0;
}

#ifndef _WIN32
int closesocket(int fd)
{
  return close(fd);
}
#endif

#define define(sym) pure_def(pure_sym(#sym), pure_int(sym))

void __socket_defs(void)
{
  // Address families.
  define(AF_UNSPEC);
#ifdef AF_LOCAL
  define(AF_LOCAL);
#endif
#ifdef AF_UNIX
  define(AF_UNIX);
#endif
#ifdef AF_FILE
  define(AF_FILE);
#endif
  define(AF_INET);
  define(AF_INET6);
  // Socket types.
  define(SOCK_STREAM);
  define(SOCK_DGRAM);
#ifdef SOCK_SEQPACKET
  define(SOCK_SEQPACKET);
#endif
#ifdef SOCK_RAW
  define(SOCK_RAW);
#endif
#ifdef SOCK_RDM
  define(SOCK_RDM);
#endif
#ifdef SOCK_NONBLOCK
  // Useful on Linux to enable non-blocking operation.
  define(SOCK_NONBLOCK);
#endif
#ifdef SOCK_CLOEXEC
  // Useful on Linux to set the FD_CLOEXEC flag on the socket descriptor.
  define(SOCK_CLOEXEC);
#endif
  // shutdown flags.
  define(SHUT_RD);
  define(SHUT_WR);
  define(SHUT_RDWR);
  // send/recv flags.
  define(MSG_EOR);
  define(MSG_OOB);
  define(MSG_PEEK);
  define(MSG_WAITALL);
#ifdef MSG_DONTWAIT
  // Useful on Linux to enable non-blocking operation.
  define(MSG_DONTWAIT);
#endif
  // getsockopt/setsockopt flags.
  define(SOL_SOCKET);
  define(SO_ACCEPTCONN);
  define(SO_BROADCAST);
  define(SO_DEBUG);
  define(SO_DONTROUTE);
  define(SO_ERROR);
  define(SO_KEEPALIVE);
  define(SO_LINGER);
  define(SO_OOBINLINE);
  define(SO_RCVBUF);
#ifdef SO_RCVLOWAT
  define(SO_RCVLOWAT);
#endif
#ifdef SO_RCVTIMEO
  define(SO_RCVTIMEO);
#endif
  define(SO_REUSEADDR);
  define(SO_SNDBUF);
#ifdef SO_SNDLOWAT
  define(SO_SNDLOWAT);
#endif
#ifdef SO_SNDTIMEO
  define(SO_SNDTIMEO);
#endif
  define(SO_TYPE);
  // Important IP protocol numbers.
  define(IPPROTO_IP);
  define(IPPROTO_ICMP);
  define(IPPROTO_TCP);
  define(IPPROTO_UDP);
}
