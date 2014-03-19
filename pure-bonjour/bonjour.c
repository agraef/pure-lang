#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <assert.h>
#include <pthread.h>

#include <pure/runtime.h>

#include <dns_sd.h>
#include <arpa/inet.h>

#ifndef DEBUG
// Set this to a nonzero value to enable debugging output.
#define DEBUG 0
#endif

// Generic main loop which processes events and dispatches callbacks.

typedef struct {
  DNSServiceRef service_ref;
  bool done, gc;
} bonjour_data_t;

static void *main_loop(void *data)
{
  bonjour_data_t *d = (bonjour_data_t*)data;
  int dns_sd_fd = DNSServiceRefSockFD(d->service_ref), nfds = dns_sd_fd + 1;
  fd_set readfds;
  struct timeval tv;
  while (!d->done) {
    int ret;
    FD_ZERO(&readfds); FD_SET(dns_sd_fd, &readfds);
    tv.tv_sec = 1;
    tv.tv_usec = 0;
    if ((ret = select(nfds, &readfds, (fd_set*)NULL, (fd_set*)NULL, &tv)) > 0) {
      DNSServiceErrorType ret;
      if (FD_ISSET(dns_sd_fd, &readfds) &&
	  (ret = DNSServiceProcessResult(d->service_ref)) != kDNSServiceErr_NoError) {
	d->done = true;
#if DEBUG
	fprintf(stderr, "(main_loop) %p: DNSServiceProcessResult() error, return code: %d\n",
		d, ret);
#endif
      }
    } else if (ret < 0 && errno != EINTR) {
      d->done = true;
#if DEBUG
      fprintf(stderr, "(main_loop) %p: select() error, errno: %d (%s)\n",
	      d, errno, strerror(errno));
#endif
    }
  }
#if DEBUG
  fprintf(stderr, "(main_loop) %p: exiting\n", d);
#endif
  DNSServiceRefDeallocate(d->service_ref);
  if (d->gc) free(d);
  return NULL;
}

/* Service publishing. *****************************************************/

typedef struct {
  DNSServiceRef service_ref;
  bool done, gc;
  char *name, *type;
  uint16_t port;
  int ret;
} bonjour_service_t;

static void register_callback(DNSServiceRef service,
			      DNSServiceFlags flags,
			      DNSServiceErrorType ret,
			      const char *name,
			      const char *type,
			      const char *domain,
			      void *data)
{
  bonjour_service_t *t = (bonjour_service_t*)data;
  t->ret = ret;
  if (ret != kDNSServiceErr_NoError) {
#if DEBUG
    fprintf(stderr, "failed to register service '%s', return code: %d\n", t->name, ret);
#endif
  } else {
    // service was registered successfully, but there might be changes in the
    // registered information we have to pick up in our service record
    assert(name); assert(type);
    if (strcmp(t->name, name)) {
      // service name was changed (probably due to name conflict)
      free(t->name);
      t->name = strdup(name);
      assert(t->name);
    }
    if (strcmp(t->type, type)) {
      // service type was changed (cosmetic?)
      free(t->type);
      t->type = strdup(type);
      assert(t->type);
    }
#if DEBUG
    fprintf(stderr, "registered service '%s', type '%s', domain '%s'\n", name, type, domain);
#endif
  }
}

bonjour_service_t *bonjour_publish(const char *name, const char *type, int port)
{
  bonjour_service_t *t = calloc(1, sizeof(bonjour_service_t));
  assert(t);
  t->done = false;
  t->name = strdup(name);
  t->type = strdup(type);
  t->port = port;
  t->ret = 0;
  assert(t->name && t->type);
  // Create the service record.
  DNSServiceErrorType err =
    DNSServiceRegister(&t->service_ref, 0, 0, t->name, t->type, "", NULL, htons(port), 0, NULL,
		       register_callback, t);
  if (err != kDNSServiceErr_NoError) goto fail;
  return t;
 fail:
  free(t->name); free(t->type); free(t);
#if DEBUG
  fprintf(stderr, "couldn't create service, return code: %d\n", err);
#endif
  return NULL;
}

void bonjour_unpublish(bonjour_service_t *t)
{
  if (!t) return;
  DNSServiceRefDeallocate(t->service_ref);
  if (t->name) free(t->name);
  if (t->type) free(t->type);
  free(t);
}

pure_expr *bonjour_check(bonjour_service_t *t)
{
  if (!t) return 0;
  int ret = t->done?t->ret:DNSServiceProcessResult(t->service_ref);
  t->done = true;
  if (ret != kDNSServiceErr_NoError)
    return pure_int(ret);
  else if (t->ret < 0)
    return pure_int(t->ret);
  else
    return pure_tuplel(3,
		       pure_cstring_dup(t->name),
		       pure_cstring_dup(t->type),
		       pure_int(t->port));
}

/* Service discovery. ******************************************************/

typedef struct _service_t {
  char *name, *type, *domain, *addr;
  uint16_t port;
  struct _service_t *next;
} service_t;

static service_t *add_service(service_t *s, const char *name,
			      const char *type, const char *domain,
			      const char *addr, uint16_t port)
{
  service_t r = { strdup(name), strdup(type), strdup(domain), strdup(addr), port, s };
  service_t *t = (service_t *)calloc(1, sizeof(service_t));
  assert(t && r.name && r.type && r.domain && r.addr);
  *t = r;
  return t;
}

static service_t *del_service(service_t *s, const char *name,
			      const char *type, const char *domain)
{
  service_t *t, *prev = NULL;
  for (t = s; t; prev = t, t = t->next)
    if (!strcmp(name, t->name) && !strcmp(type, t->type) &&
	!strcmp(domain, t->domain))
      break;
  if (!t) return s;
  if (prev) {
    assert(t == prev->next);
    prev->next = t->next;
  } else {
    assert(t == s);
    s = t->next;
  }
  free(t->name);
  free(t->type);
  free(t->domain);
  free(t->addr);
  free(t);
  return s;
}

static void free_services(service_t *s)
{
  service_t *t;
  for (t = s; t; t = s) {
    s = t->next;
    free(t->name);
    free(t->type);
    free(t->domain);
    free(t->addr);
    free(t);
  }
}

typedef struct {
  DNSServiceRef service_ref;
  bool done, gc;
  char *type;
  int ret, avail;
  service_t *services;
  pthread_t thread;
  pthread_mutex_t mutex;
} bonjour_browser_t;

typedef struct {
  DNSServiceRef service_ref;
  bool done, gc;
  bonjour_browser_t *t;
  char *name, *type, *domain;
  uint16_t port;
  pthread_t thread;
} bonjour_resolver_t;

static void getaddr_callback(DNSServiceRef service,
			     DNSServiceFlags flags,
			     uint32_t interface,
			     DNSServiceErrorType ret,
			     const char *hostname,
			     const struct sockaddr *address,
			     uint32_t ttl,
			     void *data)
{
  // This is called when the service address has been resolved successfully or
  // timed out.
  bonjour_resolver_t *r = (bonjour_resolver_t*)data;
  bonjour_browser_t *t = r->t;
  char *name = r->name, *type = r->type, *domain = r->domain;
  uint16_t port = r->port;
  if (ret != kDNSServiceErr_NoError) {
#if DEBUG
    fprintf(stderr,
"(resolver) failed to resolve service '%s' of type '%s' in domain '%s', return code %d\n",
	    name, type, domain, ret);
#endif
  } else {
    const struct sockaddr_in *in = (const struct sockaddr_in*)address;
    char ip[100];
    uint32_t addr = in->sin_addr.s_addr;
    unsigned i1 = addr&0xff, i2 = addr>>8&0xff, i3 = addr>>16&0xff, i4 = addr>>24&0xff;
    sprintf(ip, "%u.%u.%u.%u", i1, i2, i3, i4);
    pthread_mutex_lock(&t->mutex);
    t->avail = 1;
    t->services = add_service(t->services, name, type, domain, ip, port);
    pthread_mutex_unlock(&t->mutex);
#if DEBUG
    fprintf(stderr,
	    "(resolver) service '%s' of type '%s' in domain '%s': %s:%u (%s)\n",
	    name, type, domain, ip, port, hostname);
#endif
  }
  free(name); free(type); free(domain);
  r->done = r->gc = true;
}

static void resolve_callback(DNSServiceRef service,
			     DNSServiceFlags flags,
			     uint32_t interface,
			     DNSServiceErrorType ret,
			     const char *fullname,
			     const char *hosttarget,
			     uint16_t port,
			     uint16_t txtLen,
			     const unsigned char *txtRecord,
			     void *data)
{
  // This is called whenever a service has been resolved successfully or timed
  // out.
  bonjour_resolver_t *r = (bonjour_resolver_t*)data;
  bonjour_browser_t *t = r->t;
  char *name = r->name, *type = r->type, *domain = r->domain;
  if (ret != kDNSServiceErr_NoError) {
#if DEBUG
    fprintf(stderr,
"(resolver) failed to resolve service '%s' of type '%s' in domain '%s', return code %d\n",
	    name, type, domain, ret);
#endif
  } else {
    // Kick off yet another call to get the actual IP address. Oh dear.
    bonjour_resolver_t *r = calloc(1, sizeof(bonjour_resolver_t));
    int ret2 = 0;
    assert(r);
    r->t = t; r->done = false; r->port = ntohs(port);
    r->name = strdup(name); r->type = strdup(type); r->domain = strdup(domain);
    ret = DNSServiceGetAddrInfo(&r->service_ref, 0, interface, kDNSServiceProtocol_IPv4,
				hosttarget, getaddr_callback, r);
    if (ret == kDNSServiceErr_NoError) {
      ret2 = pthread_create(&r->thread, NULL, main_loop, r);
      if (ret2) {
#if DEBUG
	fprintf(stderr, "(resolver) failed to resolve service '%s', pthread_create: %d\n",
	      name, ret2);
#endif
	DNSServiceRefDeallocate(r->service_ref);
	goto fail;
      }
    } else {
#if DEBUG
      fprintf(stderr, "(resolver) failed to resolve service '%s', return code: %d\n",
	      name, ret);
#endif
    fail:
      free(r->name); free(r->type); free(r->domain); free(r);
    }
  }
  free(name); free(type); free(domain);
  r->done = r->gc = true;
}

static void browse_callback(DNSServiceRef service,
			    DNSServiceFlags flags,
			    uint32_t interface,
			    DNSServiceErrorType ret,
			    const char* name,
			    const char* type,
			    const char* domain,
			    void *data)
{
  // This is called whenever new services become available or are removed.
  bonjour_browser_t *t = (bonjour_browser_t*)data;
  pthread_mutex_lock(&t->mutex);
  t->ret = ret;
  if (ret != kDNSServiceErr_NoError) {
#if DEBUG
    fprintf(stderr, "(browser) error code %d\n", ret);
#endif
    // XXXFIXME: do we really want to exit the browser loop here?
    t->done = true;
  } else if (flags & kDNSServiceFlagsAdd) {
#if DEBUG
    fprintf(stderr, "(browser) ADD service '%s' of type '%s' in domain '%s'\n",
	   name, type, domain);
#endif
    // resolve this service
    bonjour_resolver_t *r = calloc(1, sizeof(bonjour_resolver_t));
    int ret2 = 0;
    assert(r);
    r->t = t; r->done = false;
    r->name = strdup(name); r->type = strdup(type); r->domain = strdup(domain);
    ret = DNSServiceResolve(&r->service_ref, 0, interface, name, type, domain,
			    resolve_callback, r);
    if (ret == kDNSServiceErr_NoError) {
      ret2 = pthread_create(&r->thread, NULL, main_loop, r);
      if (ret2) {
#if DEBUG
	fprintf(stderr, "(resolver) failed to resolve service '%s', pthread_create: %d\n",
	      name, ret2);
#endif
	DNSServiceRefDeallocate(r->service_ref);
	goto fail;
      }
    } else {
#if DEBUG
      fprintf(stderr, "(resolver) failed to resolve service '%s', return code: %d\n",
	      name, ret);
#endif
    fail:
      free(r->name); free(r->type); free(r->domain); free(r);
    }
  } else {
    t->avail = 1;
    t->services = del_service(t->services, name, type, domain);
#if DEBUG
    fprintf(stderr, "(browser) DEL service '%s' of type '%s' in domain '%s'\n",
	   name, type, domain);
#endif
  }
  pthread_mutex_unlock(&t->mutex);
}

bonjour_browser_t *bonjour_browse(const char *type)
{
  bonjour_browser_t *t = calloc(1, sizeof(bonjour_browser_t));
  assert(t);
  t->type = strdup(type);
  t->done = false;
  t->ret = t->avail = 0;
  t->services = NULL;
  assert(t->type);
  // Create the service browser.
  DNSServiceErrorType err =
    DNSServiceBrowse(&t->service_ref, 0, 0, t->type, "", browse_callback, t);
  if (err != kDNSServiceErr_NoError) goto fail;
  pthread_mutex_init(&t->mutex, NULL);
  if (pthread_create(&t->thread, NULL, main_loop, t)) goto fail2;
  return t;
 fail2:
  pthread_mutex_destroy(&t->mutex);
  DNSServiceRefDeallocate(t->service_ref);
 fail:
  free(t->type); free(t);
#if DEBUG
  fprintf(stderr, "couldn't create service browser, return code: %d\n", err);
#endif
  return NULL;
}

void bonjour_close(bonjour_browser_t *t)
{
  if (!t) return;
  t->done = true;
  pthread_join(t->thread, NULL);
  if (t->type) free(t->type);
  free_services(t->services);
  pthread_mutex_destroy(&t->mutex);
  free(t);
}

int bonjour_avail(bonjour_browser_t *t)
{
  int ret;
  if (!t) return 0;
  pthread_mutex_lock(&t->mutex);
  if (t->ret < 0)
    ret = t->ret;
  else
    ret = t->avail;
  pthread_mutex_unlock(&t->mutex);
  return ret;
}

pure_expr *bonjour_get(bonjour_browser_t *t)
{
  pure_expr *ret;
  if (!t) return 0;
  pthread_mutex_lock(&t->mutex);
  if (t->ret < 0)
    ret = pure_int(t->ret);
  else {
    pure_expr *cons = pure_symbol(pure_sym(":"));
    service_t *s;
    ret = pure_listl(0);
    for (s = t->services; s; s= s->next)
      ret = pure_appl(cons, 2,
		      pure_tuplel(5,
				  pure_cstring_dup(s->name),
				  pure_cstring_dup(s->type),
				  pure_cstring_dup(s->domain),
				  pure_cstring_dup(s->addr),
				  pure_int(s->port)),
		      ret);
    pure_freenew(cons);
    t->avail = 0;
  }
  pthread_mutex_unlock(&t->mutex);
  return ret;
}
