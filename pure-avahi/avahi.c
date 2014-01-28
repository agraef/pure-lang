#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <pthread.h>

#include <pure/runtime.h>

#include <avahi-client/client.h>
#include <avahi-client/publish.h>
#include <avahi-client/lookup.h>

#include <avahi-common/alternative.h>
#include <avahi-common/simple-watch.h>
#include <avahi-common/malloc.h>
#include <avahi-common/error.h>
#include <avahi-common/timeval.h>

#ifndef DEBUG
// Set this to a nonzero value to enable debugging output.
#define DEBUG 0
#endif

/* Service publishing. *****************************************************/

typedef struct {
  AvahiEntryGroup *group;
  AvahiClient *client;
  AvahiSimplePoll *simple_poll;
  char *name, *type;
  uint16_t port;
  int ret;
} avahi_service_t;

static void create_services(AvahiClient *c, avahi_service_t *t);

static void entry_group_callback(AvahiEntryGroup *g,
				 AvahiEntryGroupState state,
				 void *data)
{
  avahi_service_t *t = (avahi_service_t*)data;
  assert(g == t->group || t->group == NULL);
  t->group = g;
  // This is called whenever the entry group state changes.
  switch (state) {
  case AVAHI_ENTRY_GROUP_ESTABLISHED :
    // The entry group has been established successfully.
#if DEBUG
    fprintf(stderr, "service '%s' successfully established.\n", t->name);
#endif
    t->ret = 1;
    break;
  case AVAHI_ENTRY_GROUP_COLLISION : {
    char *name;
    // A service name collision with a local service happened. Pick a new name.
    name = avahi_alternative_service_name(t->name);
    avahi_free(t->name);
    t->name = name;
    assert(name);
#if DEBUG
    fprintf(stderr, "service name collision, renaming service to '%s'\n", name);
#endif
    // And recreate the service.
    create_services(avahi_entry_group_get_client(g), t);
    break;
  }
  case AVAHI_ENTRY_GROUP_FAILURE :
    // Some kind of failure happened while we were registering our services.
    t->ret = avahi_client_errno(avahi_entry_group_get_client(g));
#if DEBUG
    fprintf(stderr, "entry group failure: %s\n", avahi_strerror(t->ret));
#endif
    avahi_simple_poll_quit(t->simple_poll);
    break;
  case AVAHI_ENTRY_GROUP_UNCOMMITED:
  case AVAHI_ENTRY_GROUP_REGISTERING:
    ;
  }
}

static void create_services(AvahiClient *c, avahi_service_t *t)
{
  char *name;
  int ret;
  assert(c);

  // If this is the first time we're called, let's create a new entry group if
  // necessary.

  if (!t->group &&
      !(t->group = avahi_entry_group_new(c, entry_group_callback, t)))
    goto fail;

  // If the group is empty (either because it was just created, or because it
  // was reset previously), add our entries.

  if (avahi_entry_group_is_empty(t->group)) {
    if ((ret = avahi_entry_group_add_service
	 (t->group, AVAHI_IF_UNSPEC, AVAHI_PROTO_UNSPEC, 0,
	  t->name, t->type, NULL, NULL, t->port, NULL)) < 0) {
      if (ret == AVAHI_ERR_COLLISION) goto collision;
      t->ret = ret;
#if DEBUG
      fprintf(stderr, "failed to add service: %s\n", avahi_strerror(ret));
#endif
      goto fail;
    }
    // Tell the server to register the service.
    if ((ret = avahi_entry_group_commit(t->group)) < 0) {
      t->ret = ret;
#if DEBUG
      fprintf(stderr, "failed to commit entry group: %s\n",
	      avahi_strerror(ret));
#endif
      goto fail;
    }
  }

  return;

 collision:

  // A service name collision with a local service happened. Pick a new name.
  name = avahi_alternative_service_name(t->name);
  avahi_free(t->name);
  t->name = name;
  assert(name);
#if DEBUG
  fprintf(stderr, "service name collision, renaming service to '%s'\n", name);
#endif
  avahi_entry_group_reset(t->group);
  create_services(c, t);
  return;

 fail:
  avahi_simple_poll_quit(t->simple_poll);
}

static void client_callback(AvahiClient *c, AvahiClientState state, void *data)
{
  avahi_service_t *t = (avahi_service_t*)data;
  // This is called whenever the client or server state changes.
  assert(c);
  switch (state) {
  case AVAHI_CLIENT_S_RUNNING:
    create_services(c, t);
    break;
  case AVAHI_CLIENT_FAILURE:
    t->ret = avahi_client_errno(c);
#if DEBUG
    fprintf(stderr, "server connection failure: %s\n", avahi_strerror(t->ret));
#endif
    avahi_simple_poll_quit(t->simple_poll);
    break;
  case AVAHI_CLIENT_S_COLLISION:
  case AVAHI_CLIENT_S_REGISTERING:
    // We need to reregister the service, e.g., because of a name collision or
    // a host name change. Simply get rid of the current record, the service
    // will then reregistered automatically when the server reenters the
    // running state.
    if (t->group) avahi_entry_group_reset(t->group);
    break;
  case AVAHI_CLIENT_CONNECTING:
    ;
  }
}

avahi_service_t *avahi_publish(const char *name, const char *type, int port)
{
  avahi_service_t *t = malloc(sizeof(avahi_service_t));
  assert(t);
  t->group = NULL;
  t->client = NULL;
  t->simple_poll = NULL;
  t->name = avahi_strdup(name);
  t->type = avahi_strdup(type);
  t->port = port;
  t->ret = 0;
  assert(t->name && t->type);
  // Create the main loop.
  if (!(t->simple_poll = avahi_simple_poll_new())) goto fail;
  // Create the client.
  t->client = avahi_client_new(avahi_simple_poll_get(t->simple_poll), 0,
			       client_callback, t, NULL);
  if (!t->client) goto fail;
  return t;
 fail:
  if (t->client) avahi_client_free(t->client);
  if (t->simple_poll) avahi_simple_poll_free(t->simple_poll);
  return NULL;
}

void avahi_unpublish(avahi_service_t *t)
{
  if (!t) return;
  if (t->group) avahi_entry_group_reset(t->group);
  // FIXME: Is this handled automatically?
  //if (t->client) avahi_client_free(t->client);
  if (t->simple_poll) avahi_simple_poll_free(t->simple_poll);
  if (t->name) avahi_free(t->name);
  if (t->type) avahi_free(t->type);
  free(t);
}

pure_expr *avahi_check(avahi_service_t *t)
{
  if (!t || !t->simple_poll) return NULL;
  while (!t->ret)
    avahi_simple_poll_iterate(t->simple_poll, -1);
  if (t->ret < 0)
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
  service_t r = { avahi_strdup(name), avahi_strdup(type),
		  avahi_strdup(domain), avahi_strdup(addr), port, s };
  service_t *t = (service_t *)malloc(sizeof(service_t));
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
  avahi_free(t->name);
  avahi_free(t->type);
  avahi_free(t->domain);
  avahi_free(t->addr);
  free(t);
  return s;
}

static void free_services(service_t *s)
{
  service_t *t;
  for (t = s; t; t = s) {
    s = t->next;
    avahi_free(t->name);
    avahi_free(t->type);
    avahi_free(t->domain);
    avahi_free(t->addr);
    free(t);
  }
}

typedef struct {
  AvahiServiceBrowser *sb;
  AvahiClient *client;
  AvahiSimplePoll *simple_poll;
  char *type;
  int ret, avail, count;
  service_t *services;
  pthread_t thread;
  pthread_mutex_t mutex;
} avahi_browser_t;

static void resolve_callback(AvahiServiceResolver *r,
			     AVAHI_GCC_UNUSED AvahiIfIndex interface,
			     AVAHI_GCC_UNUSED AvahiProtocol protocol,
			     AvahiResolverEvent event,
			     const char *name,
			     const char *type,
			     const char *domain,
			     const char *host_name,
			     const AvahiAddress *address,
			     uint16_t port,
			     AVAHI_GCC_UNUSED AvahiStringList *txt,
			     AVAHI_GCC_UNUSED AvahiLookupResultFlags flags,
			     void* data)
{
  // This is called whenever a service has been resolved successfully or timed
  // out.
  avahi_browser_t *t = (avahi_browser_t*)data;
  assert(r);
  switch (event) {
  case AVAHI_RESOLVER_FAILURE:
    t->count--;
#if DEBUG
    fprintf(stderr,
"(resolver) failed to resolve service '%s' of type '%s' in domain '%s': %s\n",
	    name, type, domain,
	    avahi_strerror(avahi_client_errno
			   (avahi_service_resolver_get_client(r))));
#endif
    break;
  case AVAHI_RESOLVER_FOUND: {
    char a[AVAHI_ADDRESS_STR_MAX];
    avahi_address_snprint(a, sizeof(a), address);
    pthread_mutex_lock(&t->mutex);
    if (--t->count==0) t->avail = 1;
    t->services = add_service(t->services, name, type, domain, a, port);
    pthread_mutex_unlock(&t->mutex);
#if DEBUG
    fprintf(stderr,
	    "(resolver) service '%s' of type '%s' in domain '%s': %s:%u\n",
	    name, type, domain, a, port);
#endif
  }
  }
  avahi_service_resolver_free(r);
}

static void browse_callback(AvahiServiceBrowser *b,
			    AvahiIfIndex interface,
			    AvahiProtocol protocol,
			    AvahiBrowserEvent event,
			    const char *name,
			    const char *type,
			    const char *domain,
			    AVAHI_GCC_UNUSED AvahiLookupResultFlags flags,
			    void* data)
{
  // This is called whenever new services become available or are removed.
  avahi_browser_t *t = (avahi_browser_t*)data;
  AvahiClient *c = t->client;
  assert(b);
  switch (event) {
  case AVAHI_BROWSER_FAILURE:
    t->ret = avahi_client_errno(avahi_service_browser_get_client(b));
#if DEBUG
    fprintf(stderr, "(browser) %s\n", avahi_strerror(t->ret));
#endif
    avahi_simple_poll_quit(t->simple_poll);
    break;
  case AVAHI_BROWSER_NEW:
    t->count++;
#if DEBUG
    fprintf(stderr, "(browser) ADD service '%s' of type '%s' in domain '%s'\n",
	   name, type, domain);
#endif
    if (!(avahi_service_resolver_new
	  (c, interface, protocol, name, type, domain,
	   AVAHI_PROTO_UNSPEC, 0, resolve_callback, t))) {
#if DEBUG
      fprintf(stderr, "(resolver) failed to resolve service '%s': %s\n", name,
	      avahi_strerror(avahi_client_errno(c)));
#endif
    }
    break;
  case AVAHI_BROWSER_REMOVE:
    pthread_mutex_lock(&t->mutex);
    t->avail = 1;
    t->services = del_service(t->services, name, type, domain);
    pthread_mutex_unlock(&t->mutex);
#if DEBUG
    fprintf(stderr, "(browser) DEL service '%s' of type '%s' in domain '%s'\n",
	   name, type, domain);
#endif
    break;
  case AVAHI_BROWSER_ALL_FOR_NOW:
  case AVAHI_BROWSER_CACHE_EXHAUSTED:
#if DEBUG
    fprintf(stderr, "(browser) %s\n",
	    event == AVAHI_BROWSER_CACHE_EXHAUSTED ? "cache exhausted" :
	    "all for now");
#endif
    break;
  }
}

static void browser_client_callback(AvahiClient *c, AvahiClientState state,
				    void *data)
{
  avahi_browser_t *t = (avahi_browser_t*)data;
  // This is called whenever the client or server state changes.
  assert(c);
  if (state == AVAHI_CLIENT_FAILURE) {
    t->ret = avahi_client_errno(c);
#if DEBUG
    fprintf(stderr, "server connection failure: %s\n", avahi_strerror(t->ret));
#endif
    avahi_simple_poll_quit(t->simple_poll);
  }
}

static void *browser_loop(void *data)
{
  avahi_browser_t *t = (avahi_browser_t*)data;
  avahi_simple_poll_loop(t->simple_poll);
  return NULL;
}

avahi_browser_t *avahi_browse(const char *type)
{
  avahi_browser_t *t = malloc(sizeof(avahi_browser_t));
  assert(t);
  t->sb = NULL;
  t->client = NULL;
  t->simple_poll = NULL;
  t->type = avahi_strdup(type);
  t->ret = t->avail = t->count = 0;
  t->services = NULL;
  assert(t->type);
  // Create the main loop.
  if (!(t->simple_poll = avahi_simple_poll_new())) goto fail;
  // Create the client.
  t->client = avahi_client_new(avahi_simple_poll_get(t->simple_poll), 0,
			       browser_client_callback, t, NULL);
  if (!t->client) goto fail;
  // Create the service browser.
  t->sb = avahi_service_browser_new
    (t->client, AVAHI_IF_UNSPEC, AVAHI_PROTO_UNSPEC, t->type,
     NULL, 0, browse_callback, t);
  if (!t->sb) goto fail;
  if (pthread_create(&t->thread, NULL, browser_loop, t)) goto fail;
  pthread_mutex_init(&t->mutex, NULL);
  return t;
 fail:
  if (t->sb) avahi_service_browser_free(t->sb);
  if (t->client) avahi_client_free(t->client);
  if (t->simple_poll) avahi_simple_poll_free(t->simple_poll);
  free(t);
  return NULL;
}

void avahi_close(avahi_browser_t *t)
{
  if (!t) return;
  avahi_simple_poll_quit(t->simple_poll);
  pthread_join(t->thread, NULL);
  if (t->sb) avahi_service_browser_free(t->sb);
  if (t->client) avahi_client_free(t->client);
  if (t->simple_poll) avahi_simple_poll_free(t->simple_poll);
  if (t->type) avahi_free(t->type);
  free_services(t->services);
  pthread_mutex_destroy(&t->mutex);
  free(t);
}

int avahi_avail(avahi_browser_t *t)
{
  int ret;
  pthread_mutex_lock(&t->mutex);
  if (!t)
    ret = 0;
  else if (t->ret < 0)
    ret = t->ret;
  else
    ret = t->avail;
  pthread_mutex_unlock(&t->mutex);
  return ret;
}

pure_expr *avahi_get(avahi_browser_t *t)
{
  pure_expr *ret;
  pthread_mutex_lock(&t->mutex);
  if (!t)
    ret = NULL;
  else if (t->ret < 0)
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
    t->avail = 0;
  }
  pthread_mutex_unlock(&t->mutex);
  return ret;
}
