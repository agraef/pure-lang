
/* Copyright (c) 2009 by Albert Graef <Dr.Graef@t-online.de>.

   This file is part of the Pure programming language and system.

   Pure is free software: you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   Pure is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along
   with this program.  If not, see <http://www.gnu.org/licenses/>. */

/* system headers */

#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include <pure/runtime.h>
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/xpath.h>
#include <libxslt/xslt.h>
#include <libxslt/xsltInternals.h>
#include <libxslt/transform.h>
#include <libxslt/xsltutils.h>

/* initialization *********************************************************/

/* symbol names */

#define sym_element pure_sym("xml::element")
#define sym_element_text pure_sym("xml::element_text")
#define sym_text pure_sym("xml::text")
#define sym_cdata pure_sym("xml::cdata")
#define sym_comment pure_sym("xml::comment")
#define sym_attr pure_sym("xml::attr")
#define sym_entity_ref pure_sym("xml::entity_ref")
#define sym_pi pure_sym("xml::pi")
  
#define sym_doctype pure_sym("xml::doctype")
  
#define sym_undefined_element pure_sym("xml::undefined_element")
#define sym_empty_element pure_sym("xml::empty_element")
#define sym_any_element pure_sym("xml::any_element")
#define sym_std_element pure_sym("xml::std_element")
#define sym_mixed_element pure_sym("xml::mixed_element")
  
#define sym_pcdata pure_sym("xml::pcdata")
#define sym_mksequence pure_sym("xml::mksequence")
#define sym_mkunion pure_sym("xml::mkunion")
#define sym_opt pure_sym("xml::opt")
#define sym_mult pure_sym("xml::mult")
#define sym_plus pure_sym("xml::plus")
  
#define sym_cdata_attr pure_sym("xml::cdata_attr")
#define sym_id_attr pure_sym("xml::id_attr")
#define sym_idref_attr pure_sym("xml::idref_attr")
#define sym_idrefs_attr pure_sym("xml::idrefs_attr")
#define sym_entity_attr pure_sym("xml::entity_attr")
#define sym_entities_attr pure_sym("xml::entities_attr")
#define sym_nmtoken_attr pure_sym("xml::nmtoken_attr")
#define sym_nmtokens_attr pure_sym("xml::nmtokens_attr")
#define sym_enum_attr pure_sym("xml::enum_attr")
#define sym_notation_attr pure_sym("xml::notation_attr")
  
#define sym_default pure_sym("xml::default")
#define sym_required pure_sym("xml::required")
#define sym_implied pure_sym("xml::implied")
#define sym_fixed pure_sym("xml::fixed")
  
#define sym_int_entity pure_sym("xml::int_entity")
#define sym_int_param_entity pure_sym("xml::int_param_entity")
#define sym_ext_entity pure_sym("xml::ext_entity")
#define sym_ext_param_entity pure_sym("xml::ext_param_entity")

/* static buffer for parsing and constructing qualified names */

static int bufsz = -1;
static char *buf = NULL;

static char *mkbuf(int size)
{
  if (!buf) {
    buf = malloc(size+1);
    if (buf)
      bufsz = size+1;
    else
      return NULL;
  } else if (size+1 > bufsz) {
    char *buf1 = realloc(buf, size+1);
    if (buf1) {
      buf = buf1;
      bufsz = size+1;
    } else
      return NULL;
  }
  return buf;
}

/* helper functions *********************************************************/

/* This equips an expression with a sentry which also serves typechecking
   purposes. NOTE: This isn't 100% foolproof, but should work ok as long as
   the user doesn't fiddle around with the sentries. */

static inline pure_expr *mktoken(const char *name, pure_expr *x, void *token)
{
  return pure_sentry(pure_app(pure_symbol(pure_sym(name)),
			      pure_pointer(token)), x);
}

static inline bool chktoken(pure_expr*x, void *p)
{
  void *q;
  pure_expr *f, *y, *s = pure_get_sentry(x);
  return s && pure_is_app(s, &f, &y) && pure_is_pointer(y, &q) && p == q;
}

/* parser flags */

#define XML_DTDLOAD	0x01
#define XML_DTDVALID	0x02
#define XML_PEDANTIC	0x04
#define XML_SUBENT	0x08
#define XML_NOBLANKS	0x10

#ifdef _WIN32
/* correct garbled declarations in libxml/globals.h */
__declspec(dllimport) int xmlLoadExtDtdDefaultValue;
__declspec(dllimport) int xmlDoValidityCheckingDefaultValue;
__declspec(dllimport) int xmlPedanticParserDefaultValue;
__declspec(dllimport) int xmlSubstituteEntitiesDefaultValue;
__declspec(dllimport) int xmlKeepBlanksDefaultValue;
__declspec(dllimport) int xmlIndentTreeOutput;
#endif

static unsigned set_flags(unsigned flags)
{
  unsigned oldflags = (!!xmlLoadExtDtdDefaultValue)*XML_DTDLOAD |
    (!!xmlDoValidityCheckingDefaultValue)*XML_DTDVALID |
    (!!xmlPedanticParserDefaultValue)*XML_PEDANTIC |
    (!!xmlSubstituteEntitiesDefaultValue)*XML_SUBENT |
    (!xmlKeepBlanksDefaultValue)*XML_NOBLANKS;
  xmlLoadExtDtdDefaultValue = !!(flags&XML_DTDLOAD);
  xmlDoValidityCheckingDefaultValue = !!(flags&XML_DTDVALID);
  xmlPedanticParserDefaultValue = !!(flags&XML_PEDANTIC);
  xmlSubstituteEntitiesDefaultValue = !!(flags&XML_SUBENT);
  xmlKeepBlanksDefaultValue = !(flags&XML_NOBLANKS);
  return oldflags;
}

/* supported node types */

static bool supported(xmlNodePtr node)
{
  switch (node->type) {
  case XML_ELEMENT_NODE:
  case XML_ATTRIBUTE_NODE:
  case XML_ENTITY_REF_NODE:
  case XML_TEXT_NODE:
  case XML_CDATA_SECTION_NODE:
  case XML_COMMENT_NODE:
  case XML_PI_NODE:
  case XML_DTD_NODE:
  case XML_ELEMENT_DECL:
  case XML_ATTRIBUTE_DECL:
  case XML_ENTITY_DECL:
    return true;
  default:
    return false;
  }
}

/* maintain node and doc references */

static void chknode(xmlNodePtr node)
{
  if (node->_private)
    xmlUnlinkNode(node);
  else {
    xmlNodePtr next;
    for (node = node->children; node; node = next) {
      next = node->next;
      chknode(node);
    }
  }
}

static void newdocref(xmlNodePtr node)
{
  if (node != (xmlNodePtr)node->doc && node->doc && node->doc->_private)
    pure_new(node->doc->_private);
}

static void freedocref(xmlNodePtr node)
{
  if (node != (xmlNodePtr)node->doc && node->doc && node->doc->_private)
    pure_unref(node->doc->_private);
}

static void freenode(xmlNodePtr node)
{
  if (!supported(node))
    /* Better don't touch this one ... There's a potential mem leak here, but
       we shouldn't get an unsupported node type in the first place. */
    ;
  else if (node->_private)
    /* Dangling reference -- will be collected later. */
    ;
  else if (node->type == XML_ATTRIBUTE_NODE)
    xmlFreeProp((xmlAttrPtr)node);
  else
    xmlFreeNode(node);
}

/* qualified element and attribute names */

static const char *splitname(const char *name, const char **prefix)
{
  char *pos = strchr(name, ':');
  if (pos) {
    int p = pos-name;
    char *buf = mkbuf(strlen(name));
    if (!buf) {
      *prefix = NULL;
      return NULL;
    }
    strcpy(buf, name);
    buf[p] = 0;
    *prefix = buf;
    return buf+p+1;
  } else {
    *prefix = NULL;
    return name;
  }
}

static const char *mkname(const char *name, const char *prefix)
{
  char *buf;
  if (!prefix || !*prefix)
    return name;
  buf = mkbuf(strlen(name)+strlen(prefix));
  if (buf) sprintf(buf, "%s:%s", prefix, name);
  return buf;
}

static const char *mkname2(const char *name, xmlNsPtr ns)
{
  if (ns)
    return mkname(name, (char*)ns->prefix);
  else
    return name;
}

static const char *nodename(xmlNodePtr node)
{
  return mkname2((char*)node->name, node->ns);
}

static const char *attrname(xmlAttrPtr attr)
{
  return mkname2((char*)attr->name, attr->ns);
}

/* namespaces */

static xmlNsPtr mkns(xmlDocPtr doc, xmlNodePtr parent, xmlNodePtr node,
		     const char *prefix)
{
  xmlNsPtr ns = xmlSearchNs(doc, node, (const xmlChar*)prefix);
  if (!ns && parent && node->parent != parent)
    ns = xmlSearchNs(doc, parent, (const xmlChar*)prefix);
  return ns;
}

/* parse namespaces, attributes and node infos */

static bool parse_namespace(pure_expr *namespace, const char **prefix,
			    const char **href)
{
  pure_expr **xs;
  size_t n;
  *prefix = *href = NULL;
  if (pure_is_tuplev(namespace, &n, &xs))
    if (n == 1 && pure_is_string(xs[0], href)) {
      free(xs);
      return true;
    } else if (n == 2 && pure_is_string(xs[0], prefix) &&
	       pure_is_string(xs[1], href)) {
      free(xs);
      return true;
    } else {
      free(xs);
      return false;
    }
  else
    return false;
}

static bool parse_namespaces(xmlNodePtr node, pure_expr *namespaces)
{
  pure_expr **xs;
  size_t n;
  const char *prefix, *href;
  if (pure_is_listv(namespaces, &n, &xs)) {
    size_t i;
    for (i = 0; i < n; i++)
      if (!(parse_namespace(xs[i], &prefix, &href) && (prefix || href) &&
	    xmlNewNs(node, (const xmlChar*)href, (const xmlChar*)prefix))) {
	free(xs);
	return false;
      }
    free(xs);
    return true;
  } else
    return false;
}

static bool parse_attr(pure_expr* attr, const char **key, const char **val)
{
  pure_expr *f, **xs;
  size_t n;
  int32_t sym;
  if (pure_is_appv(attr, &f, &n, &xs))
    if (pure_is_symbol(f, &sym) && sym == pure_sym("=>") && n == 2 &&
	pure_is_string(xs[0], key) && pure_is_string(xs[1], val)) {
      free(xs);
      return true;
    } else {
      free(xs);
      return true;
    }
  else
    return true;
}

static bool parse_attrs(xmlDocPtr doc, xmlNodePtr parent, xmlNodePtr node,
			pure_expr *attrs)
{
  pure_expr **xs;
  size_t n;
  if (pure_is_listv(attrs, &n, &xs)) {
    size_t i;
    for (i = 0; i < n; i++) {
      const char *key, *val;
      if (parse_attr(xs[i], &key, &val)) {
	const char *prefix, *name = splitname(key, &prefix);
	xmlNsPtr ns = mkns(doc, parent, node, prefix);
	if ((prefix && !ns) ||
	    !xmlNewNsProp(node, ns, (const xmlChar*)name,
			  (const xmlChar*)val)) {
	  free(xs);
	  return false;
	}
      } else {
	free(xs);
	return false;
      }
    }
    free(xs);
    return true;
  } else
    return false;
}

static xmlNodePtr parse_info(xmlDocPtr doc, xmlNodePtr parent, pure_expr *info)
{
  xmlNodePtr node = NULL;
  const char *s, *t;
  int32_t sym;
  pure_expr *f, *g, *h, *k, *x, *y, *z, *u;
  if (!pure_is_app(info, &f, &x))
    return NULL;
  if (pure_is_symbol(f, &sym))
    if (sym == sym_text && pure_is_string(x, &s))
      node = xmlNewText((const xmlChar*)s);
    else if (sym == sym_cdata && pure_is_string(x, &s))
      node = xmlNewCDataBlock(doc, (const xmlChar*)s, strlen(s));
    else if (sym == sym_comment && pure_is_string(x, &s))
      node = xmlNewComment((const xmlChar*)s);
    else if (sym == sym_entity_ref && pure_is_string(x, &s))
      node = xmlNewReference(doc, (const xmlChar*)s);
    else
      return NULL;
  else if (!pure_is_app(f, &g, &y))
    return NULL;
  else if (pure_is_symbol(g, &sym) && sym == sym_pi &&
	   pure_is_string(y, &s) && pure_is_string(x, &t))
    node = xmlNewPI((const xmlChar*)s, (const xmlChar*)t);
  else if (!pure_is_app(g, &h, &z))
    return NULL;
  else if (pure_is_symbol(h, &sym) && sym == sym_element &&
	   pure_is_string(z, &s)) {
    const char *prefix, *name = splitname(s, &prefix);
    node = xmlNewNode(NULL, (const xmlChar*)name);
    if (!node) return NULL;
    if (!parse_namespaces(node, y)) {
      xmlFreeNode(node);
      return NULL;
    }
    if (!parse_attrs(doc, parent, node, x)) {
      xmlFreeNode(node);
      return NULL;
    }
    node->ns = mkns(doc, parent, node, prefix);
    if (prefix && !node->ns) {
      xmlFreeNode(node);
      return NULL;
    }
  } else if (!pure_is_app(h, &k, &u))
    return NULL;
  else if (pure_is_symbol(k, &sym) && sym == sym_element_text &&
	   pure_is_string(u, &s) && pure_is_string(x, &t)) {
    const char *prefix, *name = splitname(s, &prefix);
    node = xmlNewNode(NULL, (const xmlChar*)name);
    if (!node) return NULL;
    if (!parse_namespaces(node, z)) {
      xmlFreeNode(node);
      return NULL;
    }
    if (!parse_attrs(doc, parent, node, y)) {
      xmlFreeNode(node);
      return NULL;
    }
    node->ns = mkns(doc, parent, node, prefix);
    if (prefix && !node->ns) {
      xmlFreeNode(node);
      return NULL;
    }
    xmlNodeAddContent(node, (const xmlChar*)t);
  }
  return node;
}

/* construct different kinds of return values */

static pure_expr *pure_string_void(const char *s)
{
  if (s)
    return pure_string_dup(s);
  else
    return pure_tuplel(0);
}

static pure_expr *pure_string_null(const char *s)
{
  if (s)
    return pure_string_dup(s);
  else
    return pure_string_dup("");
}

static pure_expr *pure_string_ret(xmlChar *s)
{
  if (s) {
    pure_expr *ret = pure_string_dup((const char*)s);
    xmlFree(s);
    return ret;
  } else
    return NULL;
}

static int doc_token;

static pure_expr *pure_doc(xmlDocPtr doc)
{
  if (!doc)
    return pure_pointer(0);
  else if (doc->_private)
    return doc->_private;
  else {
    doc->_private = mktoken("xml::free_doc", pure_pointer(doc), &doc_token);
    return doc->_private;
  }
}

static int node_token;

static pure_expr *pure_node(xmlNodePtr node)
{
  if (!node)
    return pure_pointer(0);
  else if (node->_private)
    return node->_private;
  else {
    node->_private = mktoken("xml::free_node", pure_pointer(node),
			     &node_token);
    newdocref(node);
    return node->_private;
  }
}

static pure_expr *pure_extid(const char *extid, const char *sysid)
{
  return pure_tuplel(2, pure_string_void(extid), pure_string_void(sysid));
}

static pure_expr *pure_content(xmlElementContentPtr content)
{
  pure_expr *c;
  switch (content->type) {
  case XML_ELEMENT_CONTENT_PCDATA:
    c = pure_symbol(sym_pcdata);
    break;
  case XML_ELEMENT_CONTENT_ELEMENT:
    c = pure_string_null(mkname((char*)content->name, (char*)content->prefix));
    break;
  case XML_ELEMENT_CONTENT_SEQ:
    c = pure_appl(pure_symbol(sym_mksequence), 2, pure_content(content->c1),
		  pure_content(content->c2));
    break;
  case XML_ELEMENT_CONTENT_OR:
    c = pure_appl(pure_symbol(sym_mkunion), 2, pure_content(content->c1),
		  pure_content(content->c2));
    break;
  default:
    return NULL;
  }
  switch (content->ocur) {
  case XML_ELEMENT_CONTENT_ONCE:
    return c;
  case XML_ELEMENT_CONTENT_OPT:
    return pure_app(pure_symbol(sym_opt), c);
  case XML_ELEMENT_CONTENT_MULT:
    return pure_app(pure_symbol(sym_mult), c);
  case XML_ELEMENT_CONTENT_PLUS:
    return pure_app(pure_symbol(sym_plus), c);
  default:
    return NULL;
  }
}

static pure_expr *pure_enum(xmlEnumerationPtr ptr)
{
  size_t n = 0;
  xmlEnumerationPtr p;
  for (p = ptr; p; p = p->next) n++;
  if (n >= 0) {
    pure_expr **xs, *ret;
    xs = malloc(n*sizeof(pure_expr*));
    if (!xs) return NULL;
    for (n = 0, p = ptr; p; p = p->next)
      xs[n++] = pure_string_null((const char*)p->name);
    ret = pure_listv(n, xs);
    free(xs);
    return ret;
  } else
    return pure_listl(0);
}

/* Type checkers. */

static bool pure_is_doc(pure_expr *x, xmlDocPtr *style)
{
  void *p;
  if (pure_is_pointer(x, &p) && p && chktoken(x, &doc_token)) {
    *style = (xmlDocPtr)p;
    return true;
  } else
    return false;
}

static bool pure_is_node(pure_expr *x, xmlNodePtr *style)
{
  void *p;
  if (pure_is_pointer(x, &p) && p && chktoken(x, &node_token)) {
    *style = (xmlNodePtr)p;
    return true;
  } else
    return false;
}

/* XML interface ************************************************************/

void xml_free_doc(void *token, pure_expr *x)
{
  void *ptr;
  if (pure_is_pointer(x, &ptr) && ptr && token == &doc_token) {
    xmlDocPtr doc = (xmlDocPtr)ptr;
    xmlFreeDoc(doc);
    x->data.p = NULL;
  }
}

void xml_free_node(void *token, pure_expr *x)
{
  void *ptr;
  if (pure_is_pointer(x, &ptr) && ptr && token == &node_token) {
    xmlNodePtr node = (xmlNodePtr)ptr;
    node->_private = NULL;
    if (!node->parent &&
	(!node->doc ||
	 ((xmlNodePtr)node->doc->extSubset != node &&
	  (xmlNodePtr)node->doc->intSubset != node))) {
      xmlNodePtr n, next;
      for (n = node->children; n; n = next) {
	next = n->next;
	chknode(n);
      }
      freedocref(node);
      freenode(node);
    } else 
      freedocref(node);
    x->data.p = NULL;
  }
}

bool xml_docp(pure_expr *x)
{
  xmlDocPtr doc;
  return pure_is_doc(x, &doc);
}

bool xml_nodep(pure_expr *x)
{
  xmlNodePtr node;
  return pure_is_node(x, &node);
}

pure_expr *xml_new_doc(const char *version, pure_expr *dtd_info,
		       pure_expr *info)
{
  const char *extid = NULL, *sysid = NULL;
  pure_expr **xs;
  size_t n;
  xmlDocPtr doc;
  xmlNodePtr root;
  if (version && !*version) version = NULL; // "" => default version
  if (pure_is_string(dtd_info, &sysid))
    ; // string => DTD URI
  else if (pure_is_tuplev(dtd_info, &n, &xs)) {
    if (n == 2 &&
	pure_is_string(xs[0], &extid) && pure_is_string(xs[1], &sysid))
      // pair => DTD name and URI
      free(xs);
    else if (n != 0) {
      free(xs);
      return NULL;
    }
  } else
    return NULL;
  doc = xmlNewDoc((const xmlChar*)version);
  if (!doc) return NULL;
  root = parse_info(doc, (xmlNodePtr)doc, info);
  if (!root) {
    xmlFreeDoc(doc);
    return NULL;
  }
  xmlDocSetRootElement(doc, root);
  if (!root->name) {
    xmlFreeDoc(doc);
    return NULL;
  }
  root->parent = (xmlNodePtr)doc;
  root->doc = doc;
  if ((extid || sysid)) {
    xmlDtdPtr dtd = xmlParseDTD((const xmlChar*)extid, (const xmlChar*)sysid);
    if (!dtd) {
      xmlFreeDoc(doc);
      return NULL;
    }
    dtd->name = xmlStrdup(root->name); /* FIXME: do we need a qualid here? */
    doc->intSubset = dtd;
    if (doc->children == NULL)
      xmlAddChild((xmlNodePtr)doc, (xmlNodePtr)dtd);
    else
      xmlAddPrevSibling(doc->children, (xmlNodePtr)dtd);
  }
  return pure_doc(doc);
}

pure_expr *xml_load_file(const char *s, uint32_t flags)
{
  unsigned oldflags = set_flags(flags);
  xmlDocPtr doc = xmlParseFile(s);
  set_flags(oldflags);
  if (doc)
    return pure_doc(doc);
  else
    return NULL;
}

pure_expr *xml_load_string(const char *s, uint32_t flags)
{
  unsigned oldflags = set_flags(flags);
  xmlDocPtr doc = xmlParseDoc((const xmlChar*)s);
  set_flags(oldflags);
  if (doc)
    return pure_doc(doc);
  else
    return NULL;
}

pure_expr *xml_save_file(const char *s, pure_expr *docptr,
			 const char *enc, int32_t compression)
{
  xmlDocPtr doc;
  if (pure_is_doc(docptr, &doc) && xmlDocGetRootElement(doc)) {
    int save_compression = doc->compression, res,
      save_indent = xmlIndentTreeOutput;
    // compression<0 => default compression
    if (compression >= 0) doc->compression = compression;
    if (enc && !*enc) enc = NULL; // "" => default encoding
    xmlIndentTreeOutput = 1;
    res = xmlSaveFormatFileEnc(s, doc, enc, 1);
    xmlIndentTreeOutput = save_indent;
    doc->compression = save_compression;
    if (res >= 0)
      return pure_tuplel(0);
    else
      return NULL;
  } else
    return NULL;
}

pure_expr *xml_save_string(pure_expr *docptr)
{
  xmlDocPtr doc;
  if (pure_is_doc(docptr, &doc) && xmlDocGetRootElement(doc)) {
    xmlChar *s = NULL;
    int len, save_indent = xmlIndentTreeOutput;
    xmlIndentTreeOutput = 1;
    xmlDocDumpFormatMemoryEnc(doc, &s, &len, "UTF-8", 1);
    xmlIndentTreeOutput = save_indent;
    return pure_string_ret(s);
  } else
    return NULL;
}

pure_expr *xml_doc_info(pure_expr *docptr)
{
  xmlDocPtr doc;
  if (pure_is_doc(docptr, &doc))
    return pure_tuplel(5, pure_string_null((char*)doc->version),
		       pure_string_null((char*)doc->encoding),
		       pure_string_null((char*)doc->URL),
		       pure_int(doc->compression),
		       pure_int(doc->standalone));
  else
    return NULL;
}

pure_expr *xml_int_subset(pure_expr *docptr)
{
  xmlDocPtr doc;
  if (pure_is_doc(docptr, &doc) && doc->intSubset)
    return pure_node((xmlNodePtr)doc->intSubset);
  else
    return NULL;
}

pure_expr *xml_ext_subset(pure_expr *docptr)
{
  xmlDocPtr doc;
  if (pure_is_doc(docptr, &doc) && doc->extSubset)
    return pure_node((xmlNodePtr)doc->extSubset);
  else
    return NULL;
}

pure_expr *xml_root(pure_expr *docptr)
{
  xmlDocPtr doc;
  if (pure_is_doc(docptr, &doc)) {
    xmlNodePtr root = xmlDocGetRootElement(doc);
    if (!root)
      return NULL;
    else
      return pure_node(root);
  } else
    return NULL;
}

pure_expr *xml_doc(pure_expr *nodeptr)
{
  xmlNodePtr node;
  if (pure_is_node(nodeptr, &node) && supported(node)) {
    xmlDocPtr doc = node->doc;
    if (!doc || !doc->_private)
      return NULL;
    else
      return doc->_private;
  } else
    return NULL;
}

pure_expr *xml_parent(pure_expr *nodeptr)
{
  xmlNodePtr node;
  if (pure_is_node(nodeptr, &node) && supported(node)) {
    xmlNodePtr parent = node->parent;
    if (!parent)
      return NULL;
    else
      return pure_node(parent);
  } else
    return NULL;
}

pure_expr *xml_first(pure_expr *nodeptr)
{
  xmlNodePtr node;
  if (pure_is_node(nodeptr, &node) && supported(node) &&
      node->type != XML_ENTITY_REF_NODE &&
      node->type != XML_ATTRIBUTE_NODE) {
    xmlNodePtr first = node->children;
    if (!first)
      return NULL;
    else
      return pure_node(first);
  } else
    return NULL;
}

pure_expr *xml_last(pure_expr *nodeptr)
{
  xmlNodePtr node;
  if (pure_is_node(nodeptr, &node) && supported(node) &&
      node->type != XML_ENTITY_REF_NODE &&
      node->type != XML_ATTRIBUTE_NODE) {
    xmlNodePtr last = node->last;
    if (!last)
      return NULL;
    else
      return pure_node(last);
  } else
    return NULL;
}

pure_expr *xml_next(pure_expr *nodeptr)
{
  xmlNodePtr node;
  if (pure_is_node(nodeptr, &node) && supported(node)) {
    xmlNodePtr next = node->next;
    if (!next)
      return NULL;
    else
      return pure_node(next);
  } else
    return NULL;
}

pure_expr *xml_prev(pure_expr *nodeptr)
{
  xmlNodePtr node;
  if (pure_is_node(nodeptr, &node) && supported(node)) {
    xmlNodePtr prev = node->prev;
    if (!prev)
      return NULL;
    else
      return pure_node(prev);
  } else
    return NULL;
}

pure_expr *xml_first_attr(pure_expr *nodeptr)
{
  xmlNodePtr node;
  if (pure_is_node(nodeptr, &node)) {
    xmlAttrPtr attr = node->properties;
    if (attr)
      return pure_node((xmlNodePtr)attr);
    else
      return NULL;
  } else
    return NULL;
}

pure_expr *xml_last_attr(pure_expr *nodeptr)
{
  xmlNodePtr node;
  if (pure_is_node(nodeptr, &node)) {
    xmlAttrPtr attr = node->properties;
    if (attr) {
      while (attr->next)
	attr = attr->next;
      return pure_node((xmlNodePtr)attr);
    } else
      return NULL;
  } else
    return NULL;
}

pure_expr *xml_select(pure_expr *docptr, pure_expr *x)
{
  xmlDocPtr doc;
  const char *s;
  size_t i, n;
  pure_expr **nodes, *res;
  xmlXPathContextPtr context;
  xmlXPathObjectPtr result;
  xmlNodeSetPtr nodeset;
  if (!pure_is_doc(docptr, &doc) || !pure_is_string(x, &s))
    return NULL;
  context = xmlXPathNewContext(doc);
  if (!context) return NULL;
  result = xmlXPathEvalExpression((const xmlChar*)s, context);
  xmlXPathFreeContext(context);
  if (!result) return NULL;
  if (!(nodeset = result->nodesetval)) {
    xmlXPathFreeObject(result);
    return NULL;
  }
  n = nodeset->nodeNr;
  if (n == 0) {
    xmlXPathFreeObject(result);
    return pure_listl(0);
  }
  nodes = malloc(n*sizeof(pure_expr*));
  if (!nodes) {
    xmlXPathFreeObject(result);
    return NULL;
  }
  for (i = 0; i < n; i++) {
    xmlNodePtr nodeptr = nodeset->nodeTab[i];
    if (!nodeptr) {
      size_t j;
      xmlXPathFreeObject(result);
      for (j = 0; j < i; j++)
	pure_freenew(nodes[j]);
      free(nodes);
      return NULL;
    }
    nodes[i] = pure_node(nodeptr);
  }
  xmlXPathFreeObject(result);
  res = pure_listv(n, nodes);
  free(nodes);
  return res;
}

pure_expr *xml_node_info(pure_expr *nodeptr)
{
  xmlNodePtr node;
  if (!pure_is_node(nodeptr, &node)) return NULL;
  if (!supported(node)) {
#if 0
    printf("unrecognized node type %d\n", node->type);
#endif
    return NULL;
  }
  switch (node->type) {
  case XML_ELEMENT_NODE: {
    pure_expr *name = pure_string_null(nodename(node));
    pure_expr *namespaces, *attrs;
    xmlNsPtr nsdef = node->nsDef;
    xmlAttrPtr attr = node->properties;
    if (nsdef) {
      xmlNsPtr ns;
      pure_expr **xs;
      size_t n = 0;
      for (ns = nsdef; ns; ns = ns->next) n++;
      xs = malloc(n*sizeof(pure_expr*));
      if (!xs) {
	pure_freenew(name);
	return NULL;
      }
      for (n = 0, ns = nsdef; ns; ns = ns->next)
	xs[n++] = pure_tuplel(2, pure_string_void((const char*)ns->prefix),
			      pure_string_void((const char*)ns->href));
      namespaces = pure_listv(n, xs);
      free(xs);
    } else
      namespaces = pure_listl(0);
    if (attr) {
      xmlAttrPtr a;
      pure_expr **xs;
      size_t n = 0;
      for (a = attr; a; a = a->next) n++;
      xs = malloc(n*sizeof(pure_expr*));
      if (!xs) {
	pure_freenew(name);
	pure_freenew(namespaces);
	return NULL;
      }
      for (n = 0, a = attr; a; a = a->next) {
	const char *s = (const char*)a->children->content;
	pure_expr *val = pure_string_null(s);
	xs[n++] = pure_appl(pure_symbol(pure_sym("=>")), 2,
			    pure_string_null(attrname(a)),
			    val);
      }
      attrs = pure_listv(n, xs);
      free(xs);
    } else
      attrs = pure_listl(0);
    return pure_appl(pure_symbol(sym_element), 3, name, namespaces, attrs);
  }
  case XML_ATTRIBUTE_NODE: {
    pure_expr *name = pure_string_null(nodename(node));
    pure_expr *val = pure_string_null((const char*)node->children->content);
    return pure_appl(pure_symbol(sym_attr), 2, name, val);
  }
  case XML_ENTITY_REF_NODE: {
    pure_expr *name = pure_string_null(nodename(node));
    return pure_app(pure_symbol(sym_entity_ref), name);
  }
  case XML_TEXT_NODE: {
    pure_expr *content = pure_string_null((const char*)node->content);
    return pure_app(pure_symbol(sym_text), content);
  }
  case XML_CDATA_SECTION_NODE: {
    pure_expr *content = pure_string_null((const char*)node->content);
    return pure_app(pure_symbol(sym_cdata), content);
  }
  case XML_COMMENT_NODE: {
    pure_expr *content = pure_string_null((const char*)node->content);
    return pure_app(pure_symbol(sym_comment), content);
  }
  case XML_PI_NODE: {
    pure_expr *name = pure_string_null(nodename(node));
    pure_expr *content = pure_string_null((const char*)node->content);
    return pure_appl(pure_symbol(sym_pi), 2, name, content);
  }
  case XML_DTD_NODE: {
    xmlDtdPtr dtd = (xmlDtdPtr)node;
    pure_expr *name = pure_string_null((const char*)dtd->name);
    pure_expr *extid = pure_extid((const char*)dtd->ExternalID,
				  (const char*)dtd->SystemID);
    return pure_appl(pure_symbol(sym_doctype), 2, name, extid);
  }
  case XML_ELEMENT_DECL: {
    xmlElementPtr element = (xmlElementPtr)node;
    pure_expr *name = pure_string_null(mkname((const char*)element->name,
					      (const char*)element->prefix));
    switch (element->etype) {
    case XML_ELEMENT_TYPE_UNDEFINED:
      return pure_app(pure_symbol(sym_undefined_element), name);
    case XML_ELEMENT_TYPE_EMPTY:
      return pure_app(pure_symbol(sym_empty_element), name);
    case XML_ELEMENT_TYPE_ANY:
      return pure_app(pure_symbol(sym_any_element), name);
    case XML_ELEMENT_TYPE_ELEMENT:
      return pure_appl(pure_symbol(sym_std_element), 2, name,
		       pure_content(element->content));
    case XML_ELEMENT_TYPE_MIXED:
      return pure_appl(pure_symbol(sym_mixed_element), 2, name,
		       pure_content(element->content));
    default:
      return NULL;
    }
  }
  case XML_ATTRIBUTE_DECL: {
    xmlAttributePtr attr = (xmlAttributePtr)node;
    pure_expr *name = pure_string_null(mkname((const char*)attr->name,
					      (const char*)attr->prefix));
    pure_expr *elem_name = pure_string_null((const char*)attr->elem);
    pure_expr *dflt;
    switch (attr->def) {
    case XML_ATTRIBUTE_NONE:
      dflt = pure_app(pure_symbol(sym_default),
		      pure_string_null((const char*)attr->defaultValue));
      break;
    case XML_ATTRIBUTE_REQUIRED:
      dflt = pure_symbol(sym_required);
      break;
    case XML_ATTRIBUTE_IMPLIED:
      dflt = pure_symbol(sym_implied);
      break;
    case XML_ATTRIBUTE_FIXED:
      dflt = pure_app(pure_symbol(sym_fixed),
		      pure_string_null((const char*)attr->defaultValue));
      break;
    default:
      return NULL;
    }
    switch (attr->atype) {
    case XML_ATTRIBUTE_CDATA:
      return pure_appl(pure_symbol(sym_cdata_attr), 3, elem_name, name, dflt);
    case XML_ATTRIBUTE_ID:
      return pure_appl(pure_symbol(sym_id_attr), 3, elem_name, name, dflt);
    case XML_ATTRIBUTE_IDREF:
      return pure_appl(pure_symbol(sym_idref_attr), 3, elem_name, name, dflt);
    case XML_ATTRIBUTE_IDREFS:
      return pure_appl(pure_symbol(sym_idrefs_attr), 3, elem_name, name, dflt);
    case XML_ATTRIBUTE_ENTITY:
      return pure_appl(pure_symbol(sym_entity_attr), 3, elem_name, name, dflt);
    case XML_ATTRIBUTE_ENTITIES:
      return pure_appl(pure_symbol(sym_entities_attr), 3, elem_name, name,
		       dflt);
    case XML_ATTRIBUTE_NMTOKEN:
      return pure_appl(pure_symbol(sym_nmtoken_attr), 3, elem_name, name,
		       dflt);
    case XML_ATTRIBUTE_NMTOKENS:
      return pure_appl(pure_symbol(sym_nmtokens_attr), 3, elem_name, name,
		       dflt);
    case XML_ATTRIBUTE_ENUMERATION:
    return pure_appl(pure_symbol(sym_enum_attr), 4, elem_name, name,
		     pure_enum(attr->tree), dflt);
    case XML_ATTRIBUTE_NOTATION:
    return pure_appl(pure_symbol(sym_notation_attr), 4, elem_name,
		     name, pure_enum(attr->tree), dflt);
    default:
      return NULL;
    }
  }
  case XML_ENTITY_DECL: {
    xmlEntityPtr entity = (xmlEntityPtr)node;
    pure_expr *name = pure_string_null((const char*)entity->name);
    pure_expr *content = pure_string_null((const char*)entity->content);
    switch (entity->etype) {
    case XML_INTERNAL_GENERAL_ENTITY:
      return pure_appl(pure_symbol(sym_int_entity), 2, name, content);
    case XML_INTERNAL_PARAMETER_ENTITY:
      return pure_appl(pure_symbol(sym_int_param_entity), 2, name, content);
    case XML_EXTERNAL_GENERAL_PARSED_ENTITY: {
      pure_expr *extid = pure_extid((const char*)entity->ExternalID,
				    (const char*)entity->SystemID);
      return pure_appl(pure_symbol(sym_ext_entity), 3, name, extid, content);
    }
    case XML_EXTERNAL_GENERAL_UNPARSED_ENTITY: {
      pure_expr *extid = pure_extid((const char*)entity->ExternalID,
				    (const char*)entity->SystemID);
      return pure_appl(pure_symbol(sym_ext_entity), 3, name, extid, content);
    }
    case XML_EXTERNAL_PARAMETER_ENTITY: {
      pure_expr *extid = pure_extid((const char*)entity->ExternalID,
				    (const char*)entity->SystemID);
      return pure_appl(pure_symbol(sym_ext_param_entity), 3, name, extid,
		       content);
    }
    default:
      return NULL;
    }
  }
  default:
    return NULL;
  }
}

pure_expr *xml_is_blank_node(pure_expr *nodeptr)
{
  xmlNodePtr node;
  if (pure_is_node(nodeptr, &node))
    return pure_int(xmlIsBlankNode(node));
  else
    return NULL;
}

pure_expr *xml_node_base(pure_expr *nodeptr)
{
  xmlNodePtr node;
  if (pure_is_node(nodeptr, &node) && node->doc) {
    xmlChar *s = xmlNodeGetBase(node->doc, node);
    return pure_string_ret(s);
  } else
    return NULL;
}

pure_expr *xml_node_path(pure_expr *nodeptr)
{
  xmlNodePtr node;
  if (pure_is_node(nodeptr, &node)) {
    xmlChar *s = xmlGetNodePath(node);
    return pure_string_ret(s);
  } else
    return NULL;
}

pure_expr *xml_node_content(pure_expr *nodeptr)
{
  xmlNodePtr node;
  if (pure_is_node(nodeptr, &node)) {
    xmlChar *s = xmlNodeGetContent(node);
    return pure_string_ret(s);
  } else
    return NULL;
}

pure_expr *xml_node_attr(pure_expr *nodeptr, pure_expr *x)
{
  xmlNodePtr node;
  const char *s;
  if (pure_is_node(nodeptr, &node) && node->type == XML_ELEMENT_NODE &&
      pure_is_string(x, &s)) {
    const char *prefix, *name = splitname(s, &prefix);
    xmlNsPtr ns = mkns(node->doc, node->parent, node, prefix);
    xmlChar *t;
    if (prefix && !ns) return NULL;
    t = xmlGetNsProp(node, (const xmlChar*)name, ns?ns->href:NULL);
    return pure_string_ret(t);
  } else
    return NULL;
}

pure_expr *xml_set_node_attr(pure_expr *nodeptr, pure_expr *x, pure_expr *y)
{
  xmlNodePtr node;
  const char *s, *t;
  if (pure_is_node(nodeptr, &node) && node->type == XML_ELEMENT_NODE &&
      pure_is_string(x, &s) && pure_is_string(y, &t)) {
    const char *prefix, *name = splitname(s, &prefix);
    xmlNsPtr ns = mkns(node->doc, node->parent, node, prefix);
    if (prefix && !ns) return NULL;
    if (xmlSetNsProp(node, ns, (const xmlChar*)name, (const xmlChar*)t))
      return pure_tuplel(0);
    else
      return NULL;
  } else
    return NULL;
}

/* These are reimplemented here, because the library versions are buggy
   (missing updates of prev pointers), and we also need to take care of
   dangling references in attribute node objects. */

static int
myxmlUnsetProp(xmlNodePtr node, const xmlChar *name)
{
  xmlAttrPtr prop = node->properties, prev = NULL;

  if ((node == NULL) || (name == NULL))
    return(-1);
  while (prop != NULL) {
    if ((xmlStrEqual(prop->name, name)) &&
	(prop->ns == NULL)) {
      if (prop->next) prop->next->prev = prev;
      if (prev == NULL)
	node->properties = prop->next;
      else
	prev->next = prop->next;
      prop->prev = prop->next = NULL;
      prop->parent = NULL;
      freenode((xmlNodePtr)prop);
      return(0);
    }
    prev = prop;
    prop = prop->next;
  }
  return(-1);
}

static int
myxmlUnsetNsProp(xmlNodePtr node, xmlNsPtr ns, const xmlChar *name)
{
  xmlAttrPtr prop = node->properties, prev = NULL;

  if ((node == NULL) || (name == NULL))
    return(-1);
  if (ns == NULL)
    return(myxmlUnsetProp(node, name));
  if (ns->href == NULL)
    return(-1);
  while (prop != NULL) {
    if ((xmlStrEqual(prop->name, name)) &&
	(((prop->ns == NULL) && (node->ns != NULL) &&
	  (xmlStrEqual(node->ns->href, ns->href))) ||
	 ((prop->ns != NULL) && (xmlStrEqual(prop->ns->href, ns->href))))) {
      if (prop->next) prop->next->prev = prev;
      if (prev == NULL)
	node->properties = prop->next;
      else
	prev->next = prop->next;
      prop->prev = prop->next = NULL;
      prop->parent = NULL;
      freenode((xmlNodePtr)prop);
      return(0);
    }
    prev = prop;
    prop = prop->next;
  }
  return(-1);
}

pure_expr *xml_unset_node_attr(pure_expr *nodeptr, pure_expr *x)
{
  xmlNodePtr node;
  const char *s;
  if (pure_is_node(nodeptr, &node) &&
      node->type == XML_ELEMENT_NODE && pure_is_string(x, &s)) {
    const char *prefix, *name = splitname(s, &prefix);
    xmlNsPtr ns = mkns(node->doc, node->parent, node, prefix);
    if (prefix && !ns) return NULL;
    if (!myxmlUnsetNsProp(node, ns, (const xmlChar*)name))
      return pure_tuplel(0);
    else
      return NULL;
  } else
    return NULL;
}

pure_expr *xml_replace(pure_expr *nodeptr, pure_expr *info)
{
  xmlNodePtr node, new_node;
  if (pure_is_node(nodeptr, &node) && supported(node) &&
      node->type != XML_ATTRIBUTE_NODE &&
      (new_node = parse_info(node->doc, node->parent, info))) {
    xmlReplaceNode(node, new_node);
    return pure_node(new_node);
  } else
    return NULL;
}

pure_expr *xml_add_first(pure_expr *nodeptr, pure_expr *info)
{
  xmlNodePtr node, new_node;
  if (pure_is_node(nodeptr, &node) && supported(node) &&
      node->type != XML_ENTITY_REF_NODE &&
      node->type != XML_ATTRIBUTE_NODE &&
      (new_node = parse_info(node->doc, node, info))) {
    xmlNodePtr res, first = node->children;
    if (first)
      res = xmlAddPrevSibling(first, new_node);
    else
      res = xmlAddChild(node, new_node);
    if (res)
      return pure_node(res);
    else {
      xmlFreeNode(new_node);
      return NULL;
    }
  } else
    return NULL;
}

pure_expr *xml_add_last(pure_expr *nodeptr, pure_expr *info)
{
  xmlNodePtr node, new_node;
  if (pure_is_node(nodeptr, &node) && supported(node) &&
      node->type != XML_ENTITY_REF_NODE &&
      node->type != XML_ATTRIBUTE_NODE &&
      (new_node = parse_info(node->doc, node, info))) {
    xmlNodePtr res, last = node->last;
    if (last)
      res = xmlAddNextSibling(last, new_node);
    else
      res = xmlAddChild(node, new_node);
    if (res)
      return pure_node(res);
    else {
      xmlFreeNode(new_node);
      return NULL;
    }
  } else
    return NULL;
}

pure_expr *xml_add_next(pure_expr *nodeptr, pure_expr *info)
{
  xmlNodePtr node, new_node;
  if (pure_is_node(nodeptr, &node) && supported(node) &&
      node->type != XML_ATTRIBUTE_NODE &&
      (new_node = parse_info(node->doc, node->parent, info))) {
    xmlNodePtr res = xmlAddNextSibling(node, new_node);
    if (res)
      return pure_node(res);
    else {
      xmlFreeNode(new_node);
      return NULL;
    }
  } else
    return NULL;
}

pure_expr *xml_add_prev(pure_expr *nodeptr, pure_expr *info)
{
  xmlNodePtr node, new_node;
  if (pure_is_node(nodeptr, &node) && supported(node) &&
      node->type != XML_ATTRIBUTE_NODE &&
      (new_node = parse_info(node->doc, node->parent, info))) {
    xmlNodePtr res = xmlAddPrevSibling(node, new_node);
    if (res)
      return pure_node(res);
    else {
      xmlFreeNode(new_node);
      return NULL;
    }
  } else
    return NULL;
}

pure_expr *xml_unlink(pure_expr *nodeptr)
{
  xmlNodePtr node;
  if (pure_is_node(nodeptr, &node)) {
    xmlUnlinkNode(node);
    return pure_tuplel(0);
  } else
    return NULL;
}

/* XSLT interface ***********************************************************/

static int stylesheet_token;

static bool pure_is_stylesheet(pure_expr *x, xsltStylesheetPtr *style)
{
  void *p;
  if (pure_is_pointer(x, &p) && p && chktoken(x, &stylesheet_token)) {
    *style = (xsltStylesheetPtr)p;
    return true;
  } else
    return false;
}

void xslt_free_stylesheet(void *token, pure_expr *x)
{
  void *ptr;
  if (pure_is_pointer(x, &ptr) && ptr && token == &stylesheet_token) {
    xsltStylesheetPtr style = (xsltStylesheetPtr)ptr;
    xsltFreeStylesheet(style);
    x->data.p = NULL;
  }
}

bool xslt_stylesheetp(pure_expr *x)
{
  xsltStylesheetPtr style;
  return pure_is_stylesheet(x, &style);
}

pure_expr *xslt_load_stylesheet(pure_expr *x)
{
  char *s = NULL;
  xmlDocPtr doc = NULL;
  if (pure_is_cstring_dup(x, &s) || pure_is_pointer(x, (void**)&doc)) {
    int sub_flag = xmlSubstituteEntitiesDefault(1),
      load_flag = xmlLoadExtDtdDefaultValue;
    xsltStylesheetPtr style;
    xmlLoadExtDtdDefaultValue = 1;
    if (s) {
      /* FIXME: Is the filename parameter of xsltParseStylesheetFile really an
	 UTF-8 string? As all other filename parameters in libxml/libxslt are
	 ordinary char*, we assume that here too. */
      style = xsltParseStylesheetFile((const xmlChar*)s);
      free(s);
    } else {
      doc = xmlCopyDoc(doc, 1);
      if (!doc) return NULL;
      style = xsltParseStylesheetDoc(doc);
      /* FIXME: do we need to free doc here?? */
    }
    xmlSubstituteEntitiesDefault(sub_flag);
    xmlLoadExtDtdDefaultValue = load_flag;
    if (style)
      return mktoken("xslt::free_stylesheet", pure_pointer(style),
		     &stylesheet_token);
    else
      return NULL;
  } else
    return NULL;
}

pure_expr *xslt_apply_stylesheet(pure_expr *styleptr, pure_expr *docptr,
				 pure_expr *x)
{
  xsltStylesheetPtr style;
  xmlDocPtr doc;
  pure_expr **xs, **ys, *f;
  size_t i, k, n, m;
  if (pure_is_stylesheet(styleptr, &style) && pure_is_doc(docptr, &doc) &&
      pure_is_listv(x, &n, &xs)) {
    int32_t sym;
    const char *key, *val;
    const char **params = malloc((2*n+1)*sizeof(char*));
    xmlDocPtr res;
    /* FIXME: It's not clear whether xsltApplyStylesheet expects system or
       UTF-8 encoded parameter strings. We just leave them in UTF-8 for
       now. */
    if (!params) return NULL;
    for (i = k = 0; i < n; i++)
      if (pure_is_appv(xs[i], &f, &m, &ys)) {
	if (pure_is_symbol(f, &sym) && sym == pure_sym("=>") && m==2 &&
	    pure_is_string(ys[0], &key) && pure_is_string(ys[1], &val)) {
	  free(ys);
	  params[k++] = key;
	  params[k++] = val;
	} else {
	  free(ys);
	  free(params);
	  return NULL;
	}
      } else {
	free(params);
	return NULL;
      }
    params[k++] = NULL;
    free(xs);
    res = xsltApplyStylesheet(style, doc, params);
    free(params);
    if (res)
      return pure_doc(res);
    else
      return NULL;
  } else
    return NULL;
}

pure_expr *xslt_save_result_file(const char *s,
				 pure_expr *docptr, pure_expr *styleptr,
				 uint32_t n)
{
  xmlDocPtr doc;
  xsltStylesheetPtr style;
  if (pure_is_doc(docptr, &doc) && pure_is_stylesheet(styleptr, &style)) {
    int res = xsltSaveResultToFilename(s, doc, style, n);
    if (res >= 0)
      return pure_tuplel(0);
    else
      return NULL;
  } else
    return NULL;
}

pure_expr *xslt_save_result_string(pure_expr *docptr, pure_expr *styleptr)
{
  xmlDocPtr doc;
  xsltStylesheetPtr style;
  if (pure_is_doc(docptr, &doc) && pure_is_stylesheet(styleptr, &style)) {
    /* FIXME: result might not be UTF-8 if parameters are not set properly!? */
    xmlChar *s = NULL;
    int len;
    xsltSaveResultToString(&s, &len, doc, style);
    return pure_string_ret(s);
  } else
    return NULL;
}
