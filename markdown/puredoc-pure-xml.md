<a name="doc-pure-xml"></a>

Pure-XML - XML/XSLT interface
=============================

<a name="module-xml"></a>

Version 0.7, March 06, 2017

Albert Graef &lt;<aggraef@gmail.com>&gt;

[XML](http://www.w3.org/TR/xml), the Extensible Markup Language, facilitates
the exchange of complex structured data between applications and systems.
[XSLT](http://www.w3.org/TR/xslt) allows you to transform XML documents to
other XML-based formats such as HTML. Together, XML and XSLT let you create
dynamic web content with ease. Both XML and XSLT are open standards by the W3C
consortium (<http://www.w3.org>).

Pure's XML interface is based on the libxml2 and libxslt libraries from the
GNOME project. If you have a Linux system then you most likely have these
libraries, otherwise you can get them from <http://xmlsoft.org>. For Windows
users, the required dlls are available from the GnuWin32 project
(<http://gnuwin32.sourceforge.net>) and are already included in the Pure MSI
package.

Copying
-------

Copyright (c) 2009 by Albert Graef &lt;<aggraef@gmail.com>&gt;.

pure-xml is free software: you can redistribute it and/or modify it under the
terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

pure-xml is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
details.

You should have received a copy of the GNU Lesser General Public License along
with this program. If not, see &lt;<http://www.gnu.org/licenses/>&gt;.

Installation
------------

Get the latest source from
<https://bitbucket.org/purelang/pure-lang/downloads/pure-xml-0.7.tar.gz>.

Run `make` and then `sudo make install` to compile and install this module.
This requires libxml2, libxslt and Pure.

Usage
-----

Use the following declaration to make the operations of this module available
in your programs:

    using xml;

The module defines two namespaces `xml` and `xslt` for the XML and the XSLT
operations, respectively. For convenience, you can open these in your program
as follows:

    using namespace xml, xslt;

A number of complete examples illustrating the use of this module can be found
in the examples directory in the source distribution.

Data Structure
--------------

This module represents XML documents using pointers to the `xmlDoc` and
`xmlNode` structures provided by the libxml2 library. Similarly, stylesheets
are simply pointers to the xmlStylesheet structure from libxslt (cf.
[Transformations](#transformations)). This makes it possible to use these
objects directly with the operations of the libxml2 and libsxslt libraries
(via Pure's C interface) if necessary. Note, however, that these are all
"cooked" pointers which take care of freeing themselves automatically when
they are no longer needed, therefore you shouldn't free them manually.

You can also check for these types of pointers using the following predicates:

<a name="xml::docp"></a>`xml::docp x`
:   checks whether `x` is an XML document pointer.

<a name="xml::nodep"></a>`xml::nodep x`
:   checks whether `x` is a pointer to a node in an XML document.

<a name="xslt::stylesheetp"></a>`xslt::stylesheetp x`
:   checks whether `x` is an XSLT stylesheet pointer.

<!-- -->
### The Document Tree

An XML document is a rooted tree which can be created, traversed and
manipulated using the operations of this module. There are different types of
nodes in the tree, each carrying their own type of data. In Pure land, the
node data is described using the following "node info" constructors.

<a name="node-info"></a>

<a name="xml::element"></a>`xml::element tag ns attrs`
:   An XML element with given (possibly qualified) name `tag`, a (possibly
    empty) list of namespace declarations `ns` and a (possibly empty) list of
    attributes `attrs`. Namespace declarations normally take the form of a
    pair of strings `(prefix,href)`, where `prefix` is the prefix associated
    with the namespace and `href` the corresponding URI (the name of the
    namespace), but they can also be just a string `href` if the namespace
    prefix is missing. Attributes are encoded as `key=>value` pairs, where
    `key` is the attribute name and `value` the associated value; both `key`
    and `value` are strings.

<a name="xml::element_text"></a>`xml::element_text tag ns attrs content`
:   A convenience function which denotes a combination of an element node with
    a text child. This is only used when creating a new node, and indicates
    that a text node child is to be added to the node automatically.

<a name="xml::attr"></a>`xml::attr key val`
:   An attribute node. These only occur as results of the [`select`](#select)
    and [`attrs`](#attrs) functions, and cannot be inserted directly into a
    document.

<a name="xml::text"></a>`xml::text content`
:   A text node with the given content (a string).

<a name="xml::cdata"></a>`xml::cdata content`
:   Like [`xml::text`](#xml::text), but contains unparsed character data.

<a name="xml::comment"></a>`xml::comment content`
:   A comment.

<a name="xml::entity_ref"></a>`xml::entity_ref name`
:   An entity reference (`&name;`).

<a name="xml::pi"></a>`xml::pi name content`
:   Processing instructions. `name` is the application name, `content` the
    text of the processing instructions.

<!-- -->
### Document Types

<a name="dtd"></a>

Besides the node types described above, there are some additional node types
used in the [document type
definition](http://www.w3.org/TR/REC-xml/#dt-doctype) (DTD), which can be
extracted from a document using the [`int_subset`](#int_subset) and
[`ext_subset`](#ext_subset) functions. These are for inspection purposes only;
it is not possible to change the DTD of a document in-place. (However, you can
create a new document and attach a DTD to it, using the [`new_doc`](#new_doc)
function.)

<a name="xml::doctype"></a>`xml::doctype name extid`
:   DTDs are represented using this special type of node, where `name` is the
    name of the root element, and `extid` is a pair consisting of the external
    identifier and the URI of the DTD (or just the URI if there is no external
    identifier). The [`xml::doctype`](#xml::doctype) node has as its children
    zero or more of the following kinds of DTD declaration nodes (these are
    just straightforward abstract syntax for the !ELEMENT, !ATTLIST and
    !ENTITY declarations inside a DTD declaration; see the XML specification
    for details).

<!-- -->
<a name="element-declaration"></a>

*Element declarations:* Here, `name` is the element tag and `content` the
definition of the element structure, see [element content](#element-content)
below. XML supports various kinds of element types, please refer to [document
type definition](http://www.w3.org/TR/REC-xml/#dt-doctype) in the XML
specification for details.

<a name="xml::undefined_element"></a>`xml::undefined_element name`
:   An undefined element. This is in libxml2, but not in the XML
    specification, you shouldn't see this in normal operation.

<a name="xml::empty_element"></a>`xml::empty_element name`
:   An element without any content.

<a name="xml::any_element"></a>`xml::any_element name`
:   An element with unrestricted content.

<a name="xml::mixed_element"></a>`xml::mixed_element name content`
:   A "mixed" element which can contain character data, optionally
    interspersed with child elements, as given in the `content` specification.

<a name="xml::std_element"></a>`xml::std_element name content`
:   A standard element consisting *only* of child elements, as given in the
    `content` specification.

<!-- -->
<a name="attribute-declaration"></a>

*Attribute declarations:* These are used to declare the attributes of an
element. `elem_name` is the name of an element which describes the attribute
type, `name` is the name of the attribute itself, and `default` specifies the
default value of the attribute, see [attribute defaults](#attribute-defaults)
below. XML supports a bunch of different attribute types, please refer to
[document type definition](http://www.w3.org/TR/REC-xml/#dt-doctype) in the
XML specification for details.

<a name="xml::cdata_attr"></a>`xml::cdata_attr elem_name name default`, <a name="xml::id_attr"></a>`xml::id_attr elem_name name default`, <a name="xml::idref_attr"></a>`xml::idref_attr elem_name name default`, <a name="xml::idrefs_attr"></a>`xml::idrefs_attr elem_name name default`, <a name="xml::entity_attr"></a>`xml::entity_attr elem_name name default`

:   

<a name="xml::entities_attr"></a>`xml::entities_attr elem_name name default`, <a name="xml::nmtoken_attr"></a>`xml::nmtoken_attr elem_name name default`, <a name="xml::nmtokens_attr"></a>`xml::nmtokens_attr elem_name name default`, <a name="xml::enum_attr"></a>`xml::enum_attr elem_name name vals default`

:   

<a name="xml::notation_attr"></a>`xml::notation_attr elem_name name vals default`

:   <!-- -->

<a name="entity-declaration"></a>

*Entity declarations:* These are used for internal and external entity
declarations. `name` is the entity name and `content` its definition. External
entities also have an `extid` (external identifier/URI pair) identifying the
entity.

<a name="xml::int_entity"></a>`xml::int_entity name content`, <a name="xml::int_param_entity"></a>`xml::int_param_entity name content`, <a name="xml::ext_entity"></a>`xml::ext_entity name extid content`, <a name="xml::ext_param_entity"></a>`xml::ext_param_entity name extid content`

:   <!-- -->

<a name="element-content"></a>

The element content type (`content` argument of the [element
declaration](#element-declaration) nodes) is a kind of regular expression
formed with tags (specified as strings) and the following constructors:

<a name="xml::pcdata"></a>`xml::pcdata`
:   text data (`#PCDATA`)

<a name="xml::sequence"></a>`xml::sequence xs`
:   concatenation (`x,y,z`)

<a name="xml::union"></a>`xml::union xs`
:   alternatives (`x|y|z`)

<a name="xml::opt"></a>`xml::opt x`
:   optional element (`x?`)

<a name="xml::mult"></a>`xml::mult x`
:   repeated element (`x*`)

<a name="xml::plus"></a>`xml::plus x`
:   non-optional repeated element (`x+`)

<!-- -->
<a name="attribute-defaults"></a>

Attribute defaults (the `default` argument of [attribute
declaration](#attribute-declaration) nodes) are represented using the
following constructor symbols:

<a name="xml::required"></a>`xml::required`
:   a required attribute, i.e., the user must specify this

<a name="xml::implied"></a>`xml::implied`
:   an implied attribute, i.e., the user does not have to specify this

<a name="xml::default"></a>`xml::default val`
:   an attribute with the given default value `val`

<a name="xml::fixed"></a>`xml::fixed val`
:   an attribute with the given fixed value `val`

<!-- -->
Operations
----------

This module provides all operations necessary to create, inspect and
manipulate XML documents residing either in memory or on disk. Operations for
formatting XML documents using XSLT stylesheets are also available.

### Document Operations

The following functions allow you to create new XML documents, load them from
or save them to a file or a string, and provide general information about a
document.

<a name="xml::new_doc"></a>`xml::new_doc version dtd info`

:   This function creates an XML document. It returns a pointer to the new
    document. `version` is a string denoting the XML version (or `""` to
    indicate the default). `info` is the [node info](#node-info) of the root
    node (which should denote an element node). `dtd` denotes the document
    type which can be `()` to denote an empty DTD, a string (the URI/filename
    of the DTD), or a pair `(pubid,sysid)` where `pubid` denotes the public
    identifier of the DTD and `sysid` its system identifier (URI).

    Note that only simple kinds of documents with an internal DTD can be
    created this way. Use the [`load_file`](#load_file) or
    [`load_string`](#load_string) function below to create a skeleton document
    if a more elaborate prolog is required.

<a name="xml::load_file"></a>`xml::load_file name flags`, <a name="xml::load_string"></a>`xml::load_string s flags`

:   Load an XML document from a file `name` or a string `s`. `flags` denotes
    the parser flags, a bitwise disjunction of any of the following constants,
    or 0 for the default:

    -   `xml::DTDLOAD`: load DTD
    -   `xml::DTDVALID`: validate
    -   `xml::PEDANTIC`: pedantic parse
    -   `xml::SUBENT`: substitute entities
    -   `xml::NOBLANKS`: suppress blank nodes

    The return value is the document pointer. These operations may also fail
    if there is a fatal error parsing the document.

<a name="xml::save_file"></a>`xml::save_file name doc encoding compression`, <a name="xml::save_string"></a>`xml::save_string doc`
:   Save an XML document `doc` to a file or a string. When saving to a file,
    `encoding` denotes the desired encoding (or `""` for the default),
    `compression` the desired level of zlib compression (0 means none, 9 is
    maximum, -1 indicates the default). Note that with
    [`xml::save_string`](#xml::save_string), the result is always encoded as
    UTF-8.

<a name="xml::doc_info"></a>`xml::doc_info doc`
:   Retrieve general information about a document. Returns a tuple
    `(version,encoding,url,compression,standalone)`, where `version` is the
    XML version of the document, `encoding` the external encoding (if any),
    `url` the name/location of the document (if any), `compression` the level
    of zlib compression, and `standalone` is a flag indicating whether the
    document contains any external markup declarations "which affect the
    information passed from the XML processor to the application", see the
    section on the [standalone document
    declaration](http://www.w3.org/TR/REC-xml/#sec-rmd) in the XML spec for
    details. (Apparently, in libxml2 `standalone` is either a truth value or
    one of the special values -1, indicating that there's no XML declaration
    in the prolog, or -2, indicating that there's an XML declaration but no
    `standalone` attribute.)

<!-- -->

<a name="xml::int_subset"></a>`xml::int_subset doc`, <a name="xml::ext_subset"></a>`xml::ext_subset doc`
:   Retrieve the internal and external DTD subset of a document. Returns a
    `doctype` node (fails if there's no corresponding DTD).

<!-- -->
**Example**

Read the sample.xml document distributed with the sources (ignoring blank
nodes) and retrieve the document info:

    > using xml;
    > let sample = xml::load_file "sample.xml" xml::NOBLANKS;
    > xml::doc_info sample;
    "1.0","","sample.xml",0,-2

### Traversing Documents

These operations are used to traverse the document tree, i.e., the nodes of
the document. They take either a document pointer `doc` or a node pointer
`node` as argument, and yield a corresponding node pointer (or a document
pointer, in the case of [`xml::doc`](#xml::doc)). The node pointers can then
be used with the [Node Information](#node-information) and [Node
Manipulation](#node-manipulation) operations described below.

<a name="xml::root"></a>`xml::root doc`
:   the root node of `doc`

<a name="xml::doc"></a>`xml::doc node`
:   the document `node` belongs to

<a name="xml::parent"></a>`xml::parent node`
:   the parent of `node`

<a name="xml::first"></a>`xml::first node`, <a name="xml::last"></a>`xml::last node`
:   first and last child node

<a name="xml::next"></a>`xml::next node`, <a name="xml::prev"></a>`xml::prev node`
:   next and previous sibling

<a name="xml::first_attr"></a>`xml::first_attr node`, <a name="xml::last_attr"></a>`xml::last_attr node`
:   first and last attribute node

<!-- -->
All these operations fail if the corresponding target node does not exist, or
if the type of the given node is not supported by this implementation.

There are also two convenience functions to retrieve the children and
attribute nodes of a node:

<a name="xml::children"></a>`xml::children node`
:   returns the list of all child nodes of `node`

<a name="xml::attrs"></a>`xml::attrs node`
:   returns the list of all attribute nodes of `node`

<!-- -->
Moreover, given a node pointer `node`, `node!i` can be used to retrieve the
`i`th child of `node`.

**Example**

Peek at the root node of the sample document and its children:

    > let r = xml::root sample; r;
    #<pointer 0xe15e10>
    > xml::node_info r;
    xml::element "story" [] []
    > #xml::children r;
    5
    > xml::node_info (r!0);
    xml::cdata "<greeting>Hello, world!</greeting>"

### Node Information

These operations retrieve information about the nodes of an XML document.

<a name="xml::select"></a>`xml::select doc xpath`, <a name="xml::select"></a>`xml::select doc (xpath,ns)`

:   Retrieve nodes using an [XPath](http://www.w3.org/TR/xpath) specification.
    Given an XPath (a string) `xpath`, this operation returns the list of all
    matching nodes in the given document `doc`. You can also specify a node as
    the first argument, in which case the document of the given node is
    searched and paths are interpreted relative to the given node (rather than
    the root node of the document).

    Moreover, instead of just an XPath you can also specify a pair
    `(xpath,ns)` consisting of an XPath `xpath` and a list `ns` of
    `prefix=>uri` string pairs which describe the namespaces to be recognized
    in the XPath expression. This is necessary to select nodes by qualified
    tag or attribute names. Note that only the namespace URIs must match up
    with those used in the queried document; the corresponding namespace
    prefixes can be chosen freely, so you can use whatever prefixes are
    convenient to formulate the XPath query. However, for each namespace
    prefix used in the XPath expression (not the document!), there *must* be a
    corresponding binding in the `ns` list. Otherwise the underlying libxml2
    function will complain about an undefined namespace prefix and
    [`xml::select`](#xml::select) will fail.

<a name="xml::node_info"></a>`xml::node_info node`
:   Retrieve the node data from `node`. Returns a [node info](#node-info)
    value, as described in [Data Structure](#data-structure) above. Fails if
    the node does not belong to one of the supported node types.

<a name="xml::is_blank_node"></a>`xml::is_blank_node`
:   Checks whether a node is a blank node (empty or whitespace only) and thus
    possibly ignorable.

<a name="xml::node_base"></a>`xml::node_base node`
:   Returns the base URI of the given node.

<a name="xml::node_path"></a>`xml::node_path node`
:   Returns the path of a node in the document, in the format accepted by
    [`select`](#select).

<a name="xml::node_content"></a>`xml::node_content node`
:   Returns the text carried by the node, if any (after entity substitution).

<!-- -->
In addition, you can retrieve and change attributes of element nodes with the
following operations:

<a name="xml::node_attr"></a>`xml::node_attr node name`
:   Retrieves the value of the attribute with the given `name` (after entity
    substitution).

<a name="xml::set_node_attr"></a>`xml::set_node_attr node name value`, <a name="xml::unset_node_attr"></a>`xml::unset_node_attr node name`
:   Sets or unsets an attribute value.

<!-- -->
**Examples**

Set and unset a node attribute:

    > xml::set_node_attr r "foo" "4711";
    ()
    > xml::node_info r;
    xml::element "story" [] ["foo"=>"4711"]
    > xml::node_attr r "foo";
    "4711"
    > xml::unset_node_attr r "foo";
    ()
    > xml::node_info r;
    xml::element "story" [] []

The [`select`](#select) function is *very* powerful, and probably the single
most important operation of this module if you want to extract information
from an existing XML document without traversing the entire structure. Here is
a very simple example of its use:

    > [xml::node_content n, xml::node_path n | n = xml::select sample "//author"];
    [("John Fleck","/story/storyinfo/author")]

Note that if the XPath expression contains qualified names, the corresponding
namespace prefixes and their URIs must be given in the second argument along
with the XPath, as follows:

    xml::select doc ("//foo:bar", ["foo"=>"http://www.foo.org"]);

### Node Manipulation

These operations enable you to manipulate the document structure by adding a
new node to the document tree (specified through its [node info](#node-info)),
and by removing (unlinking) existing nodes from the tree.

<a name="xml::replace"></a>`xml::replace node info`
:   Add the new node specified by `info` in place of the given node `node`.

<a name="xml::add_first"></a>`xml::add_first node info`, <a name="xml::add_last"></a>`xml::add_last node info`
:   Add the new node as the first or last child of `node`, respectively.

<a name="xml::add_next"></a>`xml::add_next node info`, <a name="xml::add_prev"></a>`xml::add_prev node info`
:   Add the new node as the next or previous sibling of `node`, respectively.

<!-- -->
The operations above all return a pointer to the new XML node object.

<a name="xml::unlink"></a>`xml::unlink node`
:   Deletes an existing node from the document tree. Returns `()`.

<!-- -->
**Examples**

Replace the first child of the root node in the sample document:

    > xml::node_info (r!0);
    xml::cdata "<greeting>Hello, world!</greeting>"
    > xml::replace (r!0) (xml::text "bla bla");
    #<pointer 0xd40d80>
    > xml::node_info (r!0);
    xml::text "bla bla"

Delete that node:

    > xml::unlink (r!0);
    ()
    > xml::node_info (r!0);
    xml::comment "This is a sample document for testing the xml interface."

### Transformations

The following operations provide basic XSLT support. As already mentioned,
stylesheets are represented as pointers to the xsltStylesheet structure
provided by libxslt. Note that, in difference to XML document pointers, this
is an opaque type, i.e., there is no direct means to inspect and manipulate
parsed stylesheets in memory using the operations of this module. However, a
stylesheet is just a special kind of XML document and thus can be manipulated
after reading the stylesheet as an ordinary XML document. The
[`load_stylesheet`](#xslt::load_stylesheet) function then allows you to
convert the document pointer to an XSLT Stylesheet object.

Applying a stylesheet to an XML document generally involves the following
steps:

1.  Load and parse the stylesheet using
    [`load_stylesheet`](#xslt::load_stylesheet). The parameter to
    [`load_stylesheet`](#xslt::load_stylesheet) can be either the name of a
    stylesheet file or a corresponding document pointer. The function returns
    a pointer to the stylesheet object which is used in the subsequent
    processing.
2.  Invoke [`apply_stylesheet`](#xslt::apply_stylesheet) on the stylesheet and
    the target document. This returns a new document containing the
    transformed XML document.
3.  Run [`save_result_file`](#xslt::save_result_file) or
    [`save_result_string`](#xslt::save_result_string) on the result and the
    stylesheet to save the transformed document in a file or a string.

Here is a brief summary of the XSLT operations. Please check the
[XSLT](http://www.w3.org/TR/xslt) documentation for details of the
transformation process.

<a name="xslt::load_stylesheet"></a>`xslt::load_stylesheet x`
:   Load a stylesheet. `x` can be either an XML document pointer, or a string
    denoting the desired `.xsl` file.

<a name="xslt::apply_stylesheet"></a>`xslt::apply_stylesheet style doc params`
:   Apply the stylesheet `style` to the given document `doc` with the given
    parameters `params`. The third argument is a (possibly empty) list of
    `key=>value` string pairs which allows you to pass additional parameters
    to the stylesheet.

<a name="xslt::save_result_file"></a>`xslt::save_result_file name doc style compression`, <a name="xslt::save_result_string"></a>`xslt::save_result_string doc style`
:   Save the transformation result `doc` obtained with the stylesheet `style`
    to a file or a string. This works pretty much like
    [`save_file`](#xslt::save_file) or [`save_string`](#xslt::save_string),
    but also keeps track of some output-related information contained in the
    stylesheet.

<!-- -->
**Example**

Load the recipes.xml document and the recipes.xsl stylesheet distributed with
the sources:

    > let recipes = xml::load_file "recipes.xml" xml::DTDVALID;
    > let style = xslt::load_stylesheet "recipes.xsl";

Apply the stylesheet to the document and save the result in a html file:

    > let res = xslt::apply_stylesheet style recipes [];
    > xslt::save_result_file "recipes.html" res style 0;
    ()

That's all. You can now have a look at recipes.html in your favourite web
browser.
