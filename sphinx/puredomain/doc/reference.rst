Reference
=========

.. highlight:: rst

The Pure domain (name **pure**) provides the following directives for module
declarations. Most of the directives work analogously to the Python domain, so
if you know Sphinx's Python markup then you should feel right at home.

.. rst:directive:: .. pure:module:: name

   Marks the beginning of the descriptions for the given Pure module. This
   directive usually does not create any content (unless the ``platform``
   option is specified), but causes an entry in the global module index to be
   created. An entry in the general index will be created as well, unless the
   ``noindex`` option is specified. The usual options are provided
   (``noindex``, ``platform``, ``synopsis`` and ``deprecated``), see
   :rst:dir:`py:module` for details.

.. rst:directive:: .. pure:currentmodule:: name

   Like :rst:dir:`py:currentmodule`, this just switches over to the given
   module, but doesn't create any index entries.

.. rst:directive:: .. pure:namespace:: name

   Selects the current Pure namespace for the following objects. (This only
   sets a default, an object directive or reference may always override this
   setting by specifying a qualified object name.)

The following directives are provided for actually producing contents:

.. rst:directive:: .. pure:function:: [fixity] name [/tag] parameter ...

   Describes a Pure function with the given name. The name may be qualified
   with a namespace prefix, otherwise the current namespace is assumed (if
   any). Also, if the function is actually an operator, you can specify its
   fixity by placing one of the keywords ``prefix``, ``postfix``, ``infix``
   and ``outfix`` before the name, e.g.::

     .. pure:function:: infix + x y -> the sum of x and y

   Operator and operands will then be arranged so that they appear in the
   right order. E.g., the above is rendered as follows:

      .. pure:function:: infix + x y -> the sum of x and y
     	:noindex:

   The parameter list takes the form of a whitespace-delimited list of
   parameter names. Actually, the parameter "names" can be pretty much
   anything, including extra annotations such as type tags. The usual markup
   for embellished function descriptions is supported as well, see
   :rst:dir:`py:function` for details. Also, return annotations of the form
   ``= value`` are supported in addition to the standard ``-> value``
   format. For instance::

     .. pure:function:: foldl f a xs = f ... (f a (xs!0)) ... (xs!n)

        Accumulate the binary function `f` over all members of `xs`,
        starting from the initial value `a` and working from the front
        of the list towards its end.
   
	:param f:  accumulating function
	:type  f:  closure
   	:param a:  initial value
   	:type  a:  any type
   	:param xs: values to be accumulated
   	:type  xs: list
   	:return:   accumulated value
   	:rtype:    any (usually the same as `a`)

   This is rendered as follows:

     .. pure:function:: foldl f a xs = f ... (f a (xs!0)) ... (xs!n)
     	:noindex:

        Accumulate the binary function `f` over all members of `xs`,
        starting from the initial value `a` and working from the front
        of the list towards its end.
   
	:param f:  accumulating function
	:type  f:  closure
   	:param a:  initial value
   	:type  a:  any type
   	:param xs: values to be accumulated
   	:type  xs: list
   	:return:   accumulated value
   	:rtype:    any (usually the same as `a`)

   Finally, to account for the fact that functions are overloaded (or rather
   extended) all the time in Pure, and function descriptions may therefore be
   scattered out over different documents, separate entries for the same
   function can be specified by supplying a unique tag for the different
   instances. The tag may consist of arbitrary alphanumeric characters and
   will be invisible in the output. The tag can then be specified when
   referring to a specific instance of the described function. For instance::

     .. pure:function:: foo x y z

     	Just the plain :pure:func:`foo`.

     .. pure:function:: foo /matic x y z (matrix version)

     	:pure:func:`foo/matic` works like :pure:func:`foo` above, but
	uses matrices instead of lists.

   This is rendered as follows:

     .. pure:function:: foo x y z

     	Just the plain :pure:func:`foo`.

     .. pure:function:: foo /matic x y z (matrix version)

     	:pure:func:`foo/matic` works like :pure:func:`foo` above, but
	uses matrices instead of lists.

.. rst:directive:: .. pure:macro:: name parameter ...

   Same as above, but describes a Pure macro. For instance:

     .. pure:macro:: infix . (f g) x = f (g x)
     	:noindex:

	Macro to optimize away function compositions.

.. rst:directive:: .. pure:extern:: name parameter ...

   Same as above, but describes an external function. Example:

     .. pure:extern:: puts s::string
     	:noindex:

     	Output a string on the terminal.

.. rst:directive:: .. pure:constructor:: name parameter ...

   Same as above, but this can be used to denote a constructor symbol (which
   is just a function without equations in Pure). For instance:

     .. pure:constructor:: infix : x xs

     	The list constructor.

.. rst:directive:: .. pure:variable:: name ...

   Describes a variable. Additional annotations like an initial value may
   follow the variable name. Example:

     .. pure:variable:: stdin
     			stdout
			stderr
     	:noindex:

     	The standard I/O streams. These are built-in variables.

.. rst:directive:: .. pure:constant:: name ...

   Same as above, but describes a constant. Example:

     .. pure:constant:: c = 299792
     	:noindex:

     	A constant. The speed of light, what else?

Note that the directives :rst:dir:`pure:function`, :rst:dir:`pure:macro`,
:rst:dir:`pure:extern` and :rst:dir:`pure:constructor` work alike and can, to
some extent, be used interchangeably. They just denote different semantic
categories of function symbols in Pure. In some cases, a function symbol may
be in multiple categories (such as an extern function which also has a
definition as an ordinary Pure function, or a function which is also
implemented as a macro). Thus some authors may prefer to just mark up all
function symbols using :rst:dir:`pure:function` and point out the categories
that the symbol belongs to in the description instead.

Cross-referencing Pure objects
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following roles refer to Pure objects and are hyperlinked if a matching
identifier is found. They mostly work like their counterparts from the Python
domain.

.. rst:role:: pure:mod

   Reference a Pure module.

.. rst:role:: pure:func

   Reference a Pure function; the name may be qualified to refer to a symbol
   outside the current namespace, as set with the :rst:dir:`pure:namespace`
   directive. Moreover, a tag may be used to differentiate between different
   descriptions of an overloaded function (see above).

.. rst:role:: pure:macro

   Reference a macro.

.. rst:role:: pure:ext

   Reference an external function.

.. rst:role:: pure:cons

   Reference a constructor.

.. rst:role:: pure:var

   Reference a variable.

.. rst:role:: pure:const

   Reference a constant.

.. rst:role:: pure:obj

   Generic reference to any kind of Pure object, including Pure modules.
   Useful as the default role.
