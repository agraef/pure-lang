Reference
=========

.. highlight:: rst

The Pure domain (name **pure**) provides the following directives for module
declarations. Almost all directives are decidely similar to Python's.

.. rst:directive:: .. pure:module:: name

   This works like :rst:dir:`py:module`, but actually denotes a Pure namespace
   in the Pure domain. Subsequent objects will be taken to belong to the given
   namespace, and the directive normally also causes an entry in the global
   module index to be created. The usual options are provided (``noindex``,
   ``platform``, ``synopsis`` and ``deprecated``).

.. rst:directive:: .. pure:currentmodule:: name

   Like :rst:dir:`py:currentmodule`, this just switches over to the given
   namespace, but doesn't create any index entries.

The following directives are provided for actually producing contents:

.. rst:directive:: .. pure:function:: name parameter ...

   Describes a Pure function. The parameter list takes the following form:

   TODO

   .. seealso:: :rst:dir:`py:function`

.. rst:directive:: .. pure:macro:: name parameter ...

   Same as above, but describes a Pure macro.

.. rst:directive:: .. pure:extern:: name parameter ...

   Same as above, but describes an external function.

.. rst:directive:: .. pure:variable:: name

   Describes a variable. Additional annotations like an initial value may
   follow the variable name.

.. rst:directive:: .. pure:constant:: name

   Same as above, but describes a constant.
   

Cross-referencing Pure objects
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following roles refer to Pure objects and are hyperlinked if a matching
identifier is found. They mostly work like their counterparts from the Python
domain.

.. rst:role:: pure:mod

   Reference a Pure namespace.

.. rst:role:: pure:func

   Reference a Pure function; the name may be qualified to refer to a symbol
   outside the current namespace, as set with :rst:dir:`pure:module` or
   :rst:dir:`pure:currentmodule`. Moreover, a tag may be used to differentiate
   between different descriptions of an overloaded function (see above).

.. rst:role:: pure:macro

   Reference a macro.

.. rst:role:: pure:ext

   Reference an external function.

.. rst:role:: pure:var

   Reference a variable.

.. rst:role:: pure:const

   Reference a constant.

.. rst:role:: pure:obj

   Generic reference to any kind of Pure object, including namespaces. Useful
   as the default role.
