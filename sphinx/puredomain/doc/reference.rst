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
   sets a default, an object directive may always override this setting by
   specifying a qualified object name.)

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

.. rst:role:: pure:var

   Reference a variable.

.. rst:role:: pure:const

   Reference a constant.

.. rst:role:: pure:obj

   Generic reference to any kind of Pure object, including Pure modules.
   Useful as the default role.
