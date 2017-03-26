<a name="doc-pure-stldict"></a>

pure-stldict
============

<a name="module-stldict"></a>

<a name="module-hashdict"></a>

<a name="module-orddict"></a>

Version 0.8, March 06, 2017

Albert Graef &lt;<aggraef@gmail.com>&gt;

This package provides a light-weight, no frills interface to the C++
dictionary containers `map` and `unordered_map`. The
[stldict](#module-stldict) module makes these data structures available in
Pure land and equips them with a (more or less) idiomatic Pure container
interface.

The C++ containers are part of the standard C++ library, see the [C++ standard
library documentation](http://en.cppreference.com/w/cpp) for details. They
were originally based on the [Standard Template
Library](http://www.sgi.com/tech/stl/), so they are also sometimes referred to
as "STL containers"; hence the name of this package.

Copying
-------

Copyright (c) 2011 by Albert Graef.

pure-stldict is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

pure-stldict is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
details.

You should have received a copy of the GNU Lesser General Public License along
with this program. If not, see &lt;<http://www.gnu.org/licenses/>&gt;.

Installation
------------

Get the latest source from
<https://bitbucket.org/purelang/pure-lang/downloads/pure-stldict-0.8.tar.gz>.

Run `make` to compile the modules and `make install` (as root) to install them
in the Pure library directory. This requires GNU make, and of course you need
to have Pure (and a C++ library which includes the STL) installed.

`make` tries to guess your Pure installation directory and platform-specific
setup. If it gets this wrong, you can set some variables manually, please
check the Makefile for details.

------------------------------------------------------------------------------

> **Note:** This module requires Pure 0.50 or later and a recent version of
> the C++ library (GNU libstdc++ v3 has been tested). All proper C++11
> libraries should work out of the box, while (recent) C++0x implementations
> may require some fiddling with the sources and/or the compilation options.
> Pre C++0x library versions surely require considerably more work, especially
> in the hashdict module.

------------------------------------------------------------------------------

Usage
-----

After installation, you can use the operations of this package by placing the
following import declaration in your Pure programs:

    using stldict;

This imports the whole shebang. If you only need either the hashed or the
ordered dictionaries, you can also import the corresponding modules
separately, i.e.:

    using hashdict;

or:

    using orddict;

Types
-----

In Pure land, the C++ `map` and `unordered_map` containers and their
`multimap` variants are made available as a collection of four data
structures:

<a name="hashdict/type"></a>*type* `hashdict`, <a name="hashmdict/type"></a>`hashmdict`
:   Hashed (unordered) dictionary data structures. These work with arbitrary
    key (and value) types, like the hashed dictionary and set data structures
    in the standard library, and can be found in the `hashdict.pure` module.

<a name="orddict/type"></a>*type* `orddict`, <a name="ordmdict/type"></a>`ordmdict`
:   Ordered dictionary data structures. These require the keys to be ordered
    by the standard `<` predicate, like the ordered dictionary and set data
    structures in the standard library, and can be found in the `orddict.pure`
    module.

<!-- -->
Note that [`hashdict`](#hashdict/type) and [`hashmdict`](#hashmdict/type)
differ in that the former has exactly one key-value association for each key
in the dictionary, while the latter is a "multidict" which allows multiple
values to be associated with a key. The same applies to the
[`orddict`](#orddict/type) and [`ordmdict`](#ordmdict/type) types.

In addition, there are various supertypes which correspond to different unions
of the hashed and ordered dictionary types. These are:

<a name="hashxdict/type"></a>*type* `hashxdict`, <a name="ordxdict/type"></a>`ordxdict`
:   Denotes any kind of hashed or ordered dictionary, respectively.

<a name="stldict/type"></a>*type* `stldict`, <a name="stlmdict/type"></a>`stlmdict`
:   Denotes any kind of singled-valued or multi-valued dictionary,
    respectively.

<a name="stlxdict/type"></a>*type* `stlxdict`
:   Denotes any kind of dictionary.

<!-- -->
For instance, you can use [`hashxdict`](#hashxdict/type) to match both
[`hashdict`](#hashdict/type) and [`hashmdict`](#hashmdict/type) values.
Likewise, [`stlmdict`](#stlmdict/type) matches both
[`hashmdict`](#hashmdict/type) and [`ordmdict`](#ordmdict/type) values. To
match any kind of dictionary, use the [`stlxdict`](#stlxdict/type) type.

These data structures are very thin wrappers around the C++ container types;
in fact, they are just pointers to the C++ containers. Memory management of
these objects is automatic, and customizable
[pretty-printing](#pretty-printing) is provided as well.

All data structures offer most of the usual Pure container interface (as well
as some extensions). In contrast to the standard library dictionaries, they
can be used both as dictionaries (holding key =&gt; value pairs) and sets
(holding only keys, without associated values), even at the same time.

The other important difference to the standard library containers is that the
stldict containers are *mutable* data structures; inserting and deleting
members really modifies the underlying C++ containers. (However, it is
possible to take copies of the containers in situations where it's necessary
to preserve value semantics.)

Operations
----------

All types of dictionaries are simply pointers to the corresponding C++
containers which hold key-value associations where both keys and values may be
arbitrary Pure expressions. The [basic operations](#basic-operations)
described below can be used to create, query and modify these objects.
[Comparisons](#comparisons) of dictionaries are implemented as well, and the
[set-like operations](#set-like-operations) let you combine dictionaries in
different ways. These operations provide an interface similar to the usual
Pure container API.

In addition, the [stldict](#module-stldict) module provides some [list-like
operations](#list-like-operations) on dictionaries, so that the member data
can be processed and aggregated in a convenient fashion (including the ability
to use dictionaries as generators in list and matrix comprehensions), and
there's also an interface to C++ [iterators](#iterators) which enables you to
traverse, inspect and modify the containers in a more C++-like way. Some
[low-level operations](#low-level-operations) are available to access
information about the underlying hash table of a hashed dictionary. Last but
not least, the module also offers some operations to customize the
[pretty-printing](#pretty-printing) of dictionary values.

When working with these data structures, please note the following special
properties of this implementation:

-   All dictionary types are *mutable*. Inserting and deleting members really
    modifies the underlying C++ data structure as a side effect of the
    operation. If you need value semantics, you should probably use one of the
    dictionary or set data structures from the standard Pure library instead.
    Another possibility is to take a copy of a hashdict using the
    [`copy`](#copy/stldict) function if you need to preserve the original
    value.
-   Keys in a hashed dictionary may be stored in an apparently random order
    (not necessarily in the order in which they were inserted), while they are
    guaranteed to be in ascending order (by key) for ordered dictionaries.
    However, note that even in the latter case, the order of different members
    for the same key in a multi-valued dictionary is not specified. This must
    be taken into account when comparing dictionaries, see below. The order of
    members in a dictionary also matters when listing data from a container
    using, e.g., the [`members`](#members/stldict), [`keys`](#keys/stldict)
    and [`vals`](#vals/stldict) operations.
-   Two dictionaries are considered syntactically equal iff they contain the
    same elements in exactly the same order, using syntactic equality on both
    the keys and the associated values. This test can always be done in linear
    time, but is of limited usefulness for most kinds of dictionaries, since
    the exact order of members in the dictionary may vary depending on how the
    dictionary was constructed. Semantic equality operations are provided
    which check (albeit at the cost of increased running time) whether two
    containers contain the same members irrespective of element order, using
    semantic equality on the members. Various subset comparisons are provided
    as well, please check the [Comparisons](#comparisons) section for details.
-   Values in a dictionary can be omitted, so that a dictionary can also be
    used as a set data structure. This obviates the need for a separate set
    data structure at the cost of some (small) increase in memory usage. Also
    note that you can't really have a hash pair `x=>y` as a member of a set,
    since it always denotes a key-value association. As a remedy, you may use
    ordinary pairs `(x,y)` instead.

### Basic Operations

<a name="hashdict"></a>`hashdict xs`, <a name="hashmdict"></a>`hashmdict xs`, <a name="orddict"></a>`orddict xs`, <a name="ordmdict"></a>`ordmdict xs`

:   Create a dictionary of the corresponding type from a list, tuple or vector
    of its members. Members can be specified as hash pairs `x=>y` to denote a
    key-value association. Any other kind of value denotes a singleton key
    without associated value. Note that the ordered dictionaries require that
    the keys be ordered, i.e., the [`<`](purelib.html#%3C) predicate must be
    defined on them.

    The same operations can also be used to construct a dictionary from
    another dictionary of any type. If the given dictionary is already of the
    corresponding type, this is a no-op (if you want to copy the dictionary
    instead, use the [`copy`](#copy/stldict) function below). Otherwise the
    given dictionary is converted to a new dictionary of the desired target
    type.

<a name="mkhashdict"></a>`mkhashdict y xs`, <a name="mkhashmdict"></a>`mkhashmdict y xs`, <a name="mkorddict"></a>`mkorddict y xs`, <a name="mkordmdict"></a>`mkordmdict y xs`
:   Create a dictionary from a list of keys and a constant value. The
    resulting dictionary has the given keys and `y` as the value for each key.

<a name="copy/stldict"></a>`copy m`

:   Create a new dictionary with the same type and content as `m`. This is
    useful if you want to preserve value semantics when using destructive
    update operations such as [`insert`](#insert/stldict) and
    [`delete`](#delete/stldict). In such a case, [`copy`](#copy/stldict) can
    be used to take a copy of the dictionary beforehand, so that the original
    dictionary remains unmodified.

    ------------------------------------------------------------------------------

    > **Note:** This operation needs linear time with respect to the size of
    > the dictionary (i.e., its number of members). If logarithmic update
    > times are needed while still preserving value semantics, you should use
    > the dictionary and set data structures from the standard library
    > instead.

    ------------------------------------------------------------------------------

<a name="hashdictp"></a>`hashdictp m`, <a name="hashmdictp"></a>`hashmdictp m`, <a name="orddictp"></a>`orddictp m`, <a name="ordmdictp"></a>`ordmdictp m`
:   Check whether the argument is a dictionary of the corresponding type.

<a name="hashxdictp"></a>`hashxdictp m`, <a name="ordxdictp"></a>`ordxdictp m`, <a name="stldictp"></a>`stldictp m`, <a name="stlmdictp"></a>`stlmdictp m`

:   

<a name="stlxdictp"></a>`stlxdictp m`
:   Check whether the argument is a dictionary of the corresponding supertype.

<a name="#/stldict"></a>`# m`
:   The size of a dictionary (the number of members it contains).

<a name="!/stldict"></a>`m ! x`
:   Get the value stored under key `x` in the dictionary `m`. This may be `x`
    itself if `x` is a member of `m` but has no associated value. In the case
    of a multidict this actually returns a list of values (which may be empty
    if `m` doesn't contain `x`). Otherwise an
    [`out_of_bounds`](purelib.html#out_of_bounds) exception is thrown if `m`
    doesn't contain `x`.

<a name="null/stldict"></a>`null m`
:   Test whether `m` is empty, i.e., has zero members.

<a name="member/stldict"></a>`member m x`
:   Test whether `m` contains a member with key `x`.

<a name="members/stldict"></a>`members m`, <a name="list/stldict"></a>`list m`
:   Return the list of members of `m`. The member list will be in an
    apparently random order in the hashed dictionary case, while it is
    guaranteed to be in ascending order (by key) for ordered dictionaries. The
    same order is also used for the other inspection operations below.

<a name="stream/stldict"></a>`stream m`
:   Like [`list`](#list/stldict), but the member list is returned as a lazy
    list (cf. [Lazy Evaluation and
    Streams](pure.html#lazy-evaluation-and-streams)) whose members will be
    computed on the fly as the list is being traversed; cf.
    [Iterators](#iterators).

<a name="tuple/stldict"></a>`tuple m`, <a name="vector/stldict"></a>`vector m`
:   Return the members as a tuple or vector.

<a name="keys/stldict"></a>`keys m`
:   Return the list of keys in the dictionary.

<a name="vals/stldict"></a>`vals m`
:   Return the list of corresponding values. In the case of a singleton key
    `x` without associated value, `x` itself is returned instead.

<!-- -->
As already mentioned, the following modification operations are destructive,
i.e., they actually modify the underlying dictionary data structure. If this
is not desired, you'll first have to take a copy of the target dictionary, see
[`copy`](#copy/stldict).

<a name="insert/stldict"></a>`insert m x`, <a name="insert/stldict2"></a>`insert m (x=>y)`, <a name="update/stldict"></a>`update m x y`
:   Insert a singleton key `x` or a key-value pair `x=>y` into `m` and return
    the modified dictionary. This always adds a new member in a multidict,
    otherwise it replaces an existing value if there is one.
    [`update`](#update/stldict) is provided as a fully curried version of
    [`insert`](#insert/stldict2), so `update m x y` behaves exactly like
    `insert m (x=>y)`.

<a name="delete/stldict"></a>`delete m x`, <a name="delete/stldict2"></a>`delete m (x=>y)`
:   Remove the key `x` or the specific key-value pair `x=>y` from `m` (if
    present) and return the modified dictionary. In the multidict case, only
    the first member with the given key `x` or key-value pair `x=>y` is
    removed.

<a name="clear/stldict"></a>`clear m`
:   Remove all members from `m`, making `m` an empty dictionary. Returns `()`.

<!-- -->
### Comparisons

The usual comparison predicates ([`==`](purelib.html#==),
[`~=`](purelib.html#~=), [`<=`](purelib.html#%3C=), [`<`](purelib.html#%3C)
etc.) are defined on all dictionary types, where two dictionaries are
considered "equal" (`m1==m2`) if they both contain the same `key=>value`
pairs, and `m1<=m2` means that `m1` is a sub-dictionary of `m2`, i.e., all
`key=>value` pairs of `m1` are also contained in `m2` (taking into account
multiplicities in the multidict case). Ordered dictionaries compare keys using
equality (assuming two keys `a` and `b` to be equal if neither `a<b` nor `b<a`
holds), while hashed dictionaries check for syntactical equality (using
[`===`](purelib.html#===)). The associated values are compared using the
[`==`](purelib.html#==) predicate if it is defined, falling back to syntactic
equality otherwise.

The module also defines syntactic equality on all dictionary types, so that
two dictionaries of the same type are considered syntactically equal iff they
contain the same (syntactically equal) members in the same order. This is
always guaranteed if two dictionaries are "identical" (the same C++ pointer),
but generally the member order will depend on how the dictionary was
constructed. Thus if you need to check that two dictionaries contain the same
members irrespective of the order in which the members are listed, the
semantic equality operation [`==`](purelib.html#==) should be used instead;
this will also handle the case of mixed operand types.

Note that if you really need to check whether two dictionaries are the same
object rather than just syntactically equal, you'll have to cast them to
generic C pointers before comparing them with [`===`](purelib.html#===). This
can be done with the following little helper function:

    same_dict x y = pointer_cast "void*" x === pointer_cast "void*" y;

### Set-Like Operations

These operations work with mixed operand types, promoting less general types
to more general ones (i.e., ordered to hashed, and single-valued to
multi-valued dictionaries). The result is always a new dictionary, leaving the
operands unmodified.

<a name="+/stldict"></a>`m1 + m2`
:   Sum: `m1+m2` adds the members of `m2` to `m1`.

<a name="-/stldict"></a>`m1 - m2`
:   Difference: `m1-m2` removes the members of `m2` from `m1`.

<a name="*/stldict"></a>`m1 * m2`
:   Intersection: `m1*m2` removes the members *not* in `m2` from `m1`.

<!-- -->
### List-Like Operations

The following operations are all overloaded so that they work like their list
counterparts, treating their dictionary argument as if it was the member list
of the dictionary:

-   [`do`](purelib.html#do), [`map`](purelib.html#map),
    [`catmap`](purelib.html#catmap), [`listmap`](purelib.html#listmap),
    [`rowmap`](purelib.html#rowmap), [`rowcatmap`](purelib.html#rowcatmap),
    [`colmap`](purelib.html#colmap), [`colcatmap`](purelib.html#colcatmap)
-   [`all`](purelib.html#all), [`any`](purelib.html#any),
    [`filter`](purelib.html#filter), [`foldl`](purelib.html#foldl),
    [`foldl1`](purelib.html#foldl1), [`foldr`](purelib.html#foldr),
    [`foldr1`](purelib.html#foldr1), [`scanl`](purelib.html#scanl),
    [`scanl1`](purelib.html#scanl1), [`scanr`](purelib.html#scanr),
    [`scanr1`](purelib.html#scanr1), [`sort`](purelib.html#sort)

Note that this includes the generic comprehension helpers
[`listmap`](purelib.html#listmap), [`catmap`](purelib.html#catmap) et al, so
that dictionaries can be used as generators in list and matrix comprehensions
as usual (see below for some [examples](#examples)).

### Iterators

These operations give direct access to C++ iterators on dictionaries which let
you query the elements and do basic manipulations of the container. The
operations are available in the `stldict` namespace.

The iterator concept is somewhat alien to Pure and there are some pitfalls
(most notably, destructive updates may render iterators invalid), but the
operations described here are still useful in some situations, especially if
you need to speed up sequential accesses to large containers or modify values
stored in a container in a direct way. They are also used internally to
compute lazy member lists of containers ([`stream`](#stream/stldict)
function).

You should only use these directly if you know what you are doing. In
particular, make sure to consult the [C++ standard library
documentation](http://en.cppreference.com/w/cpp) for further details on C++
iterator usage.

The following operations are provided to create an iterator for a given
dictionary.

<a name="stldict::begin"></a>`stldict::begin m`, <a name="stldict::end"></a>`stldict::end m`
:   Return iterators pointing to the beginning and the end of the container.
    (Note that [`stldict::end`](#stldict::end) *must* always be specified in
    qualified form since `end` is a keyword in the Pure language.)

<a name="stldict::find"></a>`stldict::find m x`
:   Locates a key or specific key=&gt;value pair `x` in the container and
    returns an iterator pointing to the corresponding member (or
    `stldict::end m` if `m` doesn't contain `x`).

<!-- -->
Note that these operations return a new iterator object for each invocation.
Also, the created iterator object keeps track of the container it belongs to,
so that the container isn't garbage-collected while the iterator is still
being used. However, removing a member from the container (using either
[`delete`](#delete/stldict) or [`stldict::erase`](#stldict::erase))
invalidates all iterators pointing to that member; the result of trying to
access such an invalidated iterator is undefined (most likely your program
will crash).

Similar caveats also apply to the [`stream`](#stream/stldict) function which,
as already mentioned, uses iterators internally to implement lazy list
traversal of the members of a dictionary. Thus, if you delete a member of a
dictionary while traversing it using [`stream`](#stream/stldict), you better
make sure that this member is not the next stream element remaining to be
visited; otherwise bad things will happen.

The following operations on iterators let you query and modify the contents of
the underlying container:

<a name="stldict::dict"></a>`stldict::dict i`
:   Return the dictionary to which `i` belongs.

<a name="stldict::endp"></a>`stldict::endp i`
:   Check whether the iterator `i` points to the end of the container (i.e.,
    past the last element).

<a name="stldict::next"></a>`stldict::next i`
:   Advance the iterator to the next element. Note that for convenience, in
    contrast to the corresponding C++ operation this operation is
    non-destructive. Thus it actually creates a *new* iterator object, leaving
    the original iterator `i` unmodified. The operation fails if `i` is
    already at the end of the container.

<a name="stldict::get"></a>`stldict::get i`
:   Retrieve the key=&gt;val pair stored in the member pointed to by `i` (or
    just the key if there is no associated value). The operation fails if `i`
    is at the end of the container.

<a name="stldict::put"></a>`stldict::put i y`
:   Change the value associated with the member pointed to by `i` to `y`, and
    return the new value `y`. The operation fails if `i` is at the end of the
    container. Note that [`stldict::put`](#stldict::put) only allows you to
    set the associated value, *not* the key of the member.

<a name="stldict::erase"></a>`stldict::erase i`
:   Remove the member pointed to by `i` (this invalidates `i` and all other
    iterators pointing to this member). The operation fails if `i` is at the
    end of the container.

<a name="==/stldict_iterator"></a>`i == j`, <a name="~=/stldict_iterator"></a>`i ~= j`
:   Semantic equality of iterators. Two iterators are considered equal
    (`i == j`) if `i` and `j` point to the same element in the same container,
    and unequal (`i ~= j`) if they don't. (In contrast, note that iterators
    are in fact just pointers to a corresponding C++ data structure, and thus
    *syntactical* equality (`i === j`) holds only if two iterators are the
    same object.)

<!-- -->
### Low-Level Operations

The [hashdict](#module-hashdict) module also provides a few specialized
low-level operations dealing with the layouts of buckets and the hash policy
of the [`hashdict`](#hashdict/type) and [`hashmdict`](#hashmdict/type)
containers, such as `bucket_count`, `load_factor`, `rehash` etc. These
operations, which are all kept in their own separate `hashdict` namespace, are
useful to obtain performance-related information and modify the setup of the
underlying hash table. Please check the `hashdict.pure` module and the [C++
standard library documentation](http://en.cppreference.com/w/cpp) for further
details.

### Pretty-Printing

By default, dictionaries are pretty-printed in the format `somedict xs`, where
`somedict` is the actual construction function such as `hashdict`, `orddict`,
etc., and `xs` is the member list of the dictionary. This is usually
convenient, as the printed expression will evaluate to an equal container when
reentered as Pure code. However, it is also possible to define your own custom
pretty-printing with the following function.

<a name="hashdict_symbol"></a>`hashdict_symbol f`, <a name="hashmdict_symbol"></a>`hashmdict_symbol f`, <a name="orddict_symbol"></a>`orddict_symbol f`, <a name="ordmdict_symbol"></a>`ordmdict_symbol f`
:   Makes the pretty-printer use the format `f xs` (where `xs` is the member
    list) for printing the corresponding type of dictionary.

<!-- -->
Note that `f` may also be an operator symbol (nonfix and unary symbols work
best). In the case of an outfix symbol the list brackets around the members
are removed; this makes it possible to render the container in a format
similar to Pure's list syntax. For instance:

    > using stldict;
    > outfix {$ $};
    > orddict_symbol ({$ $});
    ()
    > orddict (1..5);
    {$1,2,3,4,5$}

See `orddict_examp.pure` included in the distribution for a complete example
which also discusses how to make such a custom print representation
reparsable.

Examples
--------

Some basic examples showing [`hashdict`](#hashdict/type) in action:

    > using stldict;
    > let m = hashdict [foo=>99, bar=>bar 4711L, baz=>1..5]; m;
    hashdict [foo=>99,bar=>bar 4711L,baz=>[1,2,3,4,5]]
    > m!bar;
    bar 4711L
    > keys m;
    [foo,bar,baz]
    > vals m;
    [99,bar 4711L,[1,2,3,4,5]]
    > list m;
    [foo=>99,bar=>bar 4711L,baz=>[1,2,3,4,5]]
    > member m foo, member m bar;
    1,1

Hashed multidicts ([`hashmdict`](#hashmdict/type)):

    > let m = hashmdict [foo=>99,baz=>1..5,baz=>bar 4711L]; m;
    hashmdict [foo=>99,baz=>[1,2,3,4,5],baz=>bar 4711L]
    > m!baz;
    [[1,2,3,4,5],bar 4711L]
    > m!foo;
    [99]

The following example illustrates how to employ ordered dictionaries
([`orddict`](#orddict/type)) as a set data structure:

    > let m1 = orddict [5,1,3,11,3];
    > let m2 = orddict (3..6);
    > m1;m2;
    orddict [1,3,5,11]
    orddict [3,4,5,6]
    > m1+m2;
    orddict [1,3,4,5,6,11]
    > m1-m2;
    orddict [1,11]
    > m1*m2;
    orddict [3,5]
    > m1*m2 <= m1, m1*m2 <= m2;
    1,1
    > m1 < m1+m2, m2 < m1+m2;
    1,1

Of course, the same works with ordered multidicts
([`ordmdict`](#ordmdict/type)):

    > let m1 = ordmdict [5,1,3,11,3];
    > let m2 = ordmdict (3..6);
    > m1;m2;
    ordmdict [1,3,3,5,11]
    ordmdict [3,4,5,6]
    > m1+m2;
    ordmdict [1,3,3,3,4,5,5,6,11]
    > m1-m2;
    ordmdict [1,3,11]
    > m1*m2;
    ordmdict [3,5]
    > m1*m2 <= m1, m1*m2 <= m2;
    1,1
    > m1 < m1+m2, m2 < m1+m2;
    1,1

In fact, the binary operations (comparisons as well as the set operations `+`,
`-` and `*`) work with any combination of dictionary operands:

    > let m1 = hashdict (1..5);
    > let m2 = ordmdict (3..7);
    > m1+m2;
    hashmdict [1,2,3,3,4,4,5,5,6,7]

Note that the operands are always promoted to the more general operand type,
where hashed beats ordered and multi-valued beats single-valued dictionaries.
If this is not what you want, you can also specify the desired conversions
explicitly:

    > m1+orddict m2;
    hashdict [1,2,3,4,5,6,7]
    > orddict m1+m2;
    ordmdict [1,2,3,3,4,4,5,5,6,7]

Also note that the "set" operations not only work with proper sets, but also
with general dictionaries:

    > hashdict [i=>i+1|i=1..4]+hashdict [i=>i-1|i=3..5];
    hashdict [1=>2,2=>3,3=>2,4=>3,5=>4]

All dictionary containers can be used as generators in list and matrix
comprehensions:

    > let m = hashmdict [foo=>99,baz=>1..5,baz=>bar 4711L];
    > [x y | x=>y = m];
    [foo 99,baz [1,2,3,4,5],baz (bar 4711L)]
    > {{x;y} | x=>y = m};
    {foo,baz,baz;99,[1,2,3,4,5],bar 4711L}

Note that in the current implementation this always computes the full member
list of the dictionary as an intermediate value, which will need considerable
extra memory in the case of large dictionaries. As a remedy, you can also use
the [`stream`](#stream/stldict) function to convert the dictionary to a lazy
list instead. This will often be slower, but in the case of big dictionaries
the tradeoff between memory usage and execution speed might be worth
considering. For instance:

    > let m = hashdict [foo i => i | i = 1..10000];
    > stream m;
    (foo 1512=>1512):#<thunk 0x7fa1718350a8>
    > stats -m
    > #list m;
    10000
    0.01s, 40001 cells
    > #stream m;
    10000
    0.1s, 16 cells
    > #[y | x=>y = m; gcd y 767~=1];
    925
    0.05s, 61853 cells
    > #[y | x=>y = stream m; gcd y 767~=1];
    925
    0.15s, 10979 cells
