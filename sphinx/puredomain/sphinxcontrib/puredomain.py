# -*- coding: utf-8 -*-
"""
    sphinxcontrib.puredomain
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Pure domain. Based on the Erlang domain by SHIBUKAWA Yoshiki.

    :copyright: Copyright 2007-2010 by SHIBUKAWA Yoshiki, 2010 by Albert Graef
    :license: BSD, see LICENSE for details.
"""

import re
import string

from docutils import nodes
from docutils.parsers.rst import directives

from sphinx import addnodes
from sphinx.roles import XRefRole
from sphinx.locale import l_, _
from sphinx.directives import ObjectDescription
from sphinx.domains import Domain, ObjType, Index
from sphinx.util.compat import Directive
from sphinx.util.nodes import make_refnode
from sphinx.util.docfields import Field, TypedField

# REs for Pure signatures
pure_func_sig_re = re.compile(
    r'''^ ((?:in|pre|post|out)fix)? \s*  # fixity
          ([\w:]*::)?            # module/namespace prefix
          (\S+)  \s*             # thing name
          (?: [/](\s*\w+))? \s*  # optional: extra name tag
          (?: ((?:\S|\s[^-=]|\s-[^>])*) # optional: arguments/extra annotations
           (?:\s* (->|=) \s* (.*))?     #           return annotation
          )? $                   # and nothing more
          ''', re.VERBOSE)

pure_data_sig_re = re.compile(
    r'''^ ([\w:]*::)?            # module/namespace prefix
          (\S+) \s*              # thing name
          (.+)? $                # optional: extra annotations
          ''', re.VERBOSE)

pure_paramlist_re = re.compile(r'(\s+)')  # split at whitespace

pure_tagged_name_re = re.compile(r'^(\S+)[/](\w+)$')

def strip_qual(name):
    if name is None:
        return None
    if name[0:2] == "::":
        return name[2:]
    return name

def split_name(name):
    m = pure_tagged_name_re.match(name)
    if m is None:
        return name, ''
    name, tag = m.groups()
    return name, tag

# Formatting of arguments in a signature. addnodes.desc_type and
# addnodes.desc_name seem to work reasonably well for our purposes.
def desc_args(s, t):
    return addnodes.desc_type(s, t)

class PureObject(ObjectDescription):
    """
    Description of a Pure language object.
    """

    doc_field_types = [
        TypedField('parameter', label=l_('Parameters'),
                   names=('param', 'parameter', 'arg', 'argument'),
                   typerolename='obj', typenames=('type',)),
        Field('returnvalue', label=l_('Returns'), has_arg=False,
              names=('returns', 'return')),
        Field('returntype', label=l_('Return type'), has_arg=False,
              names=('rtype',)),
    ]

    def add_signature_prefix(self, signode):
        if self.objtype != 'function':
            descr = "%s " % self.objtype
            signode += addnodes.desc_annotation(descr, descr)

    def needs_arglist(self):
        # We never want this.
        return False

    def handle_signature(self, sig, signode):
        if self.objtype == 'variable' or self.objtype == 'constant':
            return self._handle_data_signature(sig, signode)
        return self._handle_function_signature(sig, signode)

    def _resolve_namespace(self, signode, namesp, name):
        # determine namespace, as well as full name
        if not namesp:
            namesp = self.options.get( # XXXFIXME
                'module', self.env.temp_data.get('pure:namespace'))
            namesp = namesp and namesp+'::' or ''
        namesp = strip_qual(namesp)
        if namesp:
            fullname = namesp + name
            signode['namespace'] = namesp[:-2]
        else:
            fullname = name
            signode['namespace'] = ''
        signode['fullname'] = fullname
        return fullname

    def _add_desc_name(self, signode, name):
        if signode['namespace']:
            name_prefix = signode['namespace'] + '::'
            signode += addnodes.desc_addname(name_prefix, name_prefix)
        signode += addnodes.desc_name(name, name)

    def _handle_data_signature(self, sig, signode):
        m = pure_data_sig_re.match(sig)
        if m is None:
            raise ValueError
        namesp, name, ann = m.groups()
        fullname = self._resolve_namespace(signode, namesp, name)
        self.add_signature_prefix(signode)
        self._add_desc_name(signode, name)
        if ann:
            ann = " "+ann
            signode += desc_args(ann, ann)
        return fullname

    def _handle_function_signature(self, sig, signode):
        m = pure_func_sig_re.match(sig)
        if m is None:
            raise ValueError
        fixity, namesp, name, tag, arglist, retsym, retann = m.groups()

        fullname = self._resolve_namespace(signode, namesp, name)
        if tag:
            fullname += "/%s" % tag
        self.add_signature_prefix(signode)

        if fixity:
            signode['fixity'] = fixity

        # Sphinx' default format for the parameter list isn't very useful with
        # Pure's curried function applications, so we use freeform text for
        # the arguments instead. We also have to take care of different
        # fixities here.
        if fixity=='postfix':
            if arglist:
                arglist += " "
                signode += desc_args(arglist, arglist)
            self._add_desc_name(signode, name)
        elif fixity=='infix':
            arglist = pure_paramlist_re.split(arglist)
            left = ''.join(arglist[:1])
            right = ''.join(arglist[2:])
            if left:
                left += " "
                signode += desc_args(left, left)
            self._add_desc_name(signode, name)
            if right:
                right = " "+right
                signode += desc_args(right, right)
        elif fixity=='outfix' and arglist:
            arglist = pure_paramlist_re.split(arglist)
            name2 = arglist[0]
            signode['other'] = name2
            arglist = ''.join(arglist[2:])
            self._add_desc_name(signode, name)
            if arglist:
                arglist = " "+arglist+" "
                signode += desc_args(arglist, arglist)
            self._add_desc_name(signode, name2)
        else:
            self._add_desc_name(signode, name)
            if arglist:
                arglist = " "+arglist
                signode += desc_args(arglist, arglist)

        if retann:
            if retsym=="->":
                signode += addnodes.desc_returns(retann, retann)
            else:
                retann = " %s %s" % (retsym, retann)
                signode += desc_args(retann, retann)
        return fullname

    def get_index_text(self, name, fixity, other):
        fname, tag = split_name(name)
        if fixity:
            fixity += " "
        if other:
            fname += " "+other
        if self.objtype == 'function':
            return _('%s (%sfunction)') % (fname,fixity)
        elif self.objtype == 'extern':
            return _('%s (extern %sfunction)') % (fname,fixity)
        elif self.objtype == 'macro':
            return _('%s (%smacro)') % (fname,fixity)
        elif self.objtype == 'constructor':
            return _('%s (%sconstructor)') % (fname,fixity)
        elif self.objtype == 'type':
            return _('%s (type)') % fname
        elif self.objtype == 'variable':
            return _('%s (variable)') % name
        elif self.objtype == 'constant':
            return _('%s (constant)') % name
        else:
            return ''

    def add_target_and_index(self, name, sig, signode):
        if name not in self.state.document.ids:
            signode['names'].append(name)
            signode['ids'].append(name)
            signode['first'] = (not self.names)
            self.state.document.note_explicit_target(signode)
            objects = self.env.domaindata['pure']['objects']
            fname, tag = split_name(name)
            if fname in objects and tag in objects[fname]:
                self.env.warn(
                    self.env.docname,
                    'duplicate Pure object description of %s, ' % name +
                    'other instance in ' +
                    self.env.doc2path(objects[fname][tag][0]),
                    self.lineno)
            tags = objects.setdefault(fname, {})
            tags[tag] = (self.env.docname, self.objtype, name)

        fixity = other = ''
        if 'fixity' in signode:
            fixity = signode['fixity']
        if 'other' in signode:
            other = signode['other']
        indextext = self.get_index_text(name, fixity, other)
        if indextext:
            self.indexnode['entries'].append(('single', indextext, name, name))


class PureModule(Directive):
    """
    Directive to mark description of a new module.
    """

    has_content = False
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = False
    option_spec = {
        'platform': lambda x: x,
        'synopsis': lambda x: x,
        'noindex': directives.flag,
        'deprecated': directives.flag,
    }

    def run(self):
        env = self.state.document.settings.env
        modname = self.arguments[0].strip()
        noindex = 'noindex' in self.options
        env.temp_data['pure:module'] = modname
        env.domaindata['pure']['modules'][modname] = \
            (env.docname, self.options.get('synopsis', ''),
             self.options.get('platform', ''), 'deprecated' in self.options)
        targetnode = nodes.target('', '', ids=['module-' + modname], ismod=True)
        self.state.document.note_explicit_target(targetnode)
        ret = [targetnode]
        # XXX this behavior of the module directive is a mess...
        if 'platform' in self.options:
            platform = self.options['platform']
            node = nodes.paragraph()
            node += nodes.emphasis('', _('Platforms: '))
            node += nodes.Text(platform, platform)
            ret.append(node)
        # the synopsis isn't printed; in fact, it is only used in the
        # modindex currently
        if not noindex:
            indextext = _('%s (module)') % modname
            inode = addnodes.index(entries=[('single', indextext,
                                             'module-' + modname, modname)])
            ret.append(inode)
        return ret


class PureCurrentModule(Directive):
    """
    This directive is just to tell Sphinx that we're documenting
    stuff in module foo, but links to module foo won't lead here.
    """

    has_content = False
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = False
    option_spec = {}

    def run(self):
        env = self.state.document.settings.env
        modname = self.arguments[0].strip()
        if modname == 'None':
            env.temp_data['pure:module'] = None
        else:
            env.temp_data['pure:module'] = modname
        return []


class PureNamespace(Directive):
    """
    Sets the current Pure namespace.
    """

    has_content = False
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = False
    option_spec = {}

    def run(self):
        env = self.state.document.settings.env
        nsname = self.arguments[0].strip()
        # Remove a leading '\' which might be used to escape a '::' argument.
        nsname = nsname.lstrip('\\')
        if nsname == 'None':
            env.temp_data['pure:namespace'] = None
        else:
            nsname = strip_qual(nsname)
            env.temp_data['pure:namespace'] = nsname
        return []


class PureXRefRole(XRefRole):
    def process_link(self, env, refnode, has_explicit_title, title, target):
        refnode['pure:namespace'] = env.temp_data.get('pure:namespace')
        if not has_explicit_title:
            # remove the tag from the title
            title,_ = split_name(title)
            # If the first character is a tilde followed by a namespace
            # qualifier, suppress the display of the qualifier. NOTE: This is
            # a bit different from the processing done in the Python, C++ and
            # Erlang domains, as the initial '~' is only recognized and
            # processed if it is actually followed by a qualifier. This
            # additional constraint is necessary because '~' is a legal symbol
            # constituent in Pure. (You'll still be out of luck if the
            # qualifier itself happens to begin with a tilde, but this
            # pathological case should hopefully never arise in practice, and
            # if it does then you can easily work around it by using an
            # explicit title.)
            if title[0:1] == '~':
                dcolon = title.rfind('::')
                if dcolon != -1:
                    title = title[dcolon+2:]
                    target = target[1:]
        return title, target


class PureModuleIndex(Index):
    """
    Index subclass to provide the Pure module index.
    """

    name = 'modindex'
    localname = l_('Module Index')
    shortname = l_('modules')

    def generate(self, docnames=None):
        content = {}
        # list of prefixes to ignore
        ignores = self.domain.env.config['modindex_common_prefix']
        ignores = sorted(ignores, key=len, reverse=True)
        # list of all modules, sorted by module name
        modules = sorted(self.domain.data['modules'].items(),
                         key=lambda x: x[0].lower())
        # sort out collapsable modules
        prev_modname = ''
        num_toplevels = 0
        for modname, (docname, synopsis, platforms, deprecated) in modules:
            if docnames and docname not in docnames:
                continue

            for ignore in ignores:
                if modname.startswith(ignore):
                    modname = modname[len(ignore):]
                    stripped = ignore
                    break
            else:
                stripped = ''

            # we stripped the whole module name?
            if not modname:
                modname, stripped = stripped, ''

            entries = content.setdefault(modname[0].lower(), [])

            package = modname.split('::')[0]
            if package != modname:
                # it's a submodule
                if prev_modname == package:
                    # first submodule - make parent a group head
                    entries[-1][1] = 1
                elif not prev_modname.startswith(package):
                    # submodule without parent in list, add dummy entry
                    entries.append([stripped + package, 1, '', '', '', '', ''])
                subtype = 2
            else:
                num_toplevels += 1
                subtype = 0

            qualifier = deprecated and _('Deprecated') or ''
            entries.append([stripped + modname, subtype, docname,
                            'module-' + stripped + modname, platforms,
                            qualifier, synopsis])
            prev_modname = modname

        # apply heuristics when to collapse modindex at page load:
        # only collapse if number of toplevel modules is larger than
        # number of submodules
        collapse = len(modules) - num_toplevels < num_toplevels

        # sort by first letter
        content = sorted(content.items())

        return content, collapse


class PureDomain(Domain):
    """Pure language domain."""
    name = 'pure'
    label = 'Pure'
    object_types = {
        'function':    ObjType(l_('function'),    'func', 'obj'),
        'extern':      ObjType(l_('extern'),      'ext', 'obj'),
        'macro':       ObjType(l_('macro'),       'macro', 'obj'),
        'constructor': ObjType(l_('constructor'), 'cons', 'obj'),
        'type':        ObjType(l_('type'),        'type', 'obj'),
        'var':         ObjType(l_('variable'),    'var', 'obj'),
        'const':       ObjType(l_('constant'),    'const', 'obj'),
        'module':      ObjType(l_('module'),      'mod', 'obj'),
    }

    directives = {
        'function':      PureObject,
        'extern':        PureObject,
        'macro':         PureObject,
        'constructor':   PureObject,
        'type':          PureObject,
        'variable':      PureObject,
        'constant':      PureObject,
        'namespace':     PureNamespace,
        'module':        PureModule,
        'currentmodule': PureCurrentModule,
    }
    roles = {
        'func' :  PureXRefRole(),
        'ext' :   PureXRefRole(),
        'macro':  PureXRefRole(),
        'cons':   PureXRefRole(),
        'type':   PureXRefRole(),
        'var':    PureXRefRole(),
        'const':  PureXRefRole(),
        'mod':    PureXRefRole(),
        'obj':    PureXRefRole(),
    }
    initial_data = {
        'objects': {},    # fullname -> tag -> docname, objtype, targetname
        'modules': {},    # modname -> docname, synopsis, platform, deprecated
    }
    indices = [
        PureModuleIndex,
    ]

    def clear_doc(self, docname):
        for fullname, objs in list(self.data['objects'].items()):
            for tag, (fn, _, _) in list(objs.items()):
                if fn == docname:
                    del self.data['objects'][fullname][tag]
            if not self.data['objects'][fullname]:
                del self.data['objects'][fullname]
        for modname, (fn, _, _, _) in list(self.data['modules'].items()):
            if fn == docname:
                del self.data['modules'][modname]

    def find_obj(self, env, namesp, name, objtype, searchorder=0):
        """
        Find a Pure object for "name", perhaps using the given namespace.
        """
        if not name:
            return None, None
        objects = self.data['objects']
        if namesp and "::" not in name:
            name = "%s::%s" % (namesp, name)
        # Remove leading '::'. This is done so that a toplevel identifier
        # 'foo' can be escaped as '::foo' if a namespace directive is
        # currently active.
        name = strip_qual(name)
        fname, tag = split_name(name)
        if fname in objects:
            tags = objects[fname]
            if tag in tags:
                docname, objtype, targetname = tags[tag]
                return targetname, docname
        return None, None

    def resolve_xref(self, env, fromdocname, builder,
                     typ, target, node, contnode):
        if (typ == 'mod' or typ == 'obj') and target in self.data['modules']:
            docname, synopsis, platform, deprecated = \
                self.data['modules'].get(target, ('','','', ''))
            if not docname:
                return None
            else:
                title = '%s%s%s' % ((platform and '(%s) ' % platform),
                                    synopsis,
                                    (deprecated and ' (deprecated)' or ''))
                return make_refnode(builder, fromdocname, docname,
                                    'module-' + target, contnode, title)
        else:
            namesp = node.get('pure:namespace')
            searchorder = node.hasattr('refspecific') and 1 or 0
            name, obj = self.find_obj(env, namesp, target, typ, searchorder)
            if not obj:
                return None
            else:
                fname, tag = split_name(name)
                return make_refnode(builder, fromdocname, obj, name,
                                    contnode, fname)

    def get_objects(self):
        for refname, tags in self.data['objects'].items():
            for tag, (docname, type, targetname) in tags.items():
                yield (refname, refname, type, docname, refname, 1)


def setup(app):
    app.add_domain(PureDomain)
