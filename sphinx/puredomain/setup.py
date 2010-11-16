# -*- coding: utf-8 -*-

from setuptools import setup, find_packages

long_desc = '''
This package contains the sphinxcontrib-puredomain Sphinx extension.

This extension adds Ruby Domain to Sphinx.
It needs Sphinx 1.0 or newer.

Detail document: http://packages.python.org/sphinxcontrib-puredomain/
'''

requires = ['Sphinx>=1.0']

setup(
    name='sphinxcontrib-puredomain',
    version='0.1',
    url='http://bitbucket.org/birkenfeld/sphinx-contrib',
    download_url='http://pypi.python.org/pypi/sphinxcontrib-puredomain',
    license='BSD',
    author='SHIBUKAWA Yoshiki',
    author_email='yoshiki at shibu.jp',
    description='Sphinx extension sphinxcontrib-puredomain',
    long_description=long_desc,
    zip_safe=False,
    classifiers=[
        'Development Status :: 4 - Beta',
        'Environment :: Console',
        'Environment :: Web Environment',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: BSD License',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Programming Language :: Ruby',
        'Topic :: Documentation',
        'Topic :: Utilities',
    ],
    platforms='any',
    packages=find_packages(),
    include_package_data=True,
    install_requires=requires,
    namespace_packages=['sphinxcontrib'],
)
