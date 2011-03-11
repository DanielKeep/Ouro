
``/ouro/stdio``
===============

This module contains definitions relating to the standard input, output and
error streams.

.. note:: For more general I/O definitions, see ``/ouro/io``.

``stderr``
----------

The standard error output stream.

``stdin``
---------

The standard input stream.

``stdout``
----------

The standard output stream.

``werr~(vs...)``
----------------

Writes out the values *vs* to standard error.

``werrF~(fmt, vs...)``
----------------------

Writes the result of formatting the values *vs* with the format string
*fmt* to standard error.

``werrL~(vs...)``
-----------------

Writes out the values *vs* to standard error, ends the line and flushes the
stream.

``werrFL~(fmt, vs...)``
-----------------------

Writes the result of formatting the values *vs* with the format string
*fmt* to standard error.  It then ends the line and flushes the stream.

``wout~(vs...)``
----------------

Writes out the values *vs* to standard output.

``woutF~(fmt, vs...)``
----------------------

Writes the result of formatting the values *vs* with the format string
*fmt* to standard output.

``woutL~(vs...)``
-----------------

Writes out the values *vs* to standard output, ends the line and flushes the
stream.

``woutFL~(fmt, vs...)``
-----------------------

Writes the result of formatting the values *vs* with the format string
*fmt* to standard output.  It then ends the line and flushes the stream.

