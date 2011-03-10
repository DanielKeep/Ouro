
``/ouro/io``
============

This module contains various functions and values relating to input and
output.

.. note:: If all you want to do is access the standard input, output and error
          streams, you should import ``/ouro/stdio`` instead.

The primary I/O primitive exposed to programmers in Ouro is the *Stream*.
A *Stream* represents either a transformation (such as (de-)compression) or a
low-level primitive (such as a file or network socket) known as a *Conduit*.
You can open a stream to a low-level conduit using the ``open*`` functions.

Some streams have a *Cursor*; this represents the position at which reads and
writes will occur within the underlying conduit.  If supported, the current
cursor position can be obtained with ``cursorForStream!(stream!)``;
the cursor can be repositioned using ``seekStream!(stream!, anchor, offset)``.
Support for a cursor can be determined with ``hasCursor?(stream)``.

Reading from and writing to a stream can result in ``'eof`` or "end of flow";
this indicates that there is no more content to read or no more space to
write.

.. note::   Actually, this is a lie.  It really means "end of file".  It's just
            that "eof" has been used for so long that the abbreviation is
            traditional.

``closeStream!(stream!)``
-------------------------

Closes an open stream.

``copyIntoStream!(stream_dst!, stream_src!)``
---------------------------------------------

Copies the contents of |stream_src| into |stream_dst| as efficiently as
possible.

.. |stream_dst| replace:: *stream!*\ :sub:`dst`
.. |stream_src| replace:: *stream!*\ :sub:`src`

``flushStream!(stream!)``
-------------------------

Forces all content buffered by *stream!* to be flushed.

``hasCursor?(stream)``
----------------------

Determines whether the given *stream* supports a cursor or not.  Note that a
result of *true* does *not* imply that seek attempts will succeed.

``openFile~(path, flags...)``
-----------------------------

Opens a file located at *path* and returns the established stream.

*flags* can be used to alter the function's behaviour.  By default, *openFile*
defaults to opening the file for read-only access and fails if the file does
not exist.  Note that you may also pass a single list of flags.

The available flags can be divided into sub-sections.

**Shorthand**
    These flags are simply shorthand for multiple flags.  Note that not every
    valid combination is necessarily expressed here.

    - ``'r`` = ``'re``
    - ``'re`` = ``'read``, ``'exists``
    - ``'res`` = ``'read``, ``'exists``, ``'shareRead``
    - ``'we`` = ``'write``, ``'exists``
    - ``'w`` = ``'wc``
    - ``'wa`` = ``'write``, ``'append``
    - ``'wc`` = ``'write``, ``'create``
    - ``'we`` = ``'write``, ``'exists``
    - ``'rw`` = ``'readWrite``, ``'sedate``
    - ``'rwe`` = ``'readWrite``, ``'existing``

**Access**
    These specify what form of access permissions you want for the file.

    - ``'read`` - Read-only access.
    - ``'write`` - Write-only access.
    - ``'readWrite`` - Read-write access.

    .. note::   Currently, ``'read`` and ``'write`` cannot be combined to
                mean ``'readWrite``.  This may be possible in the future.

**Open Behaviour**
    These tell the function how to handle files which do/don't exist when
    attempting to open them.

    - ``'exists`` - The file *must* exist.
    - ``'create`` - If the file does not exist, create it.  If it does exist,
                    truncate it to zero length.
    - ``'sedate`` - Create the file if it does not already exist.
    - ``'append`` - Create the file if it does not already exist, and position
                    the cursor at the end of the file.
    - ``'new``    - The file *must not* exist.  Create it.

**Sharing Behaviour**
    These indicate in what way other processes are allowed to access the file
    whilst you have it open.

    The default is ``'shareNone``.

    .. note::   Sharing modes other than ``'shareWrite`` are not implemented
                for POSIX platforms.  This is a limitation of the underlying
                library.

    - ``'snone``, ``'shareNone`` - Do not share file access.
    - ``'sread``, ``'shareRead`` - Share read access.
    - ``'swrite``, ``'shareWrite`` - Share read and write access.
    - ``'sall``, ``'shareAll`` - Share all forms of access.

**Cache Optimisation**
    These flags give the operating system a hint as to how you intend to use
    the file.  These hints may not be supported on all platforms; unsupported
    hints will simply be ignored.

    - ``'noOpt`` - Don't optimise.
    - ``'random`` - Optimise for random access.
    - ``'stream`` - Optimise for streaming access.
    - ``'thru``, ``'writeThru`` - Optimise for write-through.

``seekStream!(stream!, anchor, offset)``
----------------------------------------

Seeks *stream!*\ 's cursor.  *offset* specifies the number of bytes to seek,
whilst *anchor* must be one of the following:

- ``'<``, ``'b``, ``'begin``: seeks relative to the beginning of the stream.
- ``'|``, ``'c``, ``'current``: seeks relative to the current position of the
                              cursor.
- ``'>``, ``'e``, ``'end``: seeks relative to the end of the stream.

``writeToStream!(stream!, values...)``
--------------------------------------

Writes the contents of one or more *values* to *stream!*.  The exact behaviour
depends on the type of value being written.  Values of any type not listed
below results in a failure.

The function may return ``'eof``.

``String``
    Written to the stream as UTF-8 encoded text.  Invalid UTF-8 sequences may
    cause a failure.

``Function``
    If a function of one argument, the function is called with the stream and
    its result discarded, unless the function returned ``'eof`` which is
    immediately returned.

    If a function of zero arguments, the function is called and its result
    written out.

    Any other number of arguments results in a failure.

