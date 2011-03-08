/**
    Contains code used to represent and create import roots from URIs.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.util.ImportRoot;

import tango.io.model.IConduit : InputStream;
import tango.net.Uri;

interface ImportRoot
{
    Uri uri();
    Resource resolve(char[] path);
}

interface Resource
{
    Uri uri();
    char[] data();
}

abstract final class ImportRootSchemes
{
static:
    ImportRoot createRootFrom(char[] uri)
    {
        return createRootFrom(new Uri(uri));
    }

    ImportRoot createRootFrom(Uri uri)
    {
        if( auto fnptr = (uri.scheme in createFns) )
            return (*fnptr)(uri);
        else
            return null;
    }

    void register(char[] scheme, CreateFn createFn)
    {
        createFns[scheme] = createFn;
    }

private static:
    alias ImportRoot function(Uri) CreateFn;
    CreateFn[char[]] createFns;
}

