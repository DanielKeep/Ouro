/**
    Implements support for empty: import roots.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.util.uriSchemes.Empty;

import tango.io.model.IConduit : InputStream;
import tango.net.Uri;

import ouro.util.ImportRoot : ImportRoot, Resource, ImportRootSchemes;

class EmptyRoot : ImportRoot
{
    static Uri schemeUri;

    static const Prefix = "/.empty/";

    static this()
    {
        schemeUri = new Uri("empty:");

        ImportRootSchemes.register("empty", &createForUri);
    }

    this(Uri uri)
    in
    {
        assert( uri.scheme == "empty" );
        assert( uri.host == "" );
        assert( uri.port == Uri.InvalidPort );
        assert( uri.userinfo == "" );
        assert( uri.query == "" );
        assert( uri.fragment == "" );
    }
    body
    {
    }

    static ImportRoot createForUri(Uri uri)
    {
        return new EmptyRoot(uri);
    }

    Uri uri()
    {
        return schemeUri;
    }

    Resource resolve(char[] path)
    in
    {
        assert( path.length > 1 );
        assert( path[0] == '/' );
    }
    body
    {
        if( path.length < Prefix.length || path[0..Prefix.length] != Prefix )
            return null;

        return new EmptyResource(new Uri("empty", null, path));
    }

    char[] pathFrom(Uri uri)
    {
        if( uri.scheme != "empty" )
            return null;

        assert( uri.path.length > 0 );
        assert( uri.path[0] == '/' );

        return Prefix ~ uri.path[1..$];
    }
}

class EmptyResource : Resource
{
    Uri uri_;

    this(Uri uri)
    {
        uri_ = uri;
    }

    Uri uri() { return uri_; }

    char[] data()
    {
        return "|-- " ~ uri_.toString ~ "\n";
    }
}

