/**
    Implements support for file: import roots.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.util.uriSchemes.File;

import tango.io.device.File;
import tango.io.device.FileMap : MappedFile;
import tango.io.model.IConduit : InputStream;
import tango.net.Uri;
import tango.sys.Environment;

import ouro.util.ImportRoot : ImportRoot, Resource, ImportRootSchemes;

import Path = tango.io.Path;

class FileRoot : ImportRoot
{
    this(Uri uri)
    in
    {
        assert( uri.scheme == "file" );
        assert( uri.host == "" );
        assert( uri.port == Uri.InvalidPort );
        assert( uri.userinfo == "" );
        assert( uri.query == "" );
        assert( uri.fragment == "" );
    }
    body
    {
        this.uri_ = uri;
        this.basePath = pathFrom(uri);
    }

    static ImportRoot createForUri(Uri uri)
    {
        return new FileRoot(uri);
    }

    static ImportRoot createForPath(char[] path)
    {
        version( Windows )
            path = Path.standard(path);
        auto absPath = Path.normalize(Environment.toAbsolute(path));
        version( Windows )
            absPath = "/" ~ absPath;

        return createForUri(new Uri("file", null, absPath, null));
    }

    Uri uri() { return uri_; }

    Resource resolve(char[] path)
    in
    {
        assert( path.length > 1 );
        assert( path[0] == '/' );
    }
    body
    {
        // We need to drop the leading '/'
        auto newPath = Path.join(basePath, path[1..$]);
        if( Path.exists(newPath) )
            return new FileResource(new Uri("file", null, newPath), newPath);
        else
            return null;
    }

private:
    static this()
    {
        ImportRootSchemes.register("file", &createForUri);
    }

    Uri uri_;
    char[] basePath;

    static char[] pathFrom(Uri uri)
    {
        auto path = uri.path;

        // On Windows, we have to remove the leading slash to expose the root.
        version( Windows )
            path = path[1..$];

        return path;
    }
}

class FileResource : Resource
{
    private
    {
        Uri uri_;
        char[] path;

        MappedFile file;
        char[] data_;
    }

    this(Uri uri, char[] path)
    {
        this.uri_ = uri;
        this.path = path;
    }

    Uri uri() { return uri_; }

    char[] data()
    {
        if( file is null )
            file = new MappedFile(path, File.ReadExisting);

        return cast(char[]) file.map;
    }
}

