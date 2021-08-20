# adhoc-http

A command line program to quickly create an http server serving files
from one or more directories on a local disk. Each directory will be
avialable over http at a randomly generated guid or the specified prefix.

Example usage:

    $ adhoc-http -p 8080 path/to/dir/one path/to/dir/two:Foo
    ...
    Serving following dirs:
    * path/to/dir/one at http://organon.local:8080/4725de26-1722-43bc-9a5e-712ce574b7b1/
    * path/to/dir/two at http://organon.local:8080/Foo/
    * path/to/dir/three at http://organon.local:8080/fiz/baz/

Serve without prefix:

    $ adhoc-http -p 8080 path/to/dir:
    Serving following dirs:
    * path/to/dir at http://organon.local:8080/

The server is based on Snap. Some files are served compressed, based
on the file extension, determined by Snap defaults. I have modified the
defaults to additionally compress csv files. Come to think of it,
perhaps a better solution would be to compress everything except for
files that are known to be compressed. Pull requests welcome.
