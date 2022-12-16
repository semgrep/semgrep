#!!!!You need to source me with "source env.sh" from the right directory!!!!
if [ ! -r main.ml ]
    then echo "There is no main.ml here.
Are you sure you ran this script from the source directory of pfff?
";
fi

# To compile the source, using pad installation.
echo setting OPAM
#eval `~pad/packages/Linux/bin/opam config env` does not work, it expands
# to /home/pieter/.opam/... when done by another user => inlined here
OPAM=4.01
export PATH=/home/pad/.opam/$OPAM/bin:/home/pad/packages/sbin:/home/pad/packages/bin:/home/pad/bin:/usr/kerberos/bin:/opt/local/bin:/usr/local/bin:/bin:/usr/bin:/usr/facebook/ops/scripts:/usr/facebook/scripts:/usr/facebook/scripts:/usr/facebook/scripts/db:/usr/local/sbin:/usr/sbin:/sbin:/mnt/vol/engshare/svnroot/tfb/trunk/www/scripts/bin:/mnt/vol/engshare/admin/scripts/hg:/mnt/vol/engshare/admin/scripts/git:/mnt/vol/engshare/admin/scripts:/home/pad/www/scripts/bin:/home/pad/packages/Linux/bin
export CAML_LD_LIBRARY_PATH=/home/pad/.opam/$OPAM/lib/stublibs

# for exception stack traces
echo setting OCAMLRUNPARAM
export OCAMLRUNPARAM="b"

# To run, to find the data/ config files, and to run the tests,
# to find the tests/ files.
echo setting PFFF_HOME
export PFFF_HOME=`pwd`

# for ocamlgtk
echo setting PKG_CONFIG_PATH
export PKG_CONFIG_PATH=/usr/X11/lib/pkgconfig
