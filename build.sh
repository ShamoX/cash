#!/bin/bash -u

set +x

####################################################
## This script will run and make the cash package ##
####################################################

LVERSION=$(ocaml -vnum) # Get the version number of your ocaml
RELEASE=$(echo $LVERSION | cut -d . -f -2) # get the main release version

URL_DIR="http://caml.inria.fr/pub/distrib/ocaml-$RELEASE"
ARCHIVE="ocaml-$LVERSION.tar.bz2"

echo "If you already have your local distribution sources of ocaml interpreter"
echo "Please tell us where it is ? If you do not provide it, we will fetch it on Internet"
read SRCPATH

if [ ! -d "$SRCPATH" ]; # PATH not provided or do not exist
then
	wget $URL_DIR/$ARCHIVE
	tar -xjf $ARCHIVE
	SRCPATH="ocaml-$LVERSION"
fi

if [ -d "$SRCPATH" ];
then
	ln -s "$SRCPATH" ocaml
fi

./configure
make
