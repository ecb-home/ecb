#!/bin/tcsh

# Script for creating ECB releases.
# Author: Jesper Nordenberg
#
# $Id: make_release.sh,v 1.14 2003/01/07 19:20:15 berndl Exp $

if( "$1" == "" ) then
    echo "Usage: make_release.sh <version-number>"
    exit -1    
endif

# building the distribution file: ecb-$1.tar.gz
make ecb_VERSION=$1 distrib

name=ecb-$1
release_dir=releases

mv -f $name.tar.gz $release_dir
cd $release_dir
rm -Rf $name
tar -xzvf $name.tar.gz 

echo -n "Creating .zip file..."
rm -f $name.zip
zip -rq $name.zip $name
echo "done"

cd -
