#!/bin/tcsh

# Script for creating ECB releases.
# Author: Jesper Nordenberg
#
# $Id: make_release.sh,v 1.10 2001/11/19 12:11:54 berndl Exp $

set files="*.el HISTORY Makefile make.bat README RELEASE_NOTES"
set version="1.60"

set name=ecb-$version
set release_dir=releases
set dir=$release_dir/$name


rm -rf $dir
mkdir -p $dir
cp $files $dir
cd $release_dir

echo "Creating .zip file..."
rm -f $name.zip
zip -rv $name.zip $name

echo "\nCreating .tar.gz file..."
rm -f $name.tar.gz
tar cvzf $name.tar.gz $name
