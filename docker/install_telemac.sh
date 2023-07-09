#!/bin/bash

if [ -z "$1" ]; then
    telemac_dir=$PWD
else
    telemac_dir=$1
fi

export HOMETEL=$telemac_dir/telemac_home

echo "TELEMAC will be installed in the directory $HOMETEL"
source pysource.reos.sh

cd $telemac_dir

echo "Build and install metis"
if [ ! -d "metis" ] ; then
    gunzip -d -k metis-5.1.0.tar.gz
    tar -xvf metis-5.1.0.tar
    cd metis-5.1.0
    cmake -D CMAKE_INSTALL_PREFIX="$telemac_dir/metis/"
    make -j5
    make install
    cd ..
    rm -r metis-5.1.0
    rm metis-5.1.0.tar
fi

echo "Clone telemac"
if [ ! -d $HOMETEL ] ; then
    git clone https://gitlab.pam-retd.fr/otm/telemac-mascaret.git $HOMETEL
fi

cd $HOMETEL
git checkout tags/v8p4r0
cd ..

echo "Build telemac"
python3 $HOMETEL/scripts/python3/compile_telemac.py


