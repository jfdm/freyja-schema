#!/bin/bash

echo "Creating Dir"
mkdir -p sifdeps
cd sifdeps/

echo "Fetching Deps for Sif"

echo "Fetching lightyear "
git clone git@github.com:ziman/lightyear.git lightyear
cd lightyear/
make install
cd ../

echo "Fetching containers"
git clone git@github.com:jfdm/idris-containers.git containers
cd containers/
make install
cd ../

echo "Fetching grl       "
git clone git@github.com:jfdm/idris-grl.git grl
cd grl/
make install
cd ../

echo "Fetching edda      "
git clone git@github.com:jfdm/edda.git edda
cd edda/
make install
cd ../

echo "Fetching xml       "
git clone git@github.com:jfdm/idris-xml.git xml
cd xml/
make install
cd ../

cd ../
