git clone https://github.com/lutraconsulting/MDAL.git

MDAL_BUILT=$PWD

cd MDAL
mkdir build;
cd build
cmake -DCMAKE_BUILD_TYPE=Rel -DENABLE_TESTS=OFF -DCMAKE_INSTALL_PREFIX=$MDAL_BUILT ..
cmake --build .  --config Release -j20
cmake --install .

cd ..
cd ..

rm -r MDAL
ls -la


