git clone https://github.com/HydrologicEngineeringCenter/hec-dss.git

cd hec-dss
git apply ../hecdss_jni.patch
cd heclib
make -j20
cd ..
cd ..

cp -R ./hec-dss/heclib/heclib_c/src/headers .
cp ./hec-dss/heclib/Output/heclib.a .

rm -r hec-dss



