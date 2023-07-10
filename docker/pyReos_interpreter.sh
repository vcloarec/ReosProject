export LD_LIBRARY_PATH=$REOS_DIR/lib:/usr/lib/x86_64-linux-gnu/:$GMSH_DIR/lib/:$QGIS_DIR/lib/:$MDAL_DIR/lib/
export PYTHONPATH="$REOS_DIR/python/":$PYTHONPATH
export QGIS_PREFIX_PATH=$QGIS_DIR
export REOS_PREFIX_PATH=$REOS_DIR

python3
