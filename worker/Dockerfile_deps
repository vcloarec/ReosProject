FROM ubuntu:24.04 AS builder

# Set the working directory
WORKDIR /app

# Copy the current directory contents into the container at /app
COPY build_qgis.sh /app/build_qgis.sh
COPY build_reos.sh /app/build_reos.sh

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    wget bison build-essential ca-certificates ccache cmake cmake-curses-gui \
    dh-python doxygen expect flex flip gdal-bin git graphviz \
    libexiv2-dev libexpat1-dev libfcgi-dev libgdal-dev libgeos-dev \
    libgsl-dev libpq-dev libproj-dev libprotobuf-dev \
    libqca-qt5-2-dev libqca-qt5-2-plugins libqscintilla2-qt5-dev \
    libqt5opengl5-dev libqt5serialport5-dev libqt5sql5-sqlite \
    libqt5svg5-dev libqt5xmlpatterns5-dev \
    libqwt-qt5-dev libspatialindex-dev libspatialite-dev \
    libsqlite3-dev libsqlite3-mod-spatialite libyaml-tiny-perl \
    libzip-dev lighttpd locales ninja-build \
    ocl-icd-opencl-dev opencl-headers pandoc pkg-config \
    poppler-utils protobuf-compiler pyqt5-dev pyqt5-dev-tools \
    pyqt5.qsci-dev python3-all-dev python3-autopep8 python3-dev \
    python3-gdal python3-jinja2 python3-lxml python3-mock \
    python3-nose2 python3-owslib python3-plotly python3-psycopg2 \
    python3-pygments python3-pyproj python3-pyqt5 python3-pyqt5.qsci \
    python3-pyqt5.qtmultimedia python3-pyqt5.qtpositioning \
    python3-pyqt5.qtserialport python3-pyqt5.qtsql \
    python3-pyqt5.qtsvg python3-pyqt5.qtwebkit python3-pyqtbuild \
    python3-sip python3-termcolor python3-yaml \
    qtbase5-dev qtbase5-private-dev qtkeychain-qt5-dev \
    qtmultimedia5-dev qtpositioning5-dev qttools5-dev \
    qttools5-dev-tools sip-tools spawn-fcgi xauth \
    xfonts-100dpi xfonts-75dpi xfonts-base xfonts-scalable xvfb libaec-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/*



# Clone and build QGIS
RUN git clone --branch fix_without_EPT_COPC --single-branch --depth 1 https://github.com/vcloarec/QGIS.git /qgis_src && \
    chmod +x build_qgis.sh && \
    ./build_qgis.sh /qgis_src && \
    rm -rf /qgis_src


RUN mkdir -p /eccodes_built && mkdir -p /app/eccodes_src && cd /app/eccodes_src && \
    wget "https://confluence.ecmwf.int/download/attachments/45757960/eccodes-2.36.4-Source.tar.gz?api=v2" --no-check-certificate -O eccodes-2.36.4-Source.tar.gz && \
    tar -xzf eccodes-2.36.4-Source.tar.gz && \
    mkdir -p /app/eccodes_src/build && ls -la && cd /app/eccodes_src/build && \
    cmake -DCMAKE_INSTALL_PREFIX=/eccodes_built \
    -DCMAKE_BUILD_TYPE=Release \
    -DENABLE_NETCDF=OFF \ 
    -DENABLE_FORTRAN=OFF \
    -DPRODUCT_BUFR=OFF \
    -DEXAMPLES=OFF \
    ../eccodes-2.36.4-Source/. && \
    make -j && \
    make install && \
    cd /app && \
    rm -rf /app/eccodes_src

# # Clone and build REOS
RUN git clone --branch vortexio --single-branch https://github.com/vcloarec/ReosProject.git /reos_src \
    && chmod +x build_reos.sh \
    && ./build_reos.sh /reos_src \ 
    && rm -r -f /reos_src


FROM ubuntu:24.04
WORKDIR /app

ENV DEBIAN_FRONTEND=noninteractive

# Install only runtime dependencies (not compilers, cmake, etc.)
RUN apt update && apt-get install -y --no-install-recommends \
    gdal-bin libgeos-c1v5 proj-bin \
    libpq5 libsqlite3-0 libspatialindex6 libspatialite8t64 \
    libexiv2-27 libexpat1 libfcgi0ldbl \
    libqca-qt5-2 libqscintilla2-qt5-15 \
    libqt5sql5-sqlite libqt5svg5 libqt5xmlpatterns5 libqt5webkit5 \
    libqwt-qt5-6 libzip4 locales \
    poppler-utils libqt5keychain1 \
    ocl-icd-libopencl1 libprotobuf-lite32t64 libgsl27\
    libqt5multimedia5 libqt5positioning5 libqt5concurrent5 \
    python3 python3-pip &&\
    pip install --no-cache-dir --break-system-package numpy>2.0.0 wheel setuptools>=67

RUN apt-get install python3-gdal python3-lxml python3-owslib python3-psycopg2 \
    python3-pygments python3-pyproj python3-pyqt5 python3-pyqt5.qsci \
    python3-pyqt5.qtmultimedia python3-pyqt5.qtpositioning \
    python3-pyqt5.qtserialport python3-pyqt5.qtsql \
    python3-pyqt5.qtsvg python3-pyqt5.qtwebkit python3-sip python3-yaml \
    lighttpd && \
    apt clean && rm -rf /var/lib/apt/lists/*

RUN  apt clean && rm -rf /var/lib/apt/lists/*


# Copy only the built QGIS and EcCodes output from stage 1
COPY --from=builder /app/qgis_built /qgis_built
COPY --from=builder /eccodes_built /eccodes_built
COPY --from=builder /app/reos_built /reos_built

# Expose port 8000
EXPOSE 8000


