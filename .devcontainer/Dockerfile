# Dev container Dockerfile for ReosProject.
#
# Modeled on worker/Dockerfile: a builder stage compiles QGIS (branch
# fix_without_EPT_COPC of vcloarec/QGIS) and ecCodes from source; the final
# stage keeps all build dependencies (so REOS itself can be built from the
# mounted workspace), adds developer tooling, and creates a non-root user.
#
# REOS is intentionally NOT built here — the developer builds it inside the
# container against the workspace using .devcontainer/build.sh.

# ------------------------------------------------------------------------------
# Builder stage: compile QGIS + ecCodes (same recipe as worker/Dockerfile)
# ------------------------------------------------------------------------------
FROM ubuntu:24.04 AS builder

WORKDIR /app

# Reuse the exact QGIS build script from ./worker so the two images stay in sync.
COPY worker/build_qgis.sh /app/build_qgis.sh

ENV DEBIAN_FRONTEND=noninteractive

RUN mkdir /qgis_built

RUN apt-get update && apt-get install -y --no-install-recommends \
    wget bison build-essential ca-certificates ccache cmake cmake-curses-gui \
    dh-python doxygen expect flex flip gdal-bin git graphviz \
    libexiv2-dev libexpat1-dev libfcgi-dev libgdal-dev libgeos-dev \
    libhdf5-dev \
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
RUN git clone --branch fix_without_EPT_COPC --single-branch --depth 1 \
        https://github.com/vcloarec/QGIS.git /qgis_src && \
    chmod +x build_qgis.sh && \
    ./build_qgis.sh /qgis_src && \
    rm -rf /qgis_src

# Download and build ecCodes
RUN mkdir -p /eccodes_built && mkdir -p /app/eccodes_src && cd /app/eccodes_src && \
    wget "https://confluence.ecmwf.int/download/attachments/45757960/eccodes-2.36.4-Source.tar.gz?api=v2" \
        --no-check-certificate -O eccodes-2.36.4-Source.tar.gz && \
    tar -xzf eccodes-2.36.4-Source.tar.gz && \
    mkdir -p /app/eccodes_src/build && cd /app/eccodes_src/build && \
    cmake -DCMAKE_INSTALL_PREFIX=/eccodes_built \
        -DCMAKE_BUILD_TYPE=Release \
        -DENABLE_NETCDF=OFF \
        -DENABLE_FORTRAN=OFF \
        -DPRODUCT_BUFR=OFF \
        -DEXAMPLES=OFF \
        ../eccodes-2.36.4-Source/. && \
    make -j && \
    make install && \
    cd /app && rm -rf /app/eccodes_src


# ------------------------------------------------------------------------------
# Dev stage: reinstall the build deps (needed to build REOS in the workspace),
# add developer tooling, copy pre-built QGIS + ecCodes, create vscode user.
# ------------------------------------------------------------------------------
FROM ubuntu:24.04

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=${USER_UID}

ENV DEBIAN_FRONTEND=noninteractive

# Same build deps as the builder stage (so REOS can be built from the mounted
# workspace via .devcontainer/build.sh) plus developer tooling.
RUN apt-get update && apt-get install -y --no-install-recommends \
    wget bison build-essential ca-certificates ccache cmake cmake-curses-gui \
    dh-python doxygen expect flex flip gdal-bin git graphviz \
    libexiv2-dev libexpat1-dev libfcgi-dev libgdal-dev libgeos-dev \
    libhdf5-dev \
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
    xfonts-100dpi xfonts-75dpi xfonts-base xfonts-scalable xvfb libaec-dev \
    sudo openssh-client gdb lldb clangd clang-format clang-tidy \
    gdbserver less nano vim bash-completion && \
    locale-gen en_US.UTF-8 && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8

# Pre-built QGIS and ecCodes (same install prefixes as worker/build_reos.sh)
COPY --from=builder /qgis_built /qgis_built
COPY --from=builder /eccodes_built /eccodes_built

# Runtime environment for QGIS + ecCodes.
ENV QGIS_PREFIX_PATH=/qgis_built \
    QGIS_PLUGIN_PATH=/qgis_built/lib/qgis/plugins \
    LD_LIBRARY_PATH=/qgis_built/lib:/eccodes_built/lib:/usr/lib/x86_64-linux-gnu \
    PYTHONPATH=/qgis_built/share/qgis/python

# Non-root user matching host UID/GID so bind-mounted workspace files keep
# correct ownership on the host.
RUN if getent passwd ${USER_UID} >/dev/null; then \
        userdel -r "$(getent passwd ${USER_UID} | cut -d: -f1)" || true; \
    fi \
    && groupadd --gid ${USER_GID} ${USERNAME} 2>/dev/null || true \
    && useradd --uid ${USER_UID} --gid ${USER_GID} -m -s /bin/bash ${USERNAME} \
    && echo "${USERNAME} ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/${USERNAME} \
    && chmod 0440 /etc/sudoers.d/${USERNAME} \
    && mkdir -p /home/${USERNAME}/.ccache \
    && chown -R ${USERNAME}:${USERNAME} /home/${USERNAME}

USER ${USERNAME}
WORKDIR /workspaces/ReosProject
