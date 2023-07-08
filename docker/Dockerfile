FROM ubuntu:latest
LABEL maintainer="vcloarec@gmail.com"

RUN export DEBIAN_FRONTEND=noninteractive \
       && export APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE=1 \
       && apt update \
       && apt-get upgrade -y \
       && apt-get install -y git \
       && apt-get install -y mpich\
       && apt-get install -y cmake\
       && apt-get install -y python3\
       && apt-get install -y python3-pip\
       && pip3 install numpy

ARG TELEMAC_DIR="/telemac"

RUN mkdir ${TELEMAC_DIR}
COPY install_telemac.sh ${TELEMAC_DIR}/
COPY pysource.reos.sh ${TELEMAC_DIR}/
COPY systel.reos-ubuntu.cfg ${TELEMAC_DIR}/
COPY ./dependencies/metis-5.1.0.tar.gz ${TELEMAC_DIR}/
RUN chmod +x ${TELEMAC_DIR}/install_telemac.sh
RUN cd ${TELEMAC_DIR} \
       &&  ./install_telemac.sh
       
COPY build_gmsh.sh /
RUN chmod +x /build_gmsh.sh
RUN ./build_gmsh.sh

ARG QGIS_DIR="/qgis"

COPY build_qgis.sh ${QGIS_DIR}/
RUN chmod +x ${QGIS_DIR}/build_qgis.sh
RUN export QGIS_SRC=${QGIS_DIR}/qgis_src &&  cd ${QGIS_DIR} && ./build_qgis.sh

