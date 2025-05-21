FROM ubuntu:22.04

# Evita prompt durante l'installazione
ENV DEBIAN_FRONTEND=noninteractive

# Installa GnuCOBOL e dipendenze necessarie
RUN apt-get update && apt-get install -y \
    gnucobol \
    libdb-dev \
    build-essential \
    curl \
    unixodbc \
    unixodbc-dev \
    wget \
    libpq-dev \
    python3 \
    vim \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Installa client CLI DB2
RUN mkdir -p /opt/ibm && \
    cd /opt/ibm && \
    wget -q https://public.dhe.ibm.com/ibmdl/export/pub/software/data/db2/drivers/odbc_cli/linuxx64_odbc_cli.tar.gz && \
    tar -xzf linuxx64_odbc_cli.tar.gz && \
    rm linuxx64_odbc_cli.tar.gz

# Configura le variabili d'ambiente per DB2
ENV DB2_CLI_DRIVER_INSTALL_PATH=/opt/ibm/clidriver
ENV LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$DB2_CLI_DRIVER_INSTALL_PATH/lib
ENV PATH=$PATH:$DB2_CLI_DRIVER_INSTALL_PATH/bin

# Directory di lavoro
WORKDIR /app

# Comando predefinito
CMD ["/bin/bash"]