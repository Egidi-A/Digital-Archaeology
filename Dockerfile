# Dockerfile per ambiente COBOL con supporto PostgreSQL
FROM ubuntu:22.04

# Evita prompt interattivi durante l'installazione
ENV DEBIAN_FRONTEND=noninteractive

# Aggiorna i repository e installa dipendenze base
RUN apt-get update && apt-get install -y \
    build-essential \
    wget \
    curl \
    git \
    libgmp-dev \
    libdb-dev \
    libncurses5-dev \
    autoconf \
    automake \
    libtool \
    bison \
    flex \
    libpq-dev \
    postgresql-client \
    vim \
    nano \
    && rm -rf /var/lib/apt/lists/*

# Installa GnuCOBOL 3.2
WORKDIR /tmp
RUN wget https://sourceforge.net/projects/gnucobol/files/gnucobol/3.2/gnucobol-3.2.tar.gz && \
    tar -xzf gnucobol-3.2.tar.gz && \
    cd gnucobol-3.2 && \
    ./configure --with-db --with-pgsql && \
    make && \
    make install && \
    ldconfig && \
    cd / && \
    rm -rf /tmp/gnucobol*

# Crea directory di lavoro
WORKDIR /cobol
RUN mkdir -p /cobol/src /cobol/bin /cobol/data /cobol/reports

# Copia i file sorgenti COBOL
COPY src/*.cbl /cobol/src/
COPY sql/*.sql /cobol/sql/

# Script di inizializzazione
COPY scripts/init.sh /cobol/scripts/
COPY scripts/compile.sh /cobol/scripts/
COPY scripts/wait-for-postgres.sh /cobol/scripts/
RUN chmod +x /cobol/scripts/*.sh

# Variabili d'ambiente per COBOL
ENV COB_LIBRARY_PATH=/usr/local/lib
ENV LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
ENV PATH=/cobol/bin:$PATH

# Esponi volume per i report
VOLUME ["/cobol/reports"]

# Comando di default
CMD ["/bin/bash"]