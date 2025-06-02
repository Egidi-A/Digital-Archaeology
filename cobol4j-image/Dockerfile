FROM openjdk:11-jdk

# Evita prompt durante l'installazione
ENV DEBIAN_FRONTEND=noninteractive

# Installa dipendenze necessarie
RUN apt-get update && apt-get install -y \
    wget \
    unzip \
    build-essential \
    curl \
    vim \
    git \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Scarica e installa OpenSourceCOBOL-4J
RUN mkdir -p /opt/opensourcecobol4j && \
    cd /opt/opensourcecobol4j && \
    wget -q opensourcecobol4j-v1.1.9.tar.gz https://github.com/opensourcecobol/opensourcecobol4j/archive/refs/tags/v1.1.9.tar.gz && \
    tar -xzf opensourcecobol4j-v1.1.9.tar.gz && \
    rm opensourcecobol4j-v1.1.9.tar.gz

# Configura le variabili d'ambiente per OpenSourceCOBOL-4J
ENV COBC4J_HOME=/opt/opensourcecobol4j/opensourcecobol4j-1.4.0
ENV PATH=$PATH:$COBC4J_HOME/bin
ENV JAVA_HOME=/usr/local/openjdk-11

# Installa driver JDBC per PostgreSQL
RUN mkdir -p /opt/postgresql-jdbc && \
    cd /opt/postgresql-jdbc && \
    wget -q https://jdbc.postgresql.org/download/postgresql-42.6.0.jar

# Configura CLASSPATH per includere i driver JDBC
ENV CLASSPATH=$CLASSPATH:/opt/postgresql-jdbc/postgresql-42.6.0.jar:$COBC4J_HOME/lib/*

# Directory di lavoro
WORKDIR /app

# Comando predefinito
CMD ["/bin/bash"]