FROM ubuntu:18.10

# Setup java
RUN apt-get update && \
    apt-get install -y openjdk-8-jdk && \
    apt-get install -y ant && \
    apt-get clean;

ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/
RUN export JAVA_HOME

# Remove setgid and setuid rights of all binaries
RUN find / -xdev -perm /6000 -type f -exec chmod a-s {} \; || true

# Setup tlang user
RUN useradd -m tlang
USER tlang
ENV HOME /home/tlang
WORKDIR /home/tlang

# Setup TLang library
COPY stdlib /home/tlang/stdlib
