FROM ubuntu:18.10

# Setup java
RUN apt-get update && \
    apt-get install -y openjdk-8-jdk && \
    apt-get install -y ant && \
    apt-get clean;

ENV JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64/
RUN export JAVA_HOME

# Setup TLang library
COPY stdlib /home/tlang/

# Setup tlang user
RUN useradd -m tlang
USER tlang
ENV HOME /home/tlang
WORKDIR /home/tlang
