FROM fpco/stack-build:lts-18.19

# for nanovg / monomer
RUN apt-get update && apt-get install --assume-yes libsdl2-dev libglew-dev

RUN \
  wget -q https://github.com/apple/foundationdb/releases/download/6.3.23/foundationdb-clients_6.3.23-1_amd64.deb && \
  dpkg -i foundationdb-clients_*_amd64.deb
