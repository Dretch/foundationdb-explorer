FROM fpco/stack-build:lts-18.19

# for nanovg, which is needed for monomer
RUN apt update && apt install --assume-yes libglew-dev

RUN \
  wget https://github.com/apple/foundationdb/releases/download/6.3.23/foundationdb-clients_6.3.23-1_amd64.deb && \
  dpkg -i foundationdb-clients_*_amd64.deb
