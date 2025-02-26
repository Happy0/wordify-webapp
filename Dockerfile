FROM ubuntu:24.04

RUN apt-get update

# Build dependencies
RUN apt-get install --assume-yes curl
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN apt-get install --assume-yes libtinfo-dev

# Without this haddock crashes for modules containing
# non-ASCII characters.
ENV LANG C.UTF-8

RUN mkdir -p "/data"
RUN mkdir -p "wordifyApp"

COPY . "wordifyApp"
WORKDIR "/wordifyApp"

RUN stack build --copy-bins --local-bin-path bin

CMD YESOD_HOST=0.0.0.0 YESOD_PORT=8080 YESOD_SQLITE_DATABASE="/data/wordify-webapp.sqlite3" EXIT_ON_IDLE=true SESSION_BACKEND_CERTIFICATE_DIRECTORY="/data" ./bin/wordify-webapp +RTS -I0 -RTS
