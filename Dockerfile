FROM debian:latest as build

RUN apt-get update

COPY . .

# Build dependencies

RUN apt-get install --assume-yes curl
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN apt-get install --assume-yes libtinfo-dev

# Without this haddock crashes for modules containing
# non-ASCII characters.
ENV LANG C.UTF-8

RUN mkdir -p "/data"

RUN stack build --copy-bins --local-bin-path "bin"

FROM debian:latest as app
COPY --from=build bin bin

RUN apt-get update && apt-get install -y libgnutls30 netbase libstdc++6 ca-certificates

COPY config config
COPY static static
COPY templates templates
COPY ui ui

CMD YESOD_HOST=0.0.0.0 YESOD_PORT=8080 YESOD_SQLITE_DATABASE="/data/wordify-webapp-v3.sqlite3" SESSION_BACKEND_CERTIFICATE_DIRECTORY="/data" ./bin/wordify-webapp
