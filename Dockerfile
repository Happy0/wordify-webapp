FROM fpco/stack-build:lts-23.11 as build

RUN apt-get update

COPY . .

# Without this haddock crashes for modules containing
# non-ASCII characters.
ENV LANG C.UTF-8

RUN mkdir -p "/data"

RUN stack build --copy-bins --local-bin-path "bin"

FROM ubuntu:25.04 as app
COPY --from=build bin bin

RUN apt-get update && apt-get install -y libgnutls30 netbase libstdc++6 ca-certificates

COPY config config
COPY static static
COPY templates templates
COPY ui ui

CMD YESOD_HOST=0.0.0.0 YESOD_PORT=8080 YESOD_SQLITE_DATABASE="/data/wordify-webapp.sqlite3" SESSION_BACKEND_CERTIFICATE_DIRECTORY="/data" ./bin/wordify-webapp
