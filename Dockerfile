# Build stage
FROM debian:bookworm as build

# Install build dependencies in a single layer and clean up
RUN apt-get update && \
    apt-get install --assume-yes --no-install-recommends \
        curl \
        libtinfo-dev \
        ca-certificates \
        g++ \
        gcc \
        libgmp-dev \
        make \
        xz-utils \
        zlib1g-dev \
        git \
        gnupg \
        npm && \
    curl -sSL https://get.haskellstack.org/ | sh && \
    rm -rf /var/lib/apt/lists/*


# Without this haddock crashes for modules containing
# non-ASCII characters.
ENV LANG=C.UTF-8

WORKDIR /app

# Copy only dependency files first to cache dependencies
COPY stack.yaml package.yaml ./

# Build dependencies only (this layer is cached unless stack.yaml/package.yaml change)
RUN stack build --only-dependencies

# Now copy the rest of the source code
COPY . .

# Build the application
RUN stack build --copy-bins --local-bin-path "bin"

RUN bash build-ui.sh

# Runtime stage - use slim image for smaller size
FROM debian:bookworm-slim as app

# Install runtime dependencies in a single layer and clean up
RUN apt-get update && \
    apt-get install --assume-yes --no-install-recommends \
        libgnutls30 \
        netbase \
        libstdc++6 \
        ca-certificates \
        libgmp10 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy only what's needed for runtime
COPY --from=build /app/bin ./bin
COPY config config
COPY static static
COPY templates templates
COPY ui ui

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    YESOD_HOST=0.0.0.0 \
    YESOD_PORT=8080

CMD ["sh", "-c", "YESOD_SQLITE_DATABASE=/data/wordify-webapp-v3.sqlite3 SESSION_BACKEND_CERTIFICATE_DIRECTORY=/data ./bin/wordify-webapp"]
