# ===========================================
# Stage 1: Cache Haskell dependencies
# Runs in parallel with the UI build (Stage 2)
# ===========================================
FROM debian:bookworm as backend-deps

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
        gnupg && \
    curl -sSL https://get.haskellstack.org/ | sh && \
    rm -rf /var/lib/apt/lists/*

# Without this haddock crashes for modules containing
# non-ASCII characters.
ENV LANG=C.UTF-8

WORKDIR /app

# Copy only dependency files first to cache dependencies
COPY stack.yaml package.yaml ./
COPY vendor vendor

# Build dependencies only (this layer is cached unless stack.yaml/package.yaml change)
RUN stack build --only-dependencies

# ===========================================
# Stage 2: Build the UI
# Runs in parallel with the dependency build (Stage 1).
# Also updates src/Settings/StaticFiles.hs with the cache-busted JS filename.
# ===========================================
FROM debian:bookworm as ui-build

# Install only what's needed for the UI build
RUN apt-get update && \
    apt-get install --assume-yes --no-install-recommends \
        curl \
        ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# Install nvm and Node.js
ENV NVM_DIR=/root/.nvm
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.1/install.sh | bash && \
    . "$NVM_DIR/nvm.sh" && \
    nvm install --lts && \
    nvm use --lts

WORKDIR /app

# Copy UI files and the Haskell static routes file (updated by build-ui.sh)
COPY ui ui
COPY build-ui.sh ./
COPY src/Settings/StaticFiles.hs src/Settings/StaticFiles.hs

# Create output directories and build the UI
RUN mkdir -p static/js static/css && \
    . "$NVM_DIR/nvm.sh" && bash build-ui.sh

# ===========================================
# Stage 3: Build the Haskell backend
# Depends on stages 1 (cached deps) and 2 (updated source + static files)
# ===========================================
FROM backend-deps as backend-build

# Copy backend source files
COPY app app
COPY src src
COPY config config
COPY templates templates
COPY static static
COPY test test

# Override with UI build outputs: timestamped JS and updated Haskell static route
COPY --from=ui-build /app/src/Settings/StaticFiles.hs src/Settings/StaticFiles.hs
COPY --from=ui-build /app/static/js/ static/js/
COPY --from=ui-build /app/static/css/ static/css/
COPY --from=ui-build /app/static/sw.js static/sw.js

# Build the application
RUN stack build --copy-bins --local-bin-path "bin"

# ===========================================
# Stage 4: Runtime - use slim image for smaller size
# ===========================================
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

# Copy backend binary
COPY --from=backend-build /app/bin ./bin

# Copy config, templates, and base static files
COPY config config
COPY static static
COPY templates templates

# Copy UI build outputs on top of static files
COPY --from=ui-build /app/static/js/ static/js/
COPY --from=ui-build /app/static/css/ static/css/
COPY --from=ui-build /app/static/sw.js static/sw.js

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    YESOD_HOST=0.0.0.0 \
    YESOD_PORT=8080

CMD ["sh", "-c", "YESOD_SQLITE_DATABASE=/data/wordify-webapp-v3.sqlite3 VAPID_KEYS_DIR=/data SESSION_BACKEND_CERTIFICATE_DIRECTORY=/data ./bin/wordify-webapp"]
