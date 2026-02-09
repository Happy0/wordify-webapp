# ===========================================
# Stage 1: Build the Haskell backend
# Changes to ui/ will NOT invalidate this stage
# ===========================================
FROM debian:bookworm as backend-build

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

# Build dependencies only (this layer is cached unless stack.yaml/package.yaml change)
RUN stack build --only-dependencies

# Copy only backend source files (not ui/)
COPY app app
COPY src src
COPY config config
COPY templates templates
COPY static static
COPY test test

# Create placeholder files for Yesod's staticFiles TH splice.
# The real files come from the ui-build stage; these just need to exist
# so the compile-time identifier generation succeeds.
RUN mkdir -p static/js static/css && \
    touch static/js/wordify.js static/js/wordify.umd.js.map static/css/wordify.css static/sw.js

# Build the application
RUN stack build --copy-bins --local-bin-path "bin"

# ===========================================
# Stage 2: Build the UI
# Changes to backend files will NOT invalidate this stage
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

# Copy only UI files
COPY ui ui
COPY build-ui.sh ./

# Create output directories and build the UI
RUN mkdir -p static/js static/css && \
    . "$NVM_DIR/nvm.sh" && bash build-ui.sh

# ===========================================
# Stage 3: Runtime - use slim image for smaller size
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
COPY --from=ui-build /app/static/js/wordify.js static/js/wordify.js
COPY --from=ui-build /app/static/js/wordify.umd.js.map static/js/wordify.umd.js.map
COPY --from=ui-build /app/static/css/wordify.css static/css/wordify.css
COPY --from=ui-build /app/static/sw.js static/sw.js

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    YESOD_HOST=0.0.0.0 \
    YESOD_PORT=8080

CMD ["sh", "-c", "YESOD_SQLITE_DATABASE=/data/wordify-webapp-v3.sqlite3 SESSION_BACKEND_CERTIFICATE_DIRECTORY=/data ./bin/wordify-webapp"]
