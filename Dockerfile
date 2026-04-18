FROM rocker/r-ver:4.5.3

ENV DEBIAN_FRONTEND=noninteractive
ENV R_PORT_PROJECT_ROOT=/app
ENV PORT=10000

WORKDIR /app

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       ca-certificates \
       libcurl4-openssl-dev \
       libssl-dev \
       libxml2-dev \
       make \
       g++ \
       gcc \
       gfortran \
    && rm -rf /var/lib/apt/lists/*

COPY R ./R
COPY install_packages.R ./

RUN Rscript install_packages.R \
    && R -q -e "pkgs <- c('plumber','nnet','e1071','randomForest'); missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]; if (length(missing)) stop(sprintf('Missing R packages: %s', paste(missing, collapse = ', ')))"

COPY data ./data
COPY models ./models
COPY reports ./reports
COPY static ./static
COPY plumber.R run_api.R train_models.R ./

EXPOSE 10000

CMD ["sh", "-c", "Rscript run_api.R"]
RUN apt-get update && apt-get install -y \
    pkg-config \
    libz-dev \
    zlib1g-dev \
    libsodium-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*