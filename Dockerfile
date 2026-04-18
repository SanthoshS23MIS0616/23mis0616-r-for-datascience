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

RUN R -q -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); install.packages(c('plumber','nnet','e1071','randomForest'), Ncpus = max(1L, parallel::detectCores() - 1L))"

COPY R ./R
COPY data ./data
COPY models ./models
COPY reports ./reports
COPY static ./static
COPY plumber.R run_api.R train_models.R install_packages.R ./

EXPOSE 10000

CMD ["sh", "-c", "Rscript run_api.R"]
