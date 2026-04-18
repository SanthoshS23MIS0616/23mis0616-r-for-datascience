FROM rocker/r-ver:4.5.1

ENV DEBIAN_FRONTEND=noninteractive
ENV R_PORT_PROJECT_ROOT=/app
ENV PORT=10000

WORKDIR /app

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       libcurl4-openssl-dev \
       libssl-dev \
       libxml2-dev \
       libfontconfig1-dev \
       libfreetype6-dev \
       libpng-dev \
       libtiff5-dev \
       libjpeg-dev \
       make \
       g++ \
       gcc \
       gfortran \
    && rm -rf /var/lib/apt/lists/*

RUN R -q -e "install.packages(c('plumber','jsonlite','nnet','e1071','randomForest','ggplot2'), repos='https://cloud.r-project.org')"

COPY R ./R
COPY data ./data
COPY models ./models
COPY reports ./reports
COPY static ./static
COPY plumber.R run_api.R train_models.R install_packages.R ./

EXPOSE 10000

CMD ["sh", "-c", "Rscript run_api.R"]
