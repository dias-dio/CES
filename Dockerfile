FROM rocker/shiny:4.4.1

# System dependencies for R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libnlopt-dev \
    cmake \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

# Install R packages securely
RUN install2.r --error \
    shiny \
    bslib \
    shinyjs \
    plotly \
    DT \
    readxl \
    readr \
    dplyr \
    data.table \
    openxlsx \
    ggplot2 \
    drc \
    caTools \
    minpack.lm \
    DEoptim \
    tibble

# Copy app files
RUN rm -rf /srv/shiny-server/*
COPY app/ /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]