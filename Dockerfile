FROM rocker/r-ver:4.3.3

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsqlite3-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    zlib1g-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c( \
    'shiny','bslib','ggplot2','dplyr','lubridate','scales','htmltools', \
    'readxl','DBI','RSQLite','tidyr','janitor','DT','stringr','purrr' \
    ), repos='https://cloud.r-project.org')"

WORKDIR /opt/render/project/src

COPY . .

ENV APP_ROOT=/opt/render/project/src
ENV APP_DATA_DIR=/opt/render/project/src/data
ENV APP_CACHE_DIR=/opt/render/project/src/data/cache
ENV APP_RAW_DIR=/opt/render/project/src/data/raw

EXPOSE 3838

CMD ["Rscript", "run.R"]