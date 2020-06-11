FROM rocker/shiny-verse:3.5

# Logging path
RUN mkdir -p /var/lib/shiny-server/bookmarks/shiny

# Install presequities
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    file \
    git \
    libapparmor1 \
    libcurl4-openssl-dev \
    libedit2 \
    libssl-dev \
    lsb-release \
    psmisc \
    procps \
    python-setuptools \
    sudo \
    wget \
    libclang-dev \
    libclang-3.8-dev \
    libobjc-6-dev \
    libclang1-3.8 \
    libclang-common-3.8-dev \
    libllvm3.8 \
    libobjc4 \
    libgc1c2 \
    python-pip \
    dialog \
    nano \
    && pip install virtualenv \
    && pip install awscli

RUN ["install2.r", "caret", "DAAG", "data.table", "DataCombine", "DescTools", "dplyr", "DT", "fastDummies", "forcats", "forecast", "fracdiff", "ggplot2", "greybox", "lattice", "lmtest", "lubridate", "MAPA", "Metrics", "nnfor", "plyr", "purrr", "RColorBrewer", "readr", "readxl", "shiny", "shinycssloaders", "shinyjs", "shinythemes", "smooth", "stringr", "tibble", "tictoc", "tidyr", "tidyverse", "tsbox", "tseries", "tsintermittent", "TSstudio", "xts", "zoo"]

COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod 777 /usr/bin/shiny-server.sh

COPY app /srv/shiny-server/
RUN chmod -R 777 /srv/shiny-server/

# RUN mkdir /var/log/
RUN chmod 777 /var/log/

USER shiny

EXPOSE 3838

CMD ["/usr/bin/shiny-server.sh"]
