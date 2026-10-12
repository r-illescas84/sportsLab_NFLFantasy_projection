FROM rocker/r-ver:4.4.2

WORKDIR /workspace

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl libcurl4-openssl-dev libssl-dev libxml2-dev \
    libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
    libpng-dev libtiff5-dev libjpeg-dev zlib1g-dev pkg-config \
    && rm -rf /var/lib/apt/lists/*

ENV RENV_PATHS_CACHE=/renv/cache
ENV RENV_CONFIG_REPOS_OVERRIDE=https://cloud.r-project.org

RUN echo 'options(repos = c(CRAN = "https://cloud.r-project.org"))' >> /usr/local/lib/R/etc/Rprofile.site

RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')"

CMD ["tail", "-f", "/dev/null"]
