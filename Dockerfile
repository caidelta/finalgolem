FROM --platform=linux/amd64 rocker/shiny:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    g++ \
    cmake \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e "install.packages(c('remotes','devtools'), repos='https://cloud.r-project.org')"

WORKDIR /srv/shiny-server/fin451app
COPY . .

RUN Rscript -e "remotes::install_github('risktoollib/RTL')"
RUN Rscript -e "remotes::install_local('.', dependencies = TRUE, upgrade = 'never')"

RUN echo 'run_as shiny;\nserver {\n  listen 7860;\n  location / {\n    app_dir /srv/shiny-server/fin451app;\n    log_dir /var/log/shiny-server;\n  }\n}' \
    > /etc/shiny-server/shiny-server.conf

EXPOSE 7860

CMD ["/bin/sh", "-c", "Rscript -e \"options('shiny.port'=7860, 'shiny.host'='0.0.0.0'); fin451app::run_app()\""]
