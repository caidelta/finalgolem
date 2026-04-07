FROM --platform=linux/amd64 rocker/shiny:latest

# System dependencies for Rcpp compilation and R packages
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

# Install remotes and devtools
RUN Rscript -e "install.packages(c('remotes','devtools'), repos='https://cloud.r-project.org')"

# Copy app source
WORKDIR /srv/shiny-server/fin451app
COPY . .

# Install RTL from GitHub then install package with ALL dependencies from DESCRIPTION
RUN Rscript -e "remotes::install_github('risktoollib/RTL')"
RUN Rscript -e "remotes::install_local('.', dependencies = TRUE, upgrade = 'never')"

# Data is pre-cached as feather files committed to git (inst/app/data/).
# No API call needed at build time or runtime.

# Configure Shiny server to serve this app
RUN echo 'run_as shiny;\nserver {\n  listen 3838;\n  location / {\n    app_dir /srv/shiny-server/fin451app;\n    log_dir /var/log/shiny-server;\n  }\n}' \
    > /etc/shiny-server/shiny-server.conf

EXPOSE 3838

CMD ["/bin/sh", "-c", "Rscript -e \"options('shiny.port'=3838, 'shiny.host'='0.0.0.0'); fin451app::run_app()\""]
