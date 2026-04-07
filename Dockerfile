FROM rocker/shiny:latest

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

# Install remotes first (needed to install from GitHub)
RUN Rscript -e "install.packages('remotes', repos='https://cloud.r-project.org')"

# Install RTL from GitHub (dev version per assignment requirement)
RUN Rscript -e "remotes::install_github('risktoollib/RTL')"

# Install CRAN dependencies (ordered slow→fast; layer-cached)
RUN Rscript -e "install.packages(c( \
  'shiny', 'bslib', 'golem', 'config', \
  'dplyr', 'tidyr', 'lubridate', 'purrr', \
  'plotly', 'ggplot2', \
  'fredr', 'Rcpp', 'zoo', 'slider', \
  'FactoMineR', 'factoextra', \
  'DT', 'shinycssloaders', 'scales', 'rlang' \
), repos = 'https://cloud.r-project.org')"

# Copy app source and install as a package
WORKDIR /srv/shiny-server/fin451app
COPY . .

RUN Rscript -e "remotes::install_local('.', dependencies = FALSE, upgrade = 'never')"

# Data is pre-cached as feather files committed to git (inst/app/data/).
# No API call needed at build time or runtime.

# Configure Shiny server to serve this app
RUN echo 'run_as shiny;\nserver {\n  listen 3838;\n  location / {\n    app_dir /srv/shiny-server/fin451app;\n    log_dir /var/log/shiny-server;\n  }\n}' \
    > /etc/shiny-server/shiny-server.conf

EXPOSE 3838

CMD ["/bin/sh", "-c", "Rscript -e \"fin451app::run_app(host='0.0.0.0', port=3838)\""]
