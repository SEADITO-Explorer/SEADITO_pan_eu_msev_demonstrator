FROM rocker/r-ver:4.4.0

# System libraries: sf/raster/terra (GDAL, GEOS, PROJ, udunits), ragg (fonts, images), web
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgdal-dev libgeos-dev libproj-dev libudunits2-dev libsqlite3-dev \
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    libharfbuzz-dev libfribidi-dev libfontconfig1-dev \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# R packages used by app.R
RUN R -e "install.packages(c('shiny', 'shinyWidgets', 'leaflet', 'dplyr', 'sf', 'ggplot2', 'ggrepel', 'stringr', 'forcats', 'tidyr', 'scales', 'pals', 'patchwork', 'DT', 'raster', 'RColorBrewer', 'tibble', 'viridisLite', 'ragg'), Ncpus = 4)"

# Fail the build here if any package did not install
RUN R -e "pkgs <- c('shiny', 'shinyWidgets', 'leaflet', 'dplyr', 'sf', 'ggplot2', 'ggrepel', 'stringr', 'forcats', 'tidyr', 'scales', 'pals', 'patchwork', 'DT', 'raster', 'RColorBrewer', 'tibble', 'viridisLite', 'ragg'); miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]; if (length(miss)) stop('Missing packages: ', paste(miss, collapse = ', '))"

WORKDIR /app
COPY . /app

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
