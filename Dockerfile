FROM rocker/shiny-verse:latest

WORKDIR /code
# Install system dependencies
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error \
    ggplot2 \
    shiny \
    mapgl \
    sf \
    arrow \
    dplyr \
    plotly \
    EnvStats \
    seas \
    tidyr \
    markdown\
    bsicons

# Install development packages from GitHub
RUN installGithub.r \
    rstudio/bslib \
    rstudio/httpuv

COPY . .

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]
