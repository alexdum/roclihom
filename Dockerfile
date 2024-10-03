FROM rocker/shiny-verse:latest

WORKDIR /code

# Install stable packages from CRAN
RUN install2.r --error \
    ggplot2 \
    shiny \
    leaflet \
    leaflet.extras \
    arrow \
    dplyr

# Install development packages from GitHub
RUN installGithub.r \
    rstudio/bslib \
    rstudio/httpuv

COPY . .

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]
