# Basis-Image mit R und Shiny Server
FROM rocker/shiny:latest


RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libssl-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*


WORKDIR /srv/shiny-server

RUN R -e "install.packages('pak', repos='https://cloud.r-project.org/')"

RUN R -e "pak::pkg_install(c( \
    'shiny', \
    'shinydashboard', \
    'ggplot2', \
    'data.table', \
    'gt', \
    'broom', \
    'magrittr', \
    'ggdark', \
    'DT', \
    'htmltools', \
    'bslib',\
    'shinythemes',\
    'fresh' \
))"

COPY app.R /srv/shiny-server/

RUN chmod -R 755 /srv/shiny-server

EXPOSE 8080

RUN chmod -R 755 /srv/shiny-server

CMD ["/usr/bin/shiny-server"]

