FROM rocker/rstudio:latest-daily


RUN apt-get clean all && \
    apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y \
        libxml2-dev \
    && apt-get clean all && \
    apt-get purge && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ENV RENV_VERSION 0.15.1
ENV RENV_PATHS_CACHE /home/rstudio/.cache/R/renv
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"

RUN Rscript -e "install.packages(c('here', 'tidyverse', 'BiocManager', 'remotes', 'shiny', 'igraph', 'bipartite', 'ggraph'), repos = c(CRAN = 'https://cloud.r-project.org'), lib = '/usr/local/lib/R/site-library');"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

WORKDIR /home/rstudio
RUN R -e "renv::restore()"

RUN chown -R rstudio:rstudio /home/rstudio/