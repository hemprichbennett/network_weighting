FROM rocker/rstudio:latest-daily


RUN apt-get clean all && \
    apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y \
        libxml2-dev \
    && apt-get clean all && \
    apt-get purge && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*


RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"



WORKDIR /home/rstudio
RUN Rscript -e "install.packages(c('here', 'tidyverse', 'BiocManager', 'remotes', 'methods', 'utils', 'Rcpp', 'shiny', 'igraph', 'bipartite', 'ggraph', 'markdown'), repos = c(CRAN = 'https://cloud.r-project.org'), lib = '/usr/local/lib/R/site-library');"

RUN chown -R rstudio:rstudio /home/rstudio/