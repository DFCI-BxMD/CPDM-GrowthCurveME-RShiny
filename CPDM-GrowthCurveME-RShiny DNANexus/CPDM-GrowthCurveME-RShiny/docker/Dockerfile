# Use the official R image as the base image
FROM --platform=linux/amd64 rocker/shiny

# Install system dependencies for the R packages
RUN apt-get clean && \
    apt-get update && \
    apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgsl-dev \ 
    libglpk-dev \
    cmake \
    git

# Install R packages
#RUN R -e "install.packages(c('shiny', 'shinythemes', 'readxl', 'data.table', 'flextable', 'ggplot2', 'readr', 'viridis', 'htmltools', 'DT', 'officer', 'ggtree'))"
RUN R -e "install.packages(c('shiny', 'shinythemes', 'readxl', 'data.table', 'flextable', 'ggplot2', 'readr', 'viridis', 'htmltools', 'DT', 'officer', 'tinytex', 'shinycssloaders', 'shinyjs', 'magrittr', 'dplyr'))"
# install ggtree
RUN R -e "install.packages('BiocManager')"
RUN R -e "BiocManager::install('ggtree')"
# Install GrowthCurveME packages
RUN R -e "install.packages('GrowthCurveME')"
