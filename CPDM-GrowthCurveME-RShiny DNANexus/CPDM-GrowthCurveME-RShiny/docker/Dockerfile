# Use the official R image as the base image
FROM rocker/shiny

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
    cmake \
    git

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinythemes', 'readxl', 'data.table', 'flextable', 'ggplot2', 'readr', 'viridis', 'htmltools', 'DT', 'officer', 'ggtree'))"

# Install GrowthCurveME packages
RUN R -e "install.packages('GrowthCurveME')"