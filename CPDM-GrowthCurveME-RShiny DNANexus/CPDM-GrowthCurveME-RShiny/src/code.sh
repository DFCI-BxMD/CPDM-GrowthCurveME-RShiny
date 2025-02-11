#!/bin/bash
set -eux
main() {
 
mkdir rds_vizualizer
url=https://raw.githubusercontent.com/rkafrawi/RDS_Vis_v1_1/main/Webapp_source
wget -P rds_visualizer/ $url/DESCRIPTION $url/server.R $url/ui.R 
 
docker load -i growthcurveme-v0.0.1.tar.gz
docker run --rm -p 443:3838 -v $PWD/rds_visualizer:/srv/shiny-server/ growthcurveme-v0.0.1
}
