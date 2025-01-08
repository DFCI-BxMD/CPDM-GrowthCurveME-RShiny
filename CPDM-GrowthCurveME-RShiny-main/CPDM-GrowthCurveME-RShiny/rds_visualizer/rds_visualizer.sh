#!/bin/bash
# rds_vis 0.0.1
set -eux
main() {
 # get rds_vis app code
 mkdir rds_vizualizer
 url=https://raw.githubusercontent.com/rkafrawi/RDS_Vis_v1_1/main/Webapp_source
 wget -P rds_visualizer/ $url/DESCRIPTION $url/server.R $url/ui.R
 
 # pull and run Shiny Server docker image
#  dx download $DX_PROJECT_CONTEXT_ID:/rk_shiny/rds_vis_maria.tar.gz
#  docker load -i rds_vis_maria.tar.gz
 
 # attach our rds_vis app's folder as a volume
 docker run --rm -p 443:3838 -v $PWD/rds_visualizer:/srv/shiny-server/ tariship/rds_vis_tarishi
}
