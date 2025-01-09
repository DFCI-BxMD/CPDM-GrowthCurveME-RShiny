#!/bin/bash
# rds_vis 0.0.1
set -eux
main() {
 # get rds_vis app code
 mkdir CPDM_GrowthCurveME_RShiny
 #url=https://raw.githubusercontent.com/rkafrawi/RDS_Vis_v1_1/main/Webapp_source
 url=https://github.com/DFCI-BxMD/CPDM-GrowthCurveME-RShiny/tree/main/CPDM-GrowthCurveME-RShiny-main/CPDM-GrowthCurveME-RShiny/Webapp_source
 wget -P CPDM_GrowthCurveME_RShiny/ $url/DESCRIPTION $url/server.R $url/ui.R
 
 
 # attach our rds_vis app's folder as a volume
 docker run --rm -p 443:3838 -v $PWD/CPDM_GrowthCurveME_RShiny:/srv/shiny-server/ ledia/CPDM_GrowthCurveME_RShiny
}
