#!/bin/bash

main() {
set -euxo pipefail

# mount the project via dxFuse
mountpoint=$HOME/project
projName=$DX_PROJECT_CONTEXT_ID
wget https://github.com/dnanexus/dxfuse/releases/download/v1.2.0/dxfuse-linux
chmod +x dxfuse-linux
source environment >& /dev/null
echo "Mounting dxfuse"
mkdir -p "$mountpoint"
sudo -E ./dxfuse-linux -uid $(id -u) -gid $(id -g) -verbose 2 -limitedWrite "$mountpoint" "$projName"
projname=`dx describe $DX_PROJECT_CONTEXT_ID | grep "Name" | awk '{sub(/[^ ]+[ ]+/,"")}1' | sed 's, ,\\ ,g'`
output_folder=`dx describe $DX_JOB_ID --json | jq .folder | tr -d '"'`

vm_output="/home/dnanexus"

if [ "$output_folder" == "/" ];
then
   vm_output="/home/dnanexus/$projname"
   mkdir -p "$vm_output"
else
   parent_dir=$(dirname "$output_folder")
   vm_output="/home/dnanexus/$projname$output_folder"
   mkdir -p "/home/dnanexus/$projname$parent_dir"
   dx download $projName:$output_folder/ -o "/home/dnanexus/$projname$parent_dir" -r -f
fi



chmod -R +777 "$vm_output"

# ...
 #cd "${vm_output}"
 #url=https://github.com/DFCI-BxMD/CPDM-GrowthCurveME-RShiny/tree/main/CPDM-GrowthCurveME-RShiny%20DNANexus/CPDM-GrowthCurveME-RShiny/Webapp_source
 #wget -P cpdm_growthcurve/ $url/DESCRIPTION $url/server.R $url/ui.R
 cd "${vm_output}"
# Using raw URLs instead of the web interface URLs
 wget -P cpdm_growthcurve/ https://raw.githubusercontent.com/DFCI-BxMD/CPDM-GrowthCurveME-RShiny/main/CPDM-GrowthCurveME-RShiny%20DNANexus/CPDM-GrowthCurveME-RShiny/Webapp_source/DESCRIPTION
 wget -P cpdm_growthcurve/ https://raw.githubusercontent.com/DFCI-BxMD/CPDM-GrowthCurveME-RShiny/main/CPDM-GrowthCurveME-RShiny%20DNANexus/CPDM-GrowthCurveME-RShiny/Webapp_source/server.R
 wget -P cpdm_growthcurve/ https://raw.githubusercontent.com/DFCI-BxMD/CPDM-GrowthCurveME-RShiny/main/CPDM-GrowthCurveME-RShiny%20DNANexus/CPDM-GrowthCurveME-RShiny/Webapp_source/ui.R

 
 # pull and run Shiny Server docker image
 dx download project-GxzFbg0090gbkZk651G5J63p:/growthcurvemev0.0.1.tar.gz
 docker load -i growthcurvemev0.0.1.tar.gz
 
 # attach our rds_vis app's folder as a volume
 #docker run --rm -p 443:3838 -v /home/dnanexus/CPDM_GrowthCurveME_RShiny:/srv/shiny-server/ -v /home/dnanexus:/home/dnanexus growthcurveme:v0.0.1
 docker run --rm -p 443:3838 -v /home/dnanexus/CPDM_GrowthCurveME_RShiny/cpdm_growthcurve:/srv/shiny-server/ -v /home/dnanexus:/home/dnanexus growthcurveme:v0.0.1
}
