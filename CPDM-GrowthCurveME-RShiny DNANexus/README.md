# R shiny on DNAnexus
You can have an [R shiny web app](https://documentation.dnanexus.com/getting-started/developer-tutorials/bash/r-shiny-example-web-app) or an [R shiny server](https://github.com/dnanexus/dnanexus-example-applets/tree/master/Tutorials/webapp/r-shiny-server) set up on DNAnexus. It seems as though the R shiny server functionality hasn't been updated in 4 years, and we should stick to using R shiny web apps. Code for R shiny web apps can be found [here](https://github.com/dnanexus/dnanexus-example-applets/tree/master/Tutorials/bash/r-shiny-web-app).

## R Shiny Web Apps
 
### Steps
* Create shiny-asset with packages needed for the R Shiny Web App to work
* Build and publish your R Shiny Web App
#### shiny-asset
Unlike the R Shiny Server option, here we do not need to create a Dockerfile and docker run the image to get the code to run. This causes issues with uploading files to the R Shiny app to visualize since there is a size restraint. This could be fine for apps where we might not need to upload large files.

We create a shiny-asset which contains all the packages we want to use in our R Shiny Web App. Example code can be found [here](https://github.com/dnanexus/dnanexus-example-applets/blob/master/Tutorials/bash/shiny-asset/dxasset.json). You'll need to build your dxasset.json file. It will return a record-ID which we will then input into the dxapps.json file of the R Shiny Web App so it is able to use the packages.

Example **dxasset.json** file
```
{
    "name": "shiny_asset",
    "title": "Shiny Asset",
    "description": "Libraries for building web apps with R Shiny",
    "version": "0.0.1",
    "distribution": "Ubuntu",
    "release": "20.04",
    "execDepends": [
        {"name": "shiny", "package_manager": "cran"}
    ]
}
```
Building **dxasset.json** file
```
dx build_asset shiny-asset
```

#### Build R Shiny Web App
In the **dxapps.json** file, you'll need to add the following lines
```
{
  "name": "rshiny-web-app",
  "title": "rshiny-web-app",
  "summary": "R Shiny Web App",
  "dxapi": "1.0.0",
  "version": "0.0.1",
  ***"httpsApp"***: {#Tells worker to expose this port
          "ports": [443],
          "shared_access": "VIEW"
      },
  "inputSpec": [],
  "outputSpec": [],
  "runSpec": {
    "timeoutPolicy": {
      "*": {
        "hours": 48
      }
    },
    "interpreter": "bash",
    "file": "src/code.sh",
    "distribution": "Ubuntu",
    "release": "20.04",
    "version": "0",
    ***"assetDepends"***: [
    {
      "id": "record-xxxx # This you get from running: dx build_asset shiny-asset 
    }
  ]
  },
  "access": {
    "network": [
      "*"
    ],
    "project": "CONTRIBUTE"
  },
  "regionalOptions": {
    "aws:us-east-1": {
      "systemRequirements": {
        "*": {
          "instanceType": "mem1_ssd1_v2_x4"
        }
      }
    }
  }
}
```
Add your **server.R** and **ui.R** scripts under
```
resources/home/dnanexus/my_app/
```

Inside the **src/code.sh** file, just run the following
```
#!/bin/bash
set -e -x -o pipefail

main() {
    R -e "shiny::runApp('~/my_app', host='0.0.0.0', port=443)"
}
```

Build **dxapps.json** file
```
dx build -f rshiny-web-app
```
