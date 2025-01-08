<!-- dx-header -->
# rds_visualizer (DNAnexus Platform App)

This applet is a Seurat object visualizing web app. Accepts file uploads in .rds format and renders a series of user-query guided visualizations including Feature Plots, Dim Plots, Violin plots.

This is the source code for the used to build and deploy the RDS Visualizer applet that runs on the DNAnexus Platform.
This `Readme.md` file is based on the DNAnexus documentation pertaining to running RStudio Shiny Server & Apps with modifications made to reflect the workflow I used to build and deploy the web app. For the official documentation on running RStudio Shiny Server & Apps, see
[https://documentation.dnanexus.com/](https://documentation.dnanexus.com/getting-started/developer-tutorials/web-app-let-tutorials/running-rstudio-shiny-server-and-apps).
<!-- /dx-header -->

## Instructions for Building the Web App and Deploying on DNAnexus Cloud Platform

I will be describing the step-by-step approach I took in order to build and deploy the RShiny web app to DNAnexus and highlighting potential pitfalls or rabbit holes future developers might run into when they try to deploy their own RShiny web app on DNAnexus. 

### 1. Logging in and running dx-app-wizard

Log into the DNAnexus platform using a token. Tokens can be generated under the user profile/API Tokens. Upon successful login, you will be prompted to select an existing project. 

```
dx login --token <token>
# Enter your username, token
# Select the project where you want to deploy your app.
```

Note that when creating a token, please ensure the token scope is set to all projects as seen below. Otherwise, you will only have VIEW level permissions when selecting a project.

![token](https://github.com/rkafrawi/RDS_Vis_v1_1/blob/main/docs/token.png)


### 2. Start the app wizard to create a new app

The wizard will ask you questions to finalize the app's configuration.

```
dx-app-wizard

# Example answers:

App Name: rds_visualizer
Title []: RDS Visualizer
Summary []: RDS Visualizing RShiny app on DNAnexus
Version [0.0.1]:    # click <ENTER>
1st input name (<ENTER> to finish):    # click <ENTER>
1st output name (<ENTER> to finish):    # click <ENTER>
Timeout policy [48h]:    # click <ENTER>
Programming language: bash
Will this app need access to the Internet? [y/N]: y
Will this app need access to the parent project? [y/N]: y
Choose an instance type for your app [mem1_ssd1_v2_x4]:  # click <ENTER>

```

This results in a directory in your local system with the following structure:

rds_visualizer <br>
├── dxapp.json  <br>
├── src  <br>
&nbsp;│&emsp;└── rds_visualizer.sh  <br>
├── Readme.developer.md  <br>
├── Readme.md  <br>
├── resources  <br>
└── test

Note that whatever you input as the 'App Name:' parameter in the series of `dx-app-wizard` prompts becomes the name of the top folder in the directory as well as the name of the shell script in the src subfolder. 

### 3. Convert to web app

The [dxapp.json](./dxapp.json) file contains the web app metadata. Within this file, find the following lines:

```
    "version": "0.0.1",
    "inputSpec": [],
```

Modify them to the following to convert the app to a web app:

```
    "version": "0.0.1",
    "httpsApp": {
          "ports": [443],
          "shared_access": "VIEW"
      },
    "inputSpec": [],
```

### 4. Containerization

To build the web app, a docker image containing the required packages needs to be created using a rocker/shiny base. To do so, you can follow the logic of the following [Dockerfile](./Dockerfile):

```
# base image: rocker/verse (with a specific version of R)
#   has R, RStudio, tidyverse, devtools, tex, and publishing-related packages
FROM rocker/shiny

MAINTAINER Rizky Kafrawi <rkafrawi@dfci.harvard.edu>

# Install system dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libgsl-dev \
    libxml2-dev \
    libglpk-dev \
    libcurl4-openssl-dev

RUN R -e "install.packages(c('shiny','shinydashboard','shinydashboardPlus', 'purrr', 'stringr', 'DT', 'shinyalert', 'ggplot2'))" 

RUN R --no-echo --no-restore --no-save -e "install.packages('remotes')"
RUN R --no-echo --no-restore --no-save -e "install.packages('Seurat')"

# Install SeuratDisk
RUN R --no-echo --no-restore --no-save -e "remotes::install_github('mojaveazure/seurat-disk')"

```
Note that the Seurat installation is not done in line with the other required packages. For whatever reason, including Seurat in the following manner: install.packages(c(..., ..., 'Seurat')) will cause all the packages besides for Seurat to be installed. Without Seurat, the web app will not run. For organizational purposes, this Docker file and all associated images/tarball files should be kept in the `rds_visualizer/resources` subfolder.

Using the Dockerfile, you can then run the following commands in the CLI:

```
docker build -t <image_name> . 
docker save <image_name> | gzip > <image_name>.tar.gz
```

After building the tarball, make a new directory within the project on the DNAnexus platform to contain your docker images. Navigate to the new subdirectory and upload your tarball. To do so, enter the following commands into the CLI:

```
dx mkdir <Dir_Name>
dx cd <Dir_Name>
dx upload <image_name>.tar.gz
```

### 5. Create app code

The [rds_visualizer.sh](./rds_visualizer.sh) script makes a directory in the selected DNAnexus project and pulls the source code required to build the web app from this repository.

```
#!/bin/bash
# rds_visualizer 0.0.1
set -eux
main() {
 # get rds_visualizer app code
 mkdir rds_vizualizer
 url=https://raw.githubusercontent.com/rkafrawi/RDS_Vis_v1_1/main/Webapp_source
 wget -P rds_visualizer/ $url/DESCRIPTION $url/server.R $url/ui.R

# pull and run Shiny Server docker image from subfolder
 dx download $DX_PROJECT_CONTEXT_ID:/rk_shiny/rds_vis_maria.tar.gz
 docker load -i rds_vis_maria.tar.gz

 # attach our rds_visualizer app's folder as a volume
 docker run --rm -p 443:3838 -v $PWD/rds_visualizer:/srv/shiny-server/ rds_vis_maria
}
```

This shell script deviates from the one present in the official [documentation](https://documentation.dnanexus.com/getting-started/developer-tutorials/web-app-let-tutorials/running-rstudio-shiny-server-and-apps) by accessing the tarball the user uploaded to their tarball-containing-subdirectory to configure the web app environment instead of using the rocker/shiny base.

Note also that the `DESCRIPTION` file is required since the `DisplayMode` parameter is contained here and if you want to change the display to 'Showcase' mode, you would have to modify it within the  `DESCRIPTION` file.

In addition, I experienced some issues when I tried building the web app using customized names of the server and ui scripts. For example, naming the scripts  `RK_ui` and `RK_Server` caused errors during the build. That being said, I reccomend leaving the names of the server/ui scripts `server.R` and `ui.R` respectively to avoid any unecessary complications. 

### 6. Build and deploy

Now that our docker images, shell scripts, json files and applet source code is all set up, we can actually build and deploy the applet. To do so, navigate to the folder above 'rds_visualizer' on your local machine and execute the following command:

```
dx build -f rds_visualizer
```
A successful build and deploy to the platform will result in the resulting applet ID being printed into the CLI:

```
{"id": "applet-abcdefghijklmnop"}
```

At this point, your project directory will look something like this:

![project](https://github.com/rkafrawi/RDS_Vis_v1_1/blob/main/docs/project.png)

The tarball image you created to build the web app will be contained in the folder of your naming and below it will be your web app.

### 7. Start the app

Click on the app under the selected project to run the web app and click 'next.'

![run_app](https://github.com/rkafrawi/RDS_Vis_v1_1/blob/main/docs/run_app.png)

Then, click on 'Start Analysis' at the top right-hand side of the DNAnexus website.

![launch_app](https://github.com/rkafrawi/RDS_Vis_v1_1/blob/main/docs/launch_app.png)

The following pop-up will have a 'Launch Analysis' button. Click on this, there should be no need to modify the app launch configurations as they were already specified when you initialized the app using `dx-app-wizard`.

![review_launch](https://github.com/rkafrawi/RDS_Vis_v1_1/blob/main/docs/review_launch.png)

After clicking on 'Launch Analysis,' the DNAnexus platform will launch a job named after the web app being ran. Upon completion of the job, a URL will be generated.

![app_job](https://github.com/rkafrawi/RDS_Vis_v1_1/blob/main/docs/app_job.png)

Be sure to wait several minutes after the URL populates before trying to access the link. Otherwise, you will be presented with a "502 Bad Gateway" browser error loading the page. This is normal and occurs when the web server part is ready but the web app is not yet ready to listen to connections. Clicking on the link once the web app is ready will redirect you to the Home Page. For further instructions on how to get started with the web app, please refer to the [user guide](https://github.com/rkafrawi/RDS_Vis_v1_1/blob/main/docs/RDS_Visualizer_user_guide.pdf).

![home](https://github.com/rkafrawi/RDS_Vis_v1_1/blob/main/docs/home.png)


