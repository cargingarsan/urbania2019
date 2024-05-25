#usethis::create_from_github(
#  "https://github.com/cargingarsan/urbania2019.git",
#  destdir = ("E:/proyectos/rstudio_git/ml")
#)



library(archive)
library(tidyverse)
library(DataExplorer)
library(psych)
library(leaflet)
library(collapse)


lima_urb <- read_csv(archive_read("E:/proyectos/rstudio_git/ml/urbania2019/data/archive.zip",
                         file=1), col_types = cols())
           

# elimino las filas de indices

lima_urb$`Unnamed: 0` <- NULL
lima_urb$...1 <- NULL


head(lima_urb)
str(lima_urb)


skimr::skim(lima_urb)
DataExplorer::create_report(lima_urb)
correlacion <- round(cor(lima_urb), 1)


