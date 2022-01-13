library(shiny)
library(xlsx)
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(viridis)
library(zoo)
library(lubridate)
library(shinyFiles)
library(htmltools)


theme_set(theme_bw(10))
theme_update(panel.grid.major=element_line(colour="#b2b2b2", size=0.5),
             panel.grid.minor=element_line(colour="#c5c5c5", size=0.5),
             legend.title=element_text(size=18),
             axis.title.x=element_text(size=20),
             axis.title.y=element_text(size=20,angle=90,vjust=1.5),
             axis.text.x=element_text(size=16),
             axis.text.y=element_text(size=16),
             legend.text=element_text(size=16),
             plot.title=element_text(size=25, face="bold",vjust=0.5),
             strip.text.x=element_text(size=14,face="bold"),
             strip.text.y=element_text(size=14,face="bold"))


wd <- "S:\\130-LITEN\\130.7-DTNM\\130.7.87-SA3D-LFM\\130.7.87.02-Permanents\\1_Equipe\\M.Boidot\\Informatique\\Projets\\2021 - WIP - DMA analysis - Python\\test_files"
setwd(wd)

data <- read.csv2("Fatigue_L2Z_A_152_3h_06_40%.csv",header = T)
meta <- data[,c(1:3)] 
data2 <- data[,c(12:41)]

colnames(data2)


p1 <- ggplot(data2,aes(Cycles.1, Strain.p.p.1))
p1 <- p1+geom_point()
p1
