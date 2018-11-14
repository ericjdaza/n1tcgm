# ----- template.R.v170216r ----- #

# ========================== #
# ========================== #
# ===== Documentation. ===== #
# ========================== #
# ========================== #
#
# Project: N-of-1 trial: Sleep deprivation, carb cravings, glucose,
#   and physical activity.
#
# Description:
#
# 171106m [Daza_EJ] --- Created program.
# 171107t [Daza_EJ] --- Updated while working on manuscript.
#







# ========================================================= #
# ========================================================= #
# ===== Preliminary code (e.g., packages, libraries). ===== #
# ========================================================= #
# ========================================================= #



### Clear environment.
rm(list=ls())



### Packages to install.
# # install.packages("openxlsx")
# # install.packages("ggplot2")
library(openxlsx)
library(ggplot2)



### Random seeds.
scalar.seed <- 1711061706



### Path and variable names: Define.
programpath <- "Box Sync/My Documents/academic/postdocs/SPRC/Projects/n1ci/n1t.cgm"
ericname <- "edaza"
# ericname <- "ericjdaza"
path.ejdtools <- paste0("/Users/", ericname, "/Box Sync/My Documents/Programming/R/packages/ejdtools/")
path.wd <- paste0("/Users/", ericname, "/", programpath, "/programming")
path.sourceinfo <- paste0(path.wd, "/sourceinfo/")
path.output <- paste0(path.wd, "/output/R/p171106m/")
path.programs <- paste0(path.wd, "/programs/R/p171106m/")



### External user-defined functions.

# Define function to construct variable-characteristics codebook.
# if(!exists("foo", mode="function")) source(paste0(path.ejdtools, "ejdtools_charvar.R"))







# ============================ #
# ============================ #
# ===== Data Management. ===== #
# ============================ #
# ============================ #





##### Path and variable names: Define.





##### Raw datasets: Read, import, load, etc.



### Questionnaire survey (i.e., Qualtrics) data.
df_qualtrics <- read.xlsx(
  xlsxFile = paste0(
    path.sourceinfo,
    "N of 1 daily cravings and mood_September 25, 2017_11.39_ejdedits.xlsx"
  ),
  colNames = TRUE,
  detectDates = TRUE,
  skipEmptyRows = FALSE,
  skipEmptyCols = FALSE
)
df_qualtrics$Start.Date <- as.POSIXct((df_qualtrics$Start.Date)*86400, tz="GMT", origin="1899-12-30")
df_qualtrics$End.Date <- as.POSIXct((df_qualtrics$End.Date)*86400, tz="GMT", origin="1899-12-30")
df_qualtrics$Recorded.Date <- as.POSIXct((df_qualtrics$Recorded.Date)*86400, tz="GMT", origin="1899-12-30")


## Participant: Katarzyna Wac.
df_qualtrics.wac <- df_qualtrics[which(df_qualtrics$`Are.you.Kate.or.Eric?`==1),]


## Participant: Eric J. Daza.
df_qualtrics.daza <- df_qualtrics[which(df_qualtrics$`Are.you.Kate.or.Eric?`==2),]



### Continuous glucose monitoring device (CGM; i.e., FreeStyle Libre) data.


## Participant: Katarzyna Wac.
df_cgm.wac <- read.xlsx(
  xlsxFile = paste0(
    path.sourceinfo,
    "wac_k/FreestyleLibre_Glucose.xlsx"
  ),
  colNames = FALSE,
  detectDates = TRUE,
  skipEmptyRows = FALSE,
  skipEmptyCols = FALSE
)
colnames(df_cgm.wac) <- c(
  "datetime",
  "bg.level",
  "unknown",
  "sensor.scan"
)
df_cgm.wac$unknown <- NULL
df_cgm.wac$datetime <- as.POSIXct((df_cgm.wac$datetime)*86400, tz="GMT", origin="1904-01-01")
df_cgm.wac$timediff.minutes <- (c(NA, diff(df_cgm.wac$datetime)))/60


## Participant: Eric J. Daza.



### Smartwatch (e.g., Samsung Gear Fit2, Fitbit) data.


## Participant: Katarzyna Wac.


## Participant: Eric J. Daza.



# # Save image.
# save.image(paste0(path.output, "<filename>.RData"))

# # Load image.
# load(paste0(path.output, "<filename>.RData"))
# ericname <- "edaza"
# # ericname <- "ericjdaza"
# path.ejdtools <- paste0("/Users/", ericname, "/Box Sync/My Documents/Programming/R/packages/ejdtools/")
# path.wd <- paste0("/Users/", ericname, "/", programpath, "/programming")
# path.sourceinfo <- paste0(path.wd, "/sourceinfo/")
# path.output <- paste0(path.wd, "/output/R/p171106m/")
# path.programs <- paste0(path.wd, "/programs/R/p171106m/")





##### Analysis datasets: Construct, export, etc.



### Treatment data: Create from assigned treatments.


## Participant: Katarzyna Wac.
datetime.min.wac <- min()
  

## Participant: Eric J. Daza.







# ============================ #
# ============================ #
# ===== Data Generation. ===== #
# ============================ #
# ============================ #





##### Path and variable names: Define.





##### Set parameters and generate data.







# ===================== #
# ===================== #
# ===== Analyses. ===== #
# ===================== #
# ===================== #

xlim.touse <- c(
  min(df_cgm.wac$datetime, df_qualtrics.wac$Recorded.Date),
  max(df_cgm.wac$datetime, df_qualtrics.wac$Recorded.Date)
)
plot(
  x = df_cgm.wac$datetime,
  y = df_cgm.wac$bg.level,
  type = "l",
  xlim = xlim.touse
)
par(new=TRUE)
plot(
  x = df_qualtrics.wac$Recorded.Date,
  y = df_qualtrics.wac$`How.would.you.rate.your.sleep.quality.overall?`,
  type = "p",
  xlim = xlim.touse,
  col = "red"
)

plot(
  x = df_qualtrics.wac$Recorded.Date,
  y = df_qualtrics.wac$`How.would.you.rate.your.sleep.quality.overall?`
)









# ====================================================================== #
# ====================================================================== #
# ===== Miscellaneous (e.g., code, help files/websites, examples). ===== #
# ====================================================================== #
# ====================================================================== #



# ### Date conversion from MSExcel: https://www.r-bloggers.com/date-formats-in-r/
# 
# # from Windows Excel:
# dates <- c(30829, 38540)
# betterDates <- as.Date(dates,
#                        origin = "1899-12-30")
# 
# >   betterDates
# [1] "1984-05-27" "2005-07-07"
# 
# # from Mac Excel:
# dates <- c(29367, 37078)
# betterDates <- as.Date(dates,
#                        origin = "1904-01-01")