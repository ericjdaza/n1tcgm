# ----- template.R.v170216r ----- #
rm(list=ls())

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
# 181113t --- Produced output and visualizations for Kate's
#   use in her 20 Nov 2018 job talk.
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
library(dplyr)
library(magrittr)



### Set parameters.
global_alpha <- 0.6



### Random seeds.
scalar.seed <- 1711061706



### Path and variable names: Define.
programpath <- "Dropbox (Personal)/Professional/My Documents/academic/postdocs/SPRC/Projects/n1ci/n1t.cgm"
# ericname <- "edaza"
ericname <- "ericjdaza"
path.ejdtools <- paste0("/Users/", ericname, "/Dropbox (Personal)/Professional/My Documents/Programming/R/packages/ejdtools/")
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
df_qualtrics <- openxlsx::read.xlsx(
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
df_qualtrics_wac <- df_qualtrics[which(df_qualtrics$`Are.you.Kate.or.Eric?`==1),]


## Participant: Eric J. Daza.
df_qualtrics_daza <- df_qualtrics[which(df_qualtrics$`Are.you.Kate.or.Eric?`==2),]



### Continuous glucose monitoring device (CGM; i.e., FreeStyle Libre) data.


## Participant: Katarzyna Wac.
df_cgm_wac <- read.xlsx(
  xlsxFile = paste0(
    path.sourceinfo,
    "wac_k/FreestyleLibre_Glucose.xlsx"
  ),
  colNames = FALSE,
  detectDates = TRUE,
  skipEmptyRows = FALSE,
  skipEmptyCols = FALSE
)
colnames(df_cgm_wac) <- c(
  "datetime",
  "bg.level",
  "unknown",
  "sensor.scan"
)
df_cgm_wac$unknown <- NULL
df_cgm_wac$datetime <- as.POSIXct((df_cgm_wac$datetime)*86400, tz="GMT", origin="1904-01-01")
df_cgm_wac$timediff.minutes <- (c(NA, diff(df_cgm_wac$datetime)))/60


## Participant: Eric J. Daza.



### Smartwatch (e.g., Samsung Gear Fit2, Fitbit) data.


## Participant: Katarzyna Wac.


## Participant: Eric J. Daza.



### Combine data.


## Participant: Katarzyna Wac.
tbl_wac <- dplyr::as_tibble(df_cgm_wac) %>%
  dplyr::full_join(
    dplyr::as_tibble(df_qualtrics_wac) %>%
      dplyr::rename(datetime = Recorded.Date),
    by = "datetime"
  ) %>%
  dplyr::arrange(datetime) %>%
  dplyr::mutate(
    treatment = ifelse(
      strftime(
        x = datetime,
        format = "%m-%d",
        tz="GMT",
        origin="1899-12-30"
      ) %in%
        c(
          "07-30",
          "08-01"
        ),
      "pre",
      ifelse(
        strftime(
          x = datetime,
          format = "%m-%d",
          tz="GMT",
          origin="1899-12-30"
        ) == "08-26",
        "post",
        ifelse(
          strftime(
            x = datetime,
            format = "%m-%d",
            tz="GMT",
            origin="1899-12-30"
          ) %in%
            c(
              "08-02",
              "08-03",
              "08-04",
              "08-11",
              "08-12",
              "08-13",
              "08-17",
              "08-18",
              "08-19",
              "08-23",
              "08-24",
              "08-25"
            ),
          "B",
          "A"
        )
      )
    )
  ) %>%
  dplyr::rename(dv_craving = `Craving.is.an.intense.desire.to.consume.a.particular.food.or.food.type.that.is.difficulty.to.resist..Do.you.feel.craving.for.a.food.or.food.type.right.now?.(If.you.just.ate,.answer.the.question.for.did.you.feel.craving.before.you.ate.)`)
tbl_wac$period <- NA
tbl_wac$period[tbl_wac$treatment == "pre"] <- 0
tbl_wac$period[
  strftime(
    x = tbl_wac$datetime,
    format = "%m-%d",
    tz="GMT",
    origin="1899-12-30"
  ) %in%
    c(
      "08-02",
      "08-03",
      "08-04"
    )
  ] <- 1
tbl_wac$period[
  strftime(
    x = tbl_wac$datetime,
    format = "%m-%d",
    tz="GMT",
    origin="1899-12-30"
  ) %in%
    c(
      "08-05",
      "08-06",
      "08-07"
    )
] <- 2
tbl_wac$period[
  strftime(
    x = tbl_wac$datetime,
    format = "%m-%d",
    tz="GMT",
    origin="1899-12-30"
  ) %in%
    c(
      "08-08",
      "08-09",
      "08-10"
    )
  ] <- 3
tbl_wac$period[
  strftime(
    x = tbl_wac$datetime,
    format = "%m-%d",
    tz="GMT",
    origin="1899-12-30"
  ) %in%
    c(
      "08-11",
      "08-12",
      "08-13"
    )
  ] <- 4
tbl_wac$period[
  strftime(
    x = tbl_wac$datetime,
    format = "%m-%d",
    tz="GMT",
    origin="1899-12-30"
  ) %in%
    c(
      "08-14",
      "08-15",
      "08-16"
    )
  ] <- 5
tbl_wac$period[
  strftime(
    x = tbl_wac$datetime,
    format = "%m-%d",
    tz="GMT",
    origin="1899-12-30"
  ) %in%
    c(
      "08-17",
      "08-18",
      "08-19"
    )
  ] <- 6
tbl_wac$period[
  strftime(
    x = tbl_wac$datetime,
    format = "%m-%d",
    tz="GMT",
    origin="1899-12-30"
  ) %in%
    c(
      "08-20",
      "08-21",
      "08-22"
    )
  ] <- 7
tbl_wac$period[
  strftime(
    x = tbl_wac$datetime,
    format = "%m-%d",
    tz="GMT",
    origin="1899-12-30"
  ) %in%
    c(
      "08-23",
      "08-24",
      "08-25"
    )
  ] <- 8
tbl_wac$period[tbl_wac$treatment == "post"] <- 999
tbl_wac <- tbl_wac %>%
  dplyr::group_by(period) %>%
  dplyr::mutate(timepoint_index = row_number())
tbl_wac <- tbl_wac %>%
  dplyr::filter(timepoint_index == 1) %>%
  dplyr::select(
    period,
    datetime
  ) %>%
  dplyr::rename(datetime_firstinperiod = datetime) %>%
  dplyr::right_join(
    tbl_wac,
    by = "period"
  ) %>%
  dplyr::mutate(
    timepoint = as.numeric(datetime - datetime_firstinperiod)
  )
tbl_wac_studyperiod <- tbl_wac %>%
  dplyr::filter(treatment %in% c("A", "B"))


## Participant: Eric J. Daza.



# # Save image.
# save.image(paste0(path.output, "<filename>.RData"))

# # Load image.
# load(paste0(path.output, "<filename>.RData"))
# ericname <- "edaza"
# # ericname <- "ericjdaza"
# path.ejdtools <- paste0("/Users/", ericname, "/Dropbox (Personal)/Professional/My Documents/Programming/R/packages/ejdtools/")
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


## Scatterplots.
xlim_touse <- c(
  min(tbl_wac_studyperiod$timepoint),
  max(tbl_wac_studyperiod$timepoint)
)
ylim_touse <- c(
  min(log(tbl_wac_studyperiod$bg.level), na.rm = TRUE),
  max(log(tbl_wac_studyperiod$bg.level), na.rm = TRUE)
)
tbl_wac_forplot <- tbl_wac_studyperiod %>%
  dplyr::mutate(
    dv_craving_scaled = ifelse(
      dv_craving == 1,
      ylim_touse[1],
      ifelse(
        dv_craving == 2,
        ylim_touse[2],
        NA
      )
    )
  )
set.seed(scalar.seed)
ggp_scatter <- ggplot2::ggplot(
  data = tbl_wac_forplot,
  aes(
    x = timepoint,
    y = log(bg.level)
  )
) +
  ggplot2::theme_classic() +
  ggplot2::geom_line(
    aes(
      group = period,
      color = treatment
    ),
    alpha = global_alpha
  ) +
  ggplot2::geom_jitter(
    aes(
      x = timepoint,
      y = dv_craving_scaled,
      # group = period,
      color = treatment
    ),
    width = 0.11,
    height = 0.22,
    size = 5,
    alpha = global_alpha
  ) +
  ggplot2::xlim(xlim_touse)
ggp_scatter


## Jitter plot.
set.seed(scalar.seed)
ggp_jitter <- ggplot2::ggplot(
  data = tbl_wac_forplot,
  aes(
    x = treatment,
    y = dv_craving_scaled
  )
) +
  ggplot2::theme_classic() +
  ggplot2::geom_jitter(
    aes(color = treatment),
    width = 0.22,
    height = 0.22,
    size = 5,
    alpha = global_alpha
  )
ggp_jitter







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