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
# 190126s --- Added in PANAS scoring.
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
# path_ejdtools <- paste0("/Users/ericjdaza/Dropbox (Personal)/Professional/My Documents/Programming/R/packages/ejdtools/")
path_wd <- "/Users/ericjdaza/Documents/GitHub/casia/n1tcgm/"
path_R <- paste0(path_wd, "R/")
path_data_source <- paste0(path_wd, "data/source/")
path_data_output <- paste0(path_wd, "data/output/")



### External user-defined functions.

# Define function to construct variable-characteristics codebook.
# if(!exists("foo", mode="function")) source(paste0(path_ejdtools, "ejdtools_charvar.R"))







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
    path_data_source,
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

# Construct PANAS (Positive Affect / Negative Affect Score).
for (name in names(df_qualtrics)) {
  if (
    regexpr(
      pattern = "Below.is.a.list.of.words.that.describe.feelings.that.people.have..Please.read.each.word.carefully..Then.click.the.circle.that.best.describes.how.you.feel.right.now..-.",
      text = names(df_qualtrics)[which(names(df_qualtrics) == name)]
    ) == 1
  ) names(df_qualtrics)[which(names(df_qualtrics) == name)] <- gsub(
    pattern = "Below.is.a.list.of.words.that.describe.feelings.that.people.have..Please.read.each.word.carefully..Then.click.the.circle.that.best.describes.how.you.feel.right.now..-.",
    replacement = "",
    x = names(df_qualtrics)[which(names(df_qualtrics) == name)]
  )
  
}
rm(name)
names(df_qualtrics)
df_qualtrics <- df_qualtrics %>%
  dplyr::mutate(
    posaff_score = Interested +
      Excited +
      Strong +
      Enthusiastic +
      Proud +
      Alert +
      Inspired +
      Determined +
      Attentive +
      Active,
    negaff_score = Distressed +
      Upset +
      Guilty +
      Scared +
      Hostile +
      Irritable +
      Ashamed +
      Nervous +
      Jittery +
      Afraid
  )
summary(df_qualtrics$posaff_score)
summary(df_qualtrics$negaff_score)


## Participant: Katarzyna Wac.
df_qualtrics_wac <- df_qualtrics[which(df_qualtrics$`Are.you.Kate.or.Eric?`==1),]


## Participant: Eric J. Daza.
df_qualtrics_daza <- df_qualtrics[which(df_qualtrics$`Are.you.Kate.or.Eric?`==2),]



### Continuous glucose monitoring device (CGM; i.e., FreeStyle Libre) data.


## Participant: Katarzyna Wac.
df_cgm_wac <- read.xlsx(
  xlsxFile = paste0(
    path_data_source,
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
    monthday = strftime(
      x = datetime,
      format = "%m-%d",
      tz="GMT",
      origin="1899-12-30"
    ),
    treatment = ifelse(
      monthday %in% c(
        "07-30",
        "08-01"
      ),
      "pre",
      ifelse(
        monthday == "08-26",
        "post",
        ifelse(
          monthday %in%
            c(
              "08-02", "08-03", "08-04",
              "08-11", "08-12", "08-13",
              "08-17", "08-18", "08-19",
              "08-23", "08-24", "08-25"
            ),
          "B",
          "A"
        )
      )
    )
  ) %>%
  dplyr::rename(dv_craving = `Craving.is.an.intense.desire.to.consume.a.particular.food.or.food.type.that.is.difficulty.to.resist..Do.you.feel.craving.for.a.food.or.food.type.right.now?.(If.you.just.ate,.answer.the.question.for.did.you.feel.craving.before.you.ate.)`)
tbl_wac$period <- NA
for (i in 1:(nrow(tbl_wac))) {
  
  if (tbl_wac$treatment == "pre") tbl_wac$period <- 0
  if (tbl_wac$monthday %in% c("08-02", "08-03", "08-04")) tbl_wac$period <- 1
  if (tbl_wac$monthday %in% c("08-05", "08-06", "08-07")) tbl_wac$period <- 2
  if (tbl_wac$monthday %in% c("08-08", "08-09", "08-10")) tbl_wac$period <- 3
  if (tbl_wac$monthday %in% c("08-11", "08-12", "08-13")) tbl_wac$period <- 4
  if (tbl_wac$monthday %in% c("08-14", "08-15", "08-16")) tbl_wac$period <- 5
  if (tbl_wac$monthday %in% c("08-17", "08-18", "08-19")) tbl_wac$period <- 6
  if (tbl_wac$monthday %in% c("08-20", "08-21", "08-22")) tbl_wac$period <- 7
  if (tbl_wac$monthday %in% c("08-23", "08-24", "08-25")) tbl_wac$period <- 8
  if (tbl_wac$treatment == "post") tbl_wac$period <- 999
  
}
rm(i)
tbl_wac <- tbl_wac %>%
  dplyr::group_by(period) %>%
  dplyr::mutate(timepoint_index = row_number()) %>%
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
# save.image(paste0(path_data_output, "<filename>.RData"))

# # Load image.
# load(paste0(path_data_output, "<filename>.RData"))





##### Analysis datasets: Construct, export, etc.



### Treatment data: Create from assigned treatments.


# ## Participant: Katarzyna Wac.
# datetime.min.wac <- min()


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





##### Tables.

## BGL: We have enough data to model and test with CAPTEuR.





##### Figures.
xlim_touse <- c(
  min(tbl_wac_studyperiod$timepoint),
  max(tbl_wac_studyperiod$timepoint)
)


## Blood glucose level.
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
ggp_bglevel <- ggplot2::ggplot(
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
  ggplot2::geom_point(
    aes(
      x = timepoint,
      y = dv_craving_scaled,
      # group = period,
      color = treatment
    ),
    size = 5,
    alpha = global_alpha
  ) +
  ggplot2::xlim(xlim_touse)
ggp_bglevel


## Craving.
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


## PANAS.
ylim_touse <- c(0, 50)
tbl_wac_forplot_panas <- tbl_wac_studyperiod %>%
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
ggp_panas <- ggplot2::ggplot(
  data = tbl_wac_forplot_panas,
  aes(
    x = timepoint,
    y = posaff_score
  )
) +
  ggplot2::theme_classic() +
  ggplot2::geom_point(
    aes(
      group = period,
      color = treatment
    ),
    size = 3,
    alpha = global_alpha
  ) +
  ggplot2::geom_point(
    aes(
      y = negaff_score,
      group = period,
      color = treatment
    ),
    alpha = global_alpha
  ) +
  ggplot2::geom_point(
    aes(
      x = timepoint,
      y = dv_craving_scaled,
      # group = period,
      color = treatment
    ),
    size = 5,
    alpha = global_alpha
  ) +
  ggplot2::xlim(xlim_touse)
ggp_panas







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
