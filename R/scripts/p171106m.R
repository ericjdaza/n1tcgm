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
library(readr)



### Set parameters.
global_alpha <- 0.5
global_alpha_panas <- 0.8
xaxis_time_unit_in_seconds <- 3600 # hour
# xaxis_time_unit_in_seconds <- 60 # second



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
          "B (Normal Sleep)",
          "A (Sleep-Dep)"
        )
      )
    )
  ) %>%
  dplyr::rename(dv_craving = `Craving.is.an.intense.desire.to.consume.a.particular.food.or.food.type.that.is.difficulty.to.resist..Do.you.feel.craving.for.a.food.or.food.type.right.now?.(If.you.just.ate,.answer.the.question.for.did.you.feel.craving.before.you.ate.)`) %>%
  dplyr::mutate(dv_craving = as.logical(dv_craving - 1))
tbl_wac$period <- NA
for (i in 1:(nrow(tbl_wac))) {
  
  if (tbl_wac$treatment[i] == "pre") tbl_wac$period[i] <- 0
  if (tbl_wac$monthday[i] %in% c("08-02", "08-03", "08-04")) tbl_wac$period[i] <- 1
  if (tbl_wac$monthday[i] %in% c("08-05", "08-06", "08-07")) tbl_wac$period[i] <- 2
  if (tbl_wac$monthday[i] %in% c("08-08", "08-09", "08-10")) tbl_wac$period[i] <- 3
  if (tbl_wac$monthday[i] %in% c("08-11", "08-12", "08-13")) tbl_wac$period[i] <- 4
  if (tbl_wac$monthday[i] %in% c("08-14", "08-15", "08-16")) tbl_wac$period[i] <- 5
  if (tbl_wac$monthday[i] %in% c("08-17", "08-18", "08-19")) tbl_wac$period[i] <- 6
  if (tbl_wac$monthday[i] %in% c("08-20", "08-21", "08-22")) tbl_wac$period[i] <- 7
  if (tbl_wac$monthday[i] %in% c("08-23", "08-24", "08-25")) tbl_wac$period[i] <- 8
  if (tbl_wac$treatment[i] == "post") tbl_wac$period[i] <- 999
  
}
rm(i)

names(tbl_wac)
table(tbl_wac$`What.food.(specific).or.food.type.(e.g..high.salt,.high.fat,.high.sugar).do.you.feel.a.craving.for?.(If.you.just.ate,.answer.the.question.for.which.food.did.you.feel.craving.for).`)

tbl_wac_studyperiod <- tbl_wac %>%
  dplyr::filter(treatment %in% c("A (Sleep-Dep)", "B (Normal Sleep)"))

tbl_wac_studyperiod <- tbl_wac_studyperiod %>%
  dplyr::group_by(period) %>%
  dplyr::arrange(
    period,
    datetime
  ) %>%
  dplyr::mutate(period_timept_index = row_number()) %>%
  dplyr::filter(period_timept_index == 1) %>%
  dplyr::select(
    period,
    datetime
  ) %>%
  dplyr::rename(datetime_firstinperiod = datetime) %>%
  
  dplyr::right_join(
    tbl_wac_studyperiod,
    by = "period"
  ) %>%
  
  dplyr::left_join(
    tbl_wac_studyperiod %>%
      dplyr::distinct(
        period,
        monthday
      ) %>%
      dplyr::arrange(
        period,
        monthday
      ) %>%
      dplyr::mutate(study_day = row_number()),
    by = c(
      "period",
      "monthday"
    )
  ) %>%
  
  dplyr::left_join(
    tbl_wac_studyperiod %>%
      dplyr::distinct(
        period,
        monthday
      ) %>%
      dplyr::arrange(
        period,
        monthday
      ) %>%
      dplyr::group_by(period) %>%
      dplyr::mutate(period_day = row_number()),
    by = c(
      "period",
      "monthday"
    )
  ) %>%
  dplyr::mutate(period_timept = as.numeric((datetime - datetime_firstinperiod + 1) / xaxis_time_unit_in_seconds)) %>%
  dplyr::ungroup(period)

tbl_wac_studyperiod <- tbl_wac_studyperiod %>%
  dplyr::group_by(study_day) %>%
  dplyr::arrange(
    study_day,
    datetime
  ) %>%
  dplyr::mutate(study_day_timept_index = row_number()) %>%
  dplyr::filter(study_day_timept_index == 1) %>%
  dplyr::select(
    study_day,
    datetime
  ) %>%
  dplyr::rename(datetime_firstinstudy_day = datetime) %>%
  
  dplyr::right_join(
    tbl_wac_studyperiod,
    by = "study_day"
  ) %>%

  dplyr::mutate(study_day_timept = as.numeric((datetime - datetime_firstinstudy_day + 1) / xaxis_time_unit_in_seconds)) %>%
  dplyr::ungroup(study_day) %>%

# tbl_wac_studyperiod <- tbl_wac_studyperiod %>%
  dplyr::mutate(
    timept = as.numeric((datetime - min(datetime_firstinperiod) + 1) / xaxis_time_unit_in_seconds),
    log_bg.level = log(bg.level)
  )

tbl_wac_studyperiod_byday <- tbl_wac_studyperiod %>%
  dplyr::group_by(study_day) %>%
  dplyr::mutate(

    mean_bg.level = mean(bg.level, na.rm = TRUE),
    sd_bg.level = sd(bg.level, na.rm = TRUE),
    n_bg.level = sum(!is.na(bg.level)),

    mean_log_bg.level = mean(log_bg.level, na.rm = TRUE),
    sd_log_bg.level = sd(log_bg.level, na.rm = TRUE),
    n_log_bg.level = sum(!is.na(log_bg.level)),

    mean_dv_craving = mean(dv_craving, na.rm = TRUE),
    sd_dv_craving = sd(dv_craving, na.rm = TRUE),
    n_dv_craving = sum(!is.na(dv_craving)),

    mean_posaff_score = mean(posaff_score, na.rm = TRUE),
    sd_posaff_score = sd(posaff_score, na.rm = TRUE),
    n_posaff_score = sum(!is.na(posaff_score)),

    mean_negaff_score = mean(negaff_score, na.rm = TRUE),
    sd_negaff_score = sd(negaff_score, na.rm = TRUE),
    n_negaff_score = sum(!is.na(negaff_score))

  ) %>%
  dplyr::distinct(

    period,
    period_day,
    study_day,
    treatment,

    mean_bg.level,
    sd_bg.level,
    n_bg.level,

    mean_log_bg.level,
    sd_log_bg.level,
    n_log_bg.level,

    mean_dv_craving,
    sd_dv_craving,
    n_dv_craving,

    mean_posaff_score,
    sd_posaff_score,
    n_posaff_score,

    mean_negaff_score,
    sd_negaff_score,
    n_negaff_score

  ) %>%
  dplyr::ungroup(study_day)


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





##### Set plot parameters.

what_to_plot <- "figures"
# what_to_plot <- "tables"
# what_to_plot <- "both"

# num_blockdays_to_output <- 1
num_blockdays_to_output <- 3

# values_to_plot <- "original"
# values_to_plot <- "differences"
values_to_plot <- "both"

# pdfpng <- "pdf"
# height_touse <- 5
# width_touse <- 7
pdfpng <- "png"
height_touse <- 480 * 3
width_touse <- 640 * 3

png_size_multiplier <- 4
size_default <- 1
if (pdfpng == "png") size_default <- png_size_multiplier * size_default

if (values_to_plot == "original") filename_touse_bg <- "fig_bg_original_values_"
if (values_to_plot == "differences") filename_touse_bg <- "fig_bg_differences_in_values_"
if (values_to_plot == "both") filename_touse_bg <- "fig_bg_orig_and_diffin_values_"
filename_touse_bg <- paste0(
  filename_touse_bg,
  num_blockdays_to_output,
  "days.",
  pdfpng
)

filename_touse_craving <- paste0(
  "fig_craving_",
  num_blockdays_to_output,
  "days.csv"
)

filename_touse_panas <- paste0(
  "fig_panas_values_",
  num_blockdays_to_output,
  "days.",
  pdfpng
)

# bg_num_of_partitions <- 100





##### Dataset and variables to use.
tbl_touse <- tbl_wac_studyperiod %>%
  dplyr::rename(
    timeidx_touse = timept,
    bg_touse = bg.level,
    # bg_touse = log_bg.level,
    craving_touse = dv_craving,
    posaff_touse = posaff_score,
    negaff_touse = negaff_score
  )
if (num_blockdays_to_output == 1) tbl_touse <- tbl_touse %>%
  dplyr::rename(spaghetti_timeidx_touse = study_day_timept)
if (num_blockdays_to_output > 1) tbl_touse <- tbl_touse %>%
  dplyr::rename(spaghetti_timeidx_touse = period_timept)
# tbl_touse <- tbl_wac_studyperiod_byday %>%
#   dplyr::rename(
#     timeidx_touse = study_day,
#     spaghetti_timeidx_touse = period_day,
#     # bg_touse = mean_bg.level,
#     bg_touse = mean_log_bg.level,
#     craving_touse = mean_dv_craving,
#     posaff_touse = mean_posaff_score,
#     negaff_touse = mean_negaff_score
#   )






# ##### Tables.
# 
# ## BGL: We have enough data to model and test with CAPTEuR.
# 
# 
# 
# 
# 
# ##### Figures.
# # xlim_touse <- c(
# #   min(tbl_touse$spaghetti_timeidx_touse),
# #   max(tbl_touse$spaghetti_timeidx_touse)
# # )



### Hypothesis 1: Blood glucose level.
### Hypothesis 1 was that sleep deprivation
###   (“sleep-dep”) renders blood sugar control
###   less effective the next day. Specifically,
###   we expected to observe elevated glucose
###   levels for longer intervals of time on
###   average on post-sleep-dep days (i.e., days
###   that each follow a night of sleep-dep)
###   compared with control days (i.e., days that
###   each follow a night of baseline sleep
###   without sleep-dep).


## Spaghetti plot.
if (what_to_plot %in% c("figures", "both")) {
  
  # Make plot table.
  # binsize_pctofspag <- 1 / (bg_num_of_partitions * num_blockdays_to_output)
  tbl_touse_spaghetti <- tbl_touse
  if (num_blockdays_to_output == 1) tbl_touse_spaghetti <- tbl_touse_spaghetti %>%
      dplyr::filter(period_day == 1)
  tbl_touse_spaghetti <- tbl_touse_spaghetti %>%
    dplyr::select(
      period,
      spaghetti_timeidx_touse,
      treatment,
      bg_touse
    ) %>%
    dplyr::mutate(
      # spaghetti_interval = ceiling(
      #   spaghetti_timeidx_touse /
      #     ceiling( ( max(spaghetti_timeidx_touse) - min(spaghetti_timeidx_touse) ) * binsize_pctofspag )
      # ),
      spaghetti_interval = ceiling(spaghetti_timeidx_touse),
      linetype_touse = ifelse(
        treatment == "A (Sleep-Dep)",
        "solid",
        ifelse(
          treatment == "B (Normal Sleep)",
          "dotted",
          NA
        )
      )
    )
  tbl_touse_spaghetti <- tbl_touse_spaghetti %>%
    dplyr::left_join(
      tbl_touse_spaghetti %>%
        dplyr::select(
          treatment,
          spaghetti_interval,
          bg_touse
        ) %>%
        dplyr::group_by(
          treatment,
          spaghetti_interval
        ) %>%
        dplyr::mutate(
          mean_bg_touse = mean(bg_touse, na.rm = TRUE),
          median_bg_touse = median(bg_touse, na.rm = TRUE),
          sd_bg_touse = sd(bg_touse, na.rm = TRUE),
          IQR_bg_touse = IQR(bg_touse, na.rm = TRUE)
        ) %>%
        dplyr::ungroup(
          treatment,
          spaghetti_interval
        ) %>%
        dplyr::distinct(
          treatment,
          spaghetti_interval,
          mean_bg_touse,
          median_bg_touse,
          sd_bg_touse,
          IQR_bg_touse
        ),
      by = c(
        "treatment",
        "spaghetti_interval"
      )
    )
  tbl_touse_spaghetti <- tbl_touse_spaghetti %>%
    dplyr::filter(treatment == "A (Sleep-Dep)") %>%
    dplyr::distinct(
      spaghetti_interval,
      mean_bg_touse,
      median_bg_touse,
      sd_bg_touse,
      IQR_bg_touse
    ) %>%
    dplyr::inner_join(
      tbl_touse_spaghetti %>%
        dplyr::filter(treatment == "B (Normal Sleep)") %>%
        dplyr::distinct(
          spaghetti_interval,
          mean_bg_touse,
          median_bg_touse,
          sd_bg_touse,
          IQR_bg_touse
        ),
      by = "spaghetti_interval"
    ) %>%
    dplyr::mutate(
      diff_mean_bg_touse = mean_bg_touse.x - mean_bg_touse.y,
      diff_median_bg_touse = median_bg_touse.x - median_bg_touse.y,
      diff_sd_bg_touse = sd_bg_touse.x - sd_bg_touse.y,
      diff_IQR_bg_touse = IQR_bg_touse.x - IQR_bg_touse.y
    ) %>%
    dplyr::select(
      spaghetti_interval,
      diff_mean_bg_touse,
      diff_median_bg_touse,
      diff_sd_bg_touse,
      diff_IQR_bg_touse
    ) %>%
    dplyr::right_join(
      tbl_touse_spaghetti,
      by = "spaghetti_interval"
    )
  
  # Make plot.
  ylim_touse_bg <- c(
    0,
    # max(tbl_touse$bg_touse, na.rm = TRUE)
    ceiling(
      max(
        (
          tbl_touse %>%
            dplyr::filter(period_day %in% seq(1, num_blockdays_to_output, 1))
        )$bg_touse,
        na.rm = TRUE
      ) / 10
    ) * 10
  )
  
  if (values_to_plot == "original") {
    
    ggp_bglevel <- ggplot2::ggplot(
      data = tbl_touse_spaghetti,
      aes(
        x = spaghetti_timeidx_touse,
        y = bg_touse
      )
    )
    
    if (xaxis_time_unit_in_seconds == 3600) {
      
      ggp_bglevel <- ggp_bglevel +
        ggplot2::geom_vline(
          xintercept = c(0, seq(from = 6, to = (24 * num_blockdays_to_output), by = 6)),
          color = "gray",
          linetype = "dotted",
          size = size_default * 0.4
        )
      if (num_blockdays_to_output == 1) ggp_bglevel <- ggp_bglevel +
          scale_x_continuous(
            name = "Hour",
            breaks = c(0, seq(from = 2, to = (24 * num_blockdays_to_output), by = 2))
          )
      if (num_blockdays_to_output > 1) ggp_bglevel <- ggp_bglevel +
          scale_x_continuous(
            name = "Hour",
            breaks = c(0, seq(from = 6, to = (24 * num_blockdays_to_output), by = 6))
          )
      
    }
    
    if (xaxis_time_unit_in_seconds == 60) ggp_bglevel <- ggp_bglevel +
      ggplot2::xlab(label = "Second")
    
    ggp_bglevel <- ggp_bglevel +
      ggplot2::geom_line(
        aes(
          group = period,
          color = treatment
          # , linetype = treatment
          # , linetype = linetype_touse
        )
        , size = size_default * 0.5
        , alpha = global_alpha
      ) +
      ggplot2::geom_line(
        aes(
          # y = mean_bg_touse,
          y = median_bg_touse,
          group = treatment,
          color = treatment
          # , linetype = treatment
          # , linetype = linetype_touse
        ),
        size = size_default * 1.5
      ) +
      ggplot2::geom_line(
        aes(
          # y = sd_bg_touse,
          y = IQR_bg_touse,
          group = treatment,
          color = treatment
          # , linetype = treatment
          # , linetype = linetype_touse
        ),
        # linetype = "dotted",
        size = size_default * 0.4
      ) +
      ggplot2::geom_hline(
        yintercept = c(72, 99), # normal range when fasting
        color = "gray",
        size = size_default * 0.2
      ) +
      ggplot2::geom_hline(
        yintercept = 140, # normal upper bound 2 hours after eating
        color = "gray",
        size = size_default * 0.2,
        linetype = "dotted"
      ) +
      # ggplot2::ylim(ylim_touse_bg) +
      ggplot2::scale_y_continuous(
        name = "Blood Glucose (mg/dL)", # bg_touse = bg.level
        # name = "Logged Blood Glucose (log mg/dL)", # bg_touse = log_bg.level
        limits = ylim_touse_bg,
        breaks = seq(0, ylim_touse_bg[2], 10)
      )
      # ggplot2::ylab(label = "Blood Glucose (mg/dL)") # bg_touse = bg.level
    # ggplot2::ylab(label = "Logged Blood Glucose (log mg/dL)") # bg_touse = log_bg.level
    
  }
  
  if (values_to_plot == "differences") {
    
    ggp_bglevel <- ggplot2::ggplot(
      data = tbl_touse_spaghetti %>%
        dplyr::filter(treatment == "A (Sleep-Dep)"), # use this for all diff_ variables,
      aes(
        x = spaghetti_timeidx_touse,
        # y = diff_mean_bg_touse,
        y = diff_median_bg_touse
      )
    )
    
    if (xaxis_time_unit_in_seconds == 3600) {
      
      ggp_bglevel <- ggp_bglevel +
        ggplot2::geom_vline(
          xintercept = c(0, seq(from = 6, to = (24 * num_blockdays_to_output), by = 6)),
          color = "gray",
          linetype = "dotted",
          size = size_default * 0.4
        )
      if (num_blockdays_to_output == 1) ggp_bglevel <- ggp_bglevel +
          scale_x_continuous(
            name = "Hour",
            breaks = c(0, seq(from = 2, to = (24 * num_blockdays_to_output), by = 2))
          )
      if (num_blockdays_to_output > 1) ggp_bglevel <- ggp_bglevel +
          scale_x_continuous(
            name = "Hour",
            breaks = c(0, seq(from = 6, to = (24 * num_blockdays_to_output), by = 6))
          )
      
    }
    
    if (xaxis_time_unit_in_seconds == 60) ggp_bglevel <- ggp_bglevel +
        ggplot2::xlab(label = "Second")
    
    ggp_bglevel <- ggp_bglevel +
      ggplot2::geom_line(size = size_default * 1.5) +
      ggplot2::geom_line(
        data = tbl_touse_spaghetti %>%
          dplyr::filter(treatment == "A (Sleep-Dep)"), # use this for all diff_ variables
        aes(
          # y = diff_sd_bg_touse,
          y = diff_IQR_bg_touse
        ),
        # linetype = "dotted",
        size = size_default * 0.2
      ) +
      ggplot2::ylab(label = "Difference (A-B) in Blood Glucose (mg/dL)") # bg_touse = bg.level
    # ggplot2::ylab(label = "Difference in Logged Blood Glucose (A-B)") # bg_touse = log_bg.level
    
  }
  
  if (values_to_plot == "both") {
    
    ggp_bglevel <- ggplot2::ggplot(
      data = tbl_touse_spaghetti,
      aes(
        x = spaghetti_timeidx_touse,
        y = bg_touse
      )
    )
    
    if (xaxis_time_unit_in_seconds == 3600) {
      
      ggp_bglevel <- ggp_bglevel +
        ggplot2::geom_vline(
          xintercept = c(0, seq(from = 6, to = (24 * num_blockdays_to_output), by = 6)),
          color = "gray",
          linetype = "dotted",
          size = size_default * 0.4
        )
      if (num_blockdays_to_output == 1) ggp_bglevel <- ggp_bglevel +
          scale_x_continuous(
            name = "Hour",
            breaks = c(0, seq(from = 2, to = (24 * num_blockdays_to_output), by = 2))
          )
      if (num_blockdays_to_output > 1) ggp_bglevel <- ggp_bglevel +
          scale_x_continuous(
            name = "Hour",
            breaks = c(0, seq(from = 6, to = (24 * num_blockdays_to_output), by = 6))
          )
      
    }
    
    if (xaxis_time_unit_in_seconds == 60) ggp_bglevel <- ggp_bglevel +
        ggplot2::xlab(label = "Second")
    
    ggp_bglevel <- ggp_bglevel +
      ggplot2::geom_line(
        aes(
          group = period,
          color = treatment
          # , linetype = treatment
          # , linetype = linetype_touse
        )
        , size = size_default * 0.5
        , alpha = global_alpha
      ) +
      ggplot2::geom_line(
        aes(
          # y = mean_bg_touse,
          y = median_bg_touse,
          group = treatment,
          color = treatment
          # , linetype = treatment
          # , linetype = linetype_touse
        ),
        size = size_default * 1.5
      ) +
      ggplot2::geom_line(
        aes(
          # y = sd_bg_touse,
          y = IQR_bg_touse,
          group = treatment,
          color = treatment
          # , linetype = treatment
          # , linetype = linetype_touse
        ),
        # linetype = "dotted",
        size = size_default * 0.4
      ) +
      ggplot2::geom_line(
        data = tbl_touse_spaghetti %>%
          dplyr::filter(treatment == "A (Sleep-Dep)"), # use this for all diff_ variables
        aes(
          # y = diff_mean_bg_touse,
          y = diff_median_bg_touse
        ),
        size = size_default * 1.5
      ) +
      ggplot2::geom_line(
        data = tbl_touse_spaghetti %>%
          dplyr::filter(treatment == "A (Sleep-Dep)"), # use this for all diff_ variables
        aes(
          # y = diff_sd_bg_touse,
          y = diff_IQR_bg_touse
        ),
        # linetype = "dotted",
        size = size_default * 0.3
      ) +
      ggplot2::geom_hline(
        yintercept = c(72, 99), # normal range when fasting
        color = "gray",
        size = size_default * 0.2
      ) +
      ggplot2::geom_hline(
        yintercept = 140, # normal upper bound 2 hours after eating
        color = "gray",
        size = size_default * 0.2,
        linetype = "dotted"
      ) +
      ggplot2::ylab(label = "Blood Glucose (or Difference)") # bg_touse = bg.level
    # ggplot2::ylab(label = "Logged Blood Glucose (or Difference)") # bg_touse = log_bg.level
    
  }
  
  ggp_bglevel <- ggp_bglevel +
    ggplot2::theme_classic() +
    ggplot2::geom_hline(
      yintercept = 0,
      # color = "gray",
      size = size_default * 0.3
    ) +
    scale_color_discrete(name = "Treatment") # http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/

  if (num_blockdays_to_output > 1) ggp_bglevel <- ggp_bglevel +
    ggplot2::geom_vline(
      # xintercept = (
      #   tbl_touse %>%
      #     dplyr::filter(
      #       study_day_timept == 1 / xaxis_time_unit_in_seconds,
      #       period == 1
      #     ) %>%
      #     dplyr::select(timeidx_touse)
      # )$timeidx_touse,
      xintercept = c(0, seq(from = 24, to = (24 * num_blockdays_to_output), by = 24)),
      color = "gray",
      size = size_default * 0.6
    )
  
  ggp_bglevel <- ggp_bglevel +
    ggplot2::theme_classic()
  if (pdfpng == "png") ggp_bglevel <- ggp_bglevel +
    ggplot2::theme_classic(base_size = png_size_multiplier * 11) # default is 11; https://stackoverflow.com/questions/11955229/how-to-change-the-default-font-size-in-ggplot2
  
}

# Export.
# pdf(
#   file = paste0(path_data_output, filename_touse_bg)
#   , height = height_touse
#   , width = width_touse
#   , onefile = TRUE
# )
png(
  file = paste0(path_data_output, filename_touse_bg)
  , height = height_touse
  , width = width_touse
)
ggp_bglevel
dev.off()



### Hypothesis 2: Craving.
### Hypothesis 2 was that sleep-dep physiologically
###   induces higher craving for quick carbohydrates
###   the next day because of the stress response.
###   Specifically, we expected to observe more cravings
###   for quick-energy foods on average on post-sleep-dep
###   days compared with control days.


## Table.
if (what_to_plot %in% c("tables", "both")) {
  
  tbl_craving <- tbl_touse %>%
    dplyr::filter(
      !is.na(craving_touse),
      # period_day <= num_blockdays_to_output
      period_day %in% c(3)
    ) %>%
    dplyr::select(
      period_day,
      treatment,
      craving_touse
    )
  table(
    tbl_craving$craving_touse,
    tbl_craving$treatment
  )
  fisher.test(
    tbl_craving$craving_touse,
    tbl_craving$treatment
  )
  if(num_blockdays_to_output == 1) readr::write_excel_csv(
    x = tbl_craving %>%
      dplyr::group_by(
        period_day,
        treatment,
        craving_touse
      ) %>%
      dplyr::summarize(n()) %>%
      dplyr::mutate(
        fisher_test_pvalue = fisher.test(
          tbl_craving$treatment,
          tbl_craving$craving_touse
        )$p.value
      ),
    path = paste0(
      path_data_output,
      filename_touse_craving
    )
  )
  if(num_blockdays_to_output > 1) readr::write_excel_csv(
    x = tbl_touse %>%
      dplyr::filter(
        !is.na(craving_touse),
        period_day <= num_blockdays_to_output
      ) %>%
      dplyr::group_by(
        period_day,
        treatment,
        craving_touse
      ) %>%
      dplyr::summarize(n()),
    path = paste0(
      path_data_output,
      filename_touse_craving
    )
  )
  
}



### Hypothesis 3: PANAS.
### Hypothesis 3 was that sleep-dep worsens mood
###   the next day. Specifically, we expected to
###   observe more reports of negative mood on
###   average on post-sleep-dep days compared with
###   control days.


## Spaghetti plot.
if (what_to_plot %in% c("figures", "both")) {
  
  ylim_touse_panas <- c(0, 50)
  
  # Make plot table.
  tbl_touse_forplot_panas <- tbl_touse %>%
    dplyr::filter(
      !is.na(posaff_touse),
      !is.na(negaff_touse)
    )
  if (num_blockdays_to_output == 1) tbl_touse_forplot_panas <- tbl_touse_forplot_panas %>%
    dplyr::filter(period_day == 1)
  
  # Make plot.
  
  ggp_panas <- ggplot2::ggplot(
    data = tbl_touse_forplot_panas,
    aes(
      x = spaghetti_timeidx_touse,
      y = posaff_touse
    )
  )

  if (xaxis_time_unit_in_seconds == 3600) {
    
    ggp_panas <- ggp_panas +
      ggplot2::geom_vline(
        xintercept = c(0, seq(from = 6, to = (24 * num_blockdays_to_output), by = 6)),
        color = "gray",
        linetype = "dotted",
        size = size_default * 0.4
      )
    if (num_blockdays_to_output == 1) ggp_panas <- ggp_panas +
        scale_x_continuous(
          name = "Hour",
          breaks = c(0, seq(from = 2, to = (24 * num_blockdays_to_output), by = 2))
        )
    if (num_blockdays_to_output > 1) ggp_panas <- ggp_panas +
        scale_x_continuous(
          name = "Hour",
          breaks = c(0, seq(from = 6, to = (24 * num_blockdays_to_output), by = 6))
        )
    
  }
  
  if (xaxis_time_unit_in_seconds == 60) ggp_panas <- ggp_panas +
    ggplot2::xlab(label = "Second")
  
  ggp_panas <- ggp_panas +
    ggplot2::theme_classic() +
    ggplot2::geom_line(
      aes(
        group = period,
        color = treatment
      )
      , size = size_default
      , alpha = global_alpha_panas
    ) +
    ggplot2::geom_point(
      aes(
        group = period,
        color = treatment
      )
      , size = size_default * 4
      , alpha = global_alpha_panas
    ) +
    ggplot2::geom_line(
      aes(
        y = negaff_touse,
        group = period,
        color = treatment
      )
      , linetype = "dotted"
      , size = size_default * 1.5
      , alpha = global_alpha_panas
    ) +
    ggplot2::geom_point(
      aes(
        y = negaff_touse,
        group = period,
        color = treatment
      )
      , shape = 21
      , size = size_default * 4
      , alpha = global_alpha_panas
    ) +
    ggplot2::ylim(ylim_touse_panas) +
    ggplot2::geom_hline(
      yintercept = 0,
      # color = "gray",
      size = size_default * 0.3
    ) +
    scale_color_discrete(name = "Treatment") + # http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
    ggplot2::ylab(label = "Positive and Negative Affect Scale")
  
  if (num_blockdays_to_output > 1) ggp_panas <- ggp_panas +
    ggplot2::geom_vline(
      # xintercept = (
      #   tbl_touse %>%
      #     dplyr::filter(
      #       study_day_timept == 1 / xaxis_time_unit_in_seconds,
      #       period == 1
      #     ) %>%
      #     dplyr::select(timeidx_touse)
      # )$timeidx_touse,
      xintercept = c(0, seq(from = 24, to = (24 * num_blockdays_to_output), by = 24)),
      color = "gray",
      size = size_default * 0.6
    )
  
  ggp_panas <- ggp_panas +
    ggplot2::theme_classic()
  if (pdfpng == "png") ggp_panas <- ggp_panas +
    ggplot2::theme_classic(base_size = png_size_multiplier * 11) # default is 11; https://stackoverflow.com/questions/11955229/how-to-change-the-default-font-size-in-ggplot2
  
}

# Export.
# pdf(
#   file = paste0(path_data_output, filename_touse_panas)
#   , height = height_touse
#   , width = width_touse
#   , onefile = TRUE
# )
png(
  file = paste0(path_data_output, filename_touse_panas)
  , height = height_touse
  , width = width_touse
)
ggp_panas
dev.off()







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
