
# source: https://github.com/zmcucunuba/R_estimates

#' @export R_EstimateIL
#' @export plot_R_estimates

R_EstimateIL <- function (dat,
                          si_mean_days,
                          si_sd_days,
                          window_units,
                          t_window,
                          week_start,
                          year_start,
                          R_prior,
                          R_sd_prior)
{




  date_start = week_start + 1
  t_start  <- which (dat$week == date_start & dat$year == year_start)
  t_end     <- nrow(dat)- t_window

  if (window_units == 'weeks')  {unitm = 7}
  if (window_units == 'months') {unitm = 30}
  if (window_units == 'days')   {unitm = 1}

  dat$row  <- 1:length(dat$total_cases)
  dat$date_calc <- aweek::get_date(week = dat$week, year = dat$year, day = 7, start = "Sunday")


  data <- dat$total_cases
  incid <- incidence(data)
  incid$dates <- as.numeric(dat$week)
  incid$counts <- as.matrix(data_frame(imported = dat$imported ,
                                       others    = dat$total_cases))


  resA <- estimate_R(incid, method = "parametric_si",
                     config=(make_config(list(t_start = t_start:t_end,
                                              t_end   = (t_start + t_window):(t_end + t_window),
                                              mean_si = si_mean_days /unitm,
                                              std_si  = si_sd_days/unitm,
                                              # plot    = FALSE,
                                              mean_prior = R_prior,
                                              std_prior  = R_sd_prior))))



  # Changing names for convinience
  resB <- resA$R
  resB$Median_R       <-resB$`Median(R)`
  resB$Quantile_0.025 <-resB$`Quantile.0.025(R)`
  resB$Quantile_0.975 <-resB$`Quantile.0.975(R)`
  resB$Mean_R         <-resB$`Mean(R)`
  resB$Std_R          <- resB$`Std(R)`

  variables<- c("Median_R", "Quantile_0.025", "Quantile_0.975",
                "Mean_R", "Std_R", "t_start", "t_end")
  res <- resB[, variables]

  res$t_window <- t_window
  res$row <- res$t_end


  res_final <- merge(res, dat, by = c("row"), all=TRUE)
  res_final$window_start <-  0
  res_final$window_start[t_start] <- 1
  res_final$window_end <-  0
  res_final$window_end[t_start + t_window] <- 1
  res_final$window_units <- window_units
  res_final$dates <- res_final$date_calc

  col_names <- c("row",
                 "Median_R",
                 "Quantile_0.025",
                 "Quantile_0.975",
                 "Mean_R",
                 "Std_R",
                 "t_start",
                 "t_end",
                 "t_window",
                 "window_units",
                 "total_cases",
                 "imported",
                 "place",
                 "window_start",
                 "window_end",
                 "place",
                 'dates')

  Rt_data <- res_final[,col_names]

}



plot_R_estimates <- function (Rt_data, size_text, LPV,LPH) {

  Rt_data$others <- Rt_data$total_cases
  # size_text <- 10

  scaler      <- 5
  scale_multiplier <- max(Rt_data$total_cases)/ scaler# This is an artefact to make the two axis at a same scale

  Rt_data$MedianM <- Rt_data$Median_R *  scale_multiplier
  Rt_data$LowerM  <- Rt_data$Quantile_0.025 *  scale_multiplier
  Rt_data$UpperM  <- Rt_data$Quantile_0.975 *  scale_multiplier
  Rt_data$MeanLoc <- Rt_data$Mean_R *  scale_multiplier


  row_R0   <-  which(Rt_data$t_start == min(Rt_data$t_start, na.rm = TRUE))
  R0_Median_location    <- Rt_data$MedianM[row_R0]
  R0_Median_value       <- round(Rt_data$Median_R[row_R0],1)
  R0_Lower              <- round(Rt_data$Quantile_0.025[row_R0],1)
  R0_Upper              <- round(Rt_data$Quantile_0.975[row_R0],1)

  R0_Mean_location      <- Rt_data$MeanLoc[row_R0]
  R0_Mean_value         <- round(Rt_data$Mean_R[row_R0],1)
  R0_StanD              <- round(Rt_data$Std_R[row_R0],1)

  Rt_data$time_window_xmin <- Rt_data$dates[Rt_data$window_start == 1]
  Rt_data$time_window_xmax <- Rt_data$dates[Rt_data$window_end == 1]
  Rt_data$time_window_ymin <- -Inf
  Rt_data$time_window_ymax <- Inf

  location <- Rt_data$place[1]


  # label_median <- paste0('median = ', R0_Median_value)
  label_median <- paste0('Rt: ', R0_Median_value)
  label_CI    <- paste0('(', R0_Lower, ' - ', R0_Upper ,')')


  position_R     <- as.Date('2017-11-01')
  position_weeks <- as.Date('2017-11-01')

  num_weeks_window <- which(Rt_data$window_end == 1) - which(Rt_data$window_start == 1)
  if (Rt_data$window_units[1] == 'weeks') {y_lable  <- 'weekly cases'}
  if (Rt_data$window_units[1] == 'days')  {y_lable  <- 'daily number of cases'}


  col_type <- c( 'red', 'purple')

  others         <- Rt_data
  imported       <- Rt_data

  others$cases     <- others$total_cases
  imported$cases  <- imported$imported

  others$typetrans     <- 'others'
  imported$typetrans  <- 'imported'

  Rt_type <- rbind(others, imported)

  max_cases <- max(others$cases   + imported$cases) * 1.2


  my_plot <- ggplot(data= Rt_type) +
    geom_rect(aes(xmin=time_window_xmin,
                  xmax=time_window_xmax,
                  ymin=time_window_ymin,
                  # ymax=Rt_data$time_window_ymax), fill = '#a8ddb5') + # #a8ddb5  (green)  (yellow)
                  ymax=time_window_ymax), alpha=0.2, fill="#f7fcb9") + # '#ffeda0'
    geom_bar(aes(x = dates, y = cases, fill= typetrans),
             colour = 'black', size = 0.4, stat = 'identity', alpha = .4) +
    geom_ribbon(aes( x= dates, ymin = LowerM,ymax = UpperM),   fill = 'lightblue', alpha = 0.7)+
    geom_line(aes(x = dates, y = MedianM), colour = 'blue') +
    # geom_line(aes(x = dates, y = MeanLoc), colour = 'darkgreen') +
    scale_y_continuous(sec.axis = sec_axis(~./scale_multiplier, name = "Rt"))+
    # ggtitle(location)+
    ylab (y_lable)+ xlab ('') +
    theme_bw()+
    theme(legend.justification = "top") +
    guides(fill=guide_legend(title=NULL)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = size_text),
          text = element_text(size = size_text),
          axis.text.y = element_text(size = size_text))  +
    theme( strip.background = element_rect(colour="white", fill="black"),
           strip.text = element_text(size = size_text, colour = 'white'),
           axis.title.y =  element_text(size = size_text, margin = margin(t=0, r=5,b=0,l=0))) +
    # coord_cartesian(ylim = c(0, 300)) +
    theme(legend.position="none") +
    scale_fill_manual(values = col_type) +
    # geom_hline(yintercept= R0_Median_location  , linetype="dashed",
    #            color = "blue", size=1) +
    geom_hline(yintercept= 1  *  scale_multiplier, linetype="dashed",
               color = "black", size=1) +
    annotate('text', x =   position_R,
             y = (max_cases* .99),
             label = label_median, size = size_text/3, colour = "blue" ) +
    annotate('text', x =    position_R ,
             y = (max_cases * 0.85),
             label = label_CI, size = size_text/3, colour = 'blue') +
    facet_wrap(~place) +
    coord_cartesian(xlim = c(as.Date('2017-08-01'), as.Date('2018-11-01')),
                    ylim = c(0, max_cases * 1.1)) +
    theme(legend.justification=c(1,0), legend.position=c(LPV,LPH))



  return(my_plot)
}




