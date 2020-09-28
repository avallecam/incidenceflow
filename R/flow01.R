#' MAIN TITLE
#'
#' @description main description
#'
#' @describeIn incidence_pull specific description
#'
#' @return incidence workflow!
#'
#' @import aweek
#' @import janitor
#' @import incidence
#' @import EpiEstim
#' @import patchwork
#' @import colorspace
#'
#' @export pre_clean
#' @export covid_pre_cleaning
#' @export incidence_nest_clean
#' @export incidence_dates_mutate
#' @export incidence_pull
#' @export dynamic_output_figure
#' @export incidence_output_figure
#' @export epiestim_tibble_rt
#' @export epiestim_current_rt
#' @export epiestim_last5wk_rt
#' @export figure_tsibble_rt
#' @export nested_figure_01
#' @export nested_figure_02
#' @export nested_figure_03
#' @export nested_figure_04
#' @export nested_table_01
#' @export nested_table_02
#' @export nested_table_03
#' @export nested_table_04
#' @export cdc_dotwhiskers_ggplot
#'

#### tidier workflow -------------------------------

cdc_dotwhiskers_ggplot <- function(data,
                                   nest_level=nm_dist,
                                   high_level=nm_depa,
                                   order=n_pos_clean,
                                   point=rt_estimate,
                                   xmax=rt_conf.upper,
                                   xmin=rt_conf.lower) {
  data %>%
    mutate(nest_level_var={{nest_level}}) %>%
    mutate(high_level_var={{high_level}}) %>%
    mutate(nest_level_var=fct_reorder(.f=nest_level_var,
                                      .x={{point}})) %>%

    ggplot(aes(x = {{point}},y = nest_level_var)) +
    geom_point(aes(size={{order}}),color="red",alpha=0.7) +
    geom_errorbarh(aes(xmax = {{xmax}}, xmin = {{xmin}})) +
    facet_wrap(~high_level_var,scales = "free_x") +
    theme(legend.position="bottom")
}

pre_clean <- function(data) {
  data %>%
    # mutate(import_case_x={{import_case_variable}}) %>%
    # filter(import_case_x=="caso_local") %>%
    filter(!is.na(date_incidence_case))
}

covid_pre_cleaning <- function(data) {
  data %>%
    # filter(local_importado=="caso_local") %>%
    filter(!is.na(date_incidence_case))
}

incidence_nest_clean <- function(data, nest_level,
                                 custom_pre_cleaning,
                                 issue_number=issue_number_set) {

  pre_clean <- custom_pre_cleaning

  data %>%
    group_by({{nest_level}}) %>%
    nest() %>%
    mutate(n_pre_clean=map_int(.x = data,.f = nrow)) %>%
    mutate(data=map(.x = data,.f = pre_clean)) %>%
    mutate(n_pos_clean=map_int(.x = data,.f = nrow)) %>%
    filter(!is.na({{nest_level}})) %>%
    ungroup() %>%
    filter(n_pos_clean > issue_number)
}

incidence_dates_mutate <- function(data,
                                   variable_date,
                                   date_of_analysis_today, #Sys.Date()
                                   time_delay_days=time_delay_set) {

  sgb19 <- data %>%
    mutate(fecha_de_inicio_de_sintomas_corregido={{variable_date}})

  # first --------------------
  date_firstone <- sgb19 %>%
    filter(fecha_de_inicio_de_sintomas_corregido==
             min(fecha_de_inicio_de_sintomas_corregido)) %>%
    distinct(fecha_de_inicio_de_sintomas_corregido) %>%
    pull(fecha_de_inicio_de_sintomas_corregido)

  # last --------------------
  date_lastone <- sgb19 %>%
    filter(fecha_de_inicio_de_sintomas_corregido==
             max(fecha_de_inicio_de_sintomas_corregido)) %>%
    distinct(fecha_de_inicio_de_sintomas_corregido) %>%
    pull(fecha_de_inicio_de_sintomas_corregido)

  # fecha analisis -------------------------
  if (date_of_analysis_today==TRUE) {
    date_today <- Sys.Date()
  }

  date_today <- date_lastone #Sys.Date()

  # delay --------------------
  date_with_lag_due_to_delay <- date_today %m-% days(time_delay_days)
  # date_with_lag_due_to_incubation <- date_today %m-% weeks(time_delay_weeks)

  # date_peak_of_cases <- sgb19 %>%
  #   count(fecha_de_inicio_de_sintomas_corregido,sort = T) %>%
  #   top_n(n = 1,wt = n) %>%
  #   filter(fecha_de_inicio_de_sintomas_corregido==max(fecha_de_inicio_de_sintomas_corregido)) %>%
  #   pull(fecha_de_inicio_de_sintomas_corregido)

  tibble(
    # date_today=date_today,
    date_firstone=date_firstone,
    date_lastone=date_lastone,
    date_lastlag_days=date_with_lag_due_to_delay#,
    # date_lastlag_weeks = date_with_lag_due_to_incubation,
    # date_topn=date_peak_of_cases
  ) %>%
    return()
}

incidence_pull <- function(data,variable_date,...) {
  data %>%
    pull({{variable_date}}) %>%
    incidence(dates = .,...)
}

# fit_optim_split_purrr <- possibly(incidence::fit_optim_split,NA_real_)
# name <- function(incidence_data) {
#   incidence_data %>%
#   possibly(incidence::fit_optim_split,list(split=lubridate::ymd(00000000)))
# }

dynamic_output_figure <- function(data,fit,
                                  date_start,date_end,
                                  nombre_area,date_breaks = "7 day") {

  my_theme <- theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                     vjust = 0.5, color = "black"))

  figure_dynamics <- plot(data,
                          border = "white",
                          # color = "#cecece",
                          color = "#ababab",
                          ylab = " ",
                          # date_breaks = "7 day",
                          date_breaks = date_breaks,
                          date_labels = "%b %d") %>%
    add_incidence_fit(fit) +

    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +

    my_theme +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = str_c(str_to_title(nombre_area),": Casos sin antecedente de viaje"),
         subtitle = str_c("Datos actualizados al ",
                          Sys.Date(),
                          ".\nCurvas estimadas ",
                          "del ",
                          date_start,
                          " al ",
                          date_end
                          #fit_doubling_peak_date
                          #fit_doubling_halving_peackday
         ),
         x = "Fecha de inicio de síntomas",
         y = "Casos incidentes")

  return(figure_dynamics)
}

incidence_output_figure <- function(data,#fit,
                                    date_start,date_end,
                                    nombre_area,date_breaks = "7 day") {

  my_theme <- theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                     vjust = 0.5, color = "black"))

  figure_dynamics <- plot(data,
                          border = "white",
                          # color = "#cecece",
                          # color = "#ababab",
                          ylab = " ",
                          # date_breaks = "7 day",
                          date_breaks = date_breaks,
                          date_labels = "%b %d") +
    # add_incidence_fit(fit) +

    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +

    my_theme +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = str_c(str_to_title(nombre_area),": Casos sin antecedente de viaje"),
         subtitle = str_c("Datos actualizados al ",
                          Sys.Date(),
                          ".\nCurvas estimadas ",
                          "del ",
                          date_start,
                          " al ",
                          date_end
                          #fit_doubling_peak_date
                          #fit_doubling_halving_peackday
         ),
         x = "Fecha de inicio de síntomas",
         y = "Casos incidentes")

  return(figure_dynamics)
}

epiestim_tibble_rt <- function(epiestim_output) {
  date_column <- epiestim_output %>%
    pluck("dates") %>%
    as_tibble() %>%
    rownames_to_column(var = "t_start") %>%
    mutate(t_start=as.double(t_start))

  epiestim_output %>%
    pluck("R") %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    left_join(date_column) %>%
    select(date_rt=value,everything()) %>%
    # #media movil para suavizar tendendias
    mutate(across(starts_with("rt_"),.fns = zoo::rollmean,
                  k = 7,fill=NA,align = "right"))
  # filter(!is.na(rt_estimate))
}

epiestim_current_rt <- function(epiestim_tibble_rt) {
  epiestim_tibble_rt %>%
    filter(t_end==max(t_end)) %>%
    select(date_rt,
           rt_estimate=median_r,
           rt_conf.lower=quantile_0_025_r,
           rt_conf.upper=quantile_0_975_r)
}

epiestim_last5wk_rt <- function(epiestim_tibble_rt) {
  epiestim_tibble_rt %>%
    select(date_rt,
           rt_estimate=median_r,
           rt_conf.lower=quantile_0_025_r,
           rt_conf.upper=quantile_0_975_r) %>%
    # create week
    mutate(aweek_rt=aweek::date2week(date_rt,
                                     week_start = "Sunday",
                                     floor_day = TRUE)) %>%
    # calcular promedio por semana
    group_by(aweek_rt) %>%
    summarise(across(starts_with("rt_"),.fns = mean)) %>%
    ungroup() %>%
    # conservar las últimas 5 semanas
    top_n(n = 5,wt = aweek_rt) %>%
    serosurvey::unite_dotwhiskers(variable_dot = rt_estimate,
                                  variable_low = rt_conf.lower,
                                  variable_upp = rt_conf.upper,
                                  digits_dot = 2,
                                  digits_low = 2,
                                  digits_upp = 2,
                                  decimal_to_percent = FALSE) %>%
    select(aweek_rt,unite=unite1_rt_estimate) %>%
    mutate(unite=str_replace_all(unite," ","")) %>%
    pivot_wider(names_from = aweek_rt,values_from = unite)
}

figure_tsibble_rt <- function(epiestim_tibble_rt,
                              title_rt = "Estimated R",
                              y_breaks=10,
                              date_breaks = "7 day") {
  epiestim_tibble_rt %>%
    ggplot(aes(x = date_rt,y = mean_r)) +
    geom_ribbon(aes(ymin = quantile_0_025_r,
                    ymax = quantile_0_975_r,
                    fill = "95%CrI")) +
    geom_line(aes(colour="Mean")) +
    geom_hline(aes(yintercept=1),colour="red") +
    scale_color_grey(start = 0.1) +
    scale_fill_grey(start = 0.7) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = y_breaks)) +
    # scale_x_date(date_breaks = date_breaks,date_labels = "%b-%d") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)#,
          # legend.background = element_rect(fill="white",size=0,
          #                                   linetype="solid",colour ="white"),
          #  legend.position = c(0.8, 0.7)
    ) +
    labs(title = {{title_rt}},y = "Rt",color="",fill="")
}

nested_figure_01 <- function(data,strata) {
  data %>%
    mutate(strata_minor_x={{strata}}) %>%
    select(strata_minor_x,data) %>%
    unnest(cols = c(data)) %>%
    ggplot(aes(x = date_incidence_case)) +
    geom_histogram(binwidth = 1) +
    # scale_x_date(date_breaks = "7 days",date_labels = "%b-%d") +
    facet_wrap(~strata_minor_x,scales = "free_y") +
    labs(title = "Incidence") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

nested_figure_02 <- function(data,strata,date_lastone,limit_figure) {
  data %>%
    mutate(strata_minor_x={{strata}},
           date_lastone_x={{date_lastone}}) %>%
    select(strata_minor_x,date_lastone_x,
           tsibble_rt,starts_with("cd_"),starts_with("nm_")) %>%
    unnest(cols = c(tsibble_rt)) %>%
    filter(date_rt>date_lastone_x-limit_figure) %>%
    figure_tsibble_rt(y_breaks = 5,date_breaks = "14 days") +
    facet_wrap(~strata_minor_x,scales = "free_y") +
    theme(legend.position = "none")
}

nested_figure_03 <- function(data,
                             strata_major,
                             strata_minor,
                             strata_minor_code,
                             time_delay_days=time_delay_set) {
  data %>%
    mutate(strata_minor_x={{strata_minor}},
           strata_major_x={{strata_major}},
           strata_minor_code_x={{strata_minor_code}}) %>%
    select(#starts_with("nm_"),
      strata_major_x,strata_minor_x,strata_minor_code_x,
      n_pos_clean,current_rt) %>%
    unnest(cols = c("current_rt")) %>%
    cdc_dotwhiskers_ggplot(nest_level = str_c(strata_minor_x," - ",
                                              strata_minor_code_x), # -------------- cuidado
                           high_level = strata_major_x) +
    geom_vline(aes(xintercept=1),lty=2,color="red") +
    labs(caption = str_c("* A ",
                         time_delay_days,
                         " días hacia atrás por la mediana del tiempo de rezago\nentre inicio de síntomas y confirmación de caso."),
         x="Rt actual",y="Distrito - Ubigeo",size="Número\nde casos")
}

nested_figure_04 <- function(data, geometry , strata_major, strata_minor) {

  # rescue department name
  fig04_depa <- data %>%
    mutate(strata_major_x={{strata_major}}) %>%
    select(strata_major_x) %>%
    distinct() %>% pull(strata_major_x)

  # extract key current rt values
  a_input <- data %>%
    mutate(strata_minor_x={{strata_minor}}) %>%
    select(strata_minor_x,current_rt) %>% # --------------------- cuidado
    unnest(cols = c("current_rt")) %>%
    select(-date_rt)

  # # # extract extreme rt values in case of outliers
  # # # to use in ggplot
  # a_limit <- if_else(condition = (a_input %>% pull(rt_estimate) %>% max()) > 2,
  #                    true = (a_input %>% pull(rt_estimate) %>% max() %>% ceiling()),
  #                    false = 2)
  # # a_limit %>% return()

  # plot
  geometry %>% # --------------------------------------------------------------- cambio
    mutate(strata_major_x={{strata_major}},
           strata_minor_x={{strata_minor}}) %>%
    filter(strata_major_x==fig04_depa) %>%
    left_join(a_input) %>%
    ggplot() +
    geom_sf(aes(fill=rt_estimate)) +
    scale_fill_continuous_diverging("Blue-Red 3",mid=1 #,
                                    # limits = c(0,2)
                                    # limits = c(0,a_limit)
    )

}

nested_table_01 <- function(data,strata) {
  data %>%
    select({{strata}},starts_with("strata_"),
           one_incidence_tidy,date_split_peak) %>%
    unnest(cols = c(one_incidence_tidy)) %>%
    mutate(parameter=if_else(parameter=="r","growth_rate",parameter))
}

nested_table_02 <- function(data,strata) {
  data %>%
    select({{strata}},starts_with("strata_"),one_incidence_glance) %>%
    unnest(cols = c(one_incidence_glance)) %>%
    select(-(df:df.residual),-sigma,-statistic)
}

nested_table_03 <- function(data,strata) {
  data %>%
    select({{strata}},starts_with("strata_"),current_rt) %>%
    unnest(cols = c(current_rt))
}

nested_table_04 <- function(data,strata) {
  data %>%
    select({{strata}},starts_with("strata_"),last5wk_rt) %>%
    unnest(cols = c(last5wk_rt))
}
