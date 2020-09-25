#' MAIN TITLE
#'
#' @description main description
#'
#' @describeIn incidence_pull specific description
#'
#' @param data data
#' @param variable_date variable_date
#' @param fit fit
#' @param epiestim_output epiestim_output
#' @param epiestim_tibble_rt epiestim_tibble_rt
#' @param y_breaks y_breaks
#' @param date_breaks date_breaks
#' @param date_start date_start
#' @param date_end date_end
#' @param nombre_area nombre_area
#' @param date_start date_start
#' @param date_end date_end
#' @param nombre_area nombre_area
#' @param nest_level nest_level
#' @param high_level high_level
#' @param order order
#' @param point point
#' @param xmax xmax
#' @param xmin xmin
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
#' @export incidence_pull
#' @export dynamic_output_figure
#' @export incidence_output_figure
#' @export epiestim_tibble_rt
#' @export epiestim_current_rt
#' @export figure_tsibble_rt
#' @export cdc_datatable_html
#' @export cdc_dotwhiskers_ggplot
#' @export nested_figure_01
#' @export nested_figure_02
#' @export nested_figure_03
#' @export nested_figure_04
#' @export nested_table_01
#' @export nested_table_02
#' @export nested_table_03

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

#' @describeIn incidence_pull create figure: incidence original plot number 1
#' @inheritParams incidence_pull

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

#' @describeIn incidence_pull create figure: incidence original plot number 2
#' @inheritParams incidence_pull

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

#' @describeIn incidence_pull Rt estimates: one table with dates and R values
#' @inheritParams incidence_pull

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
    select(date_rt=value,everything())
}

#' @describeIn incidence_pull Rt estimates: filter and select data
#' @inheritParams incidence_pull

epiestim_current_rt <- function(epiestim_tibble_rt) {
  epiestim_tibble_rt %>%
    filter(t_end==max(t_end)) %>%
    select(date_rt,
           rt_estimate=median_r,
           rt_conf.lower=quantile_0_025_r,
           rt_conf.upper=quantile_0_975_r) #%>%
  # mutate(variable="Número reproductivo efectivo (Rt) *")
}

#' @describeIn incidence_pull create figure: time serie of Rt
#' @inheritParams incidence_pull

figure_tsibble_rt <- function(epiestim_tibble_rt,y_breaks=10,date_breaks = "7 day") {
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
    scale_x_date(date_breaks = date_breaks,date_labels = "%b-%d") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)#,
          # legend.background = element_rect(fill="white",size=0,
          #                                   linetype="solid",colour ="white"),
          #  legend.position = c(0.8, 0.7)
    ) +
    labs(title = "Estimated R",y = "Rt",color="",fill="")
}

#' @describeIn incidence_pull other description
#' @inheritParams incidence_pull

cdc_datatable_html <- function(data) {
  data %>%
    mutate_if(.predicate = is.numeric,.funs = ~round(.x,2)) %>%
    DT::datatable(
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
}

#' @describeIn incidence_pull create figure: dot whiskers ggplot
#' @inheritParams incidence_pull

# cdcper::cdc_dotwhiskers_plot()
cdc_dotwhiskers_ggplot <- function(data,
                                   nest_level=nm_dist,
                                   high_level=nm_depa,
                                   order=n_pos_clean,
                                   point=rt_estimate,
                                   xmax=rt_conf.upper,
                                   xmin=rt_conf.lower
) {
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

#' @describeIn incidence_pull figure 1: incidence plot
#' @inheritParams incidence_pull

nested_figure_01 <- function(data) {
  data %>%
    select(cd_dist,data) %>%
    unnest(cols = c(data)) %>%
    ggplot(aes(x = fecha_de_inicio_de_sintomas_corregido)) +
    geom_histogram(binwidth = 1) +
    # scale_x_date(date_breaks = "7 days",date_labels = "%b-%d") +
    # theme(axis.text.x = element_text(angle = 90, hjust = 1) +
    facet_wrap(~nm_dist,scales = "free_y"
    ) +
    labs(title = "Incidence")
}

#' @describeIn incidence_pull figure 2: time serie of Rt per strata
#' @inheritParams incidence_pull

nested_figure_02 <- function(data) {
  data %>%
    select(cd_dist,tsibble_rt,starts_with("cd_"),starts_with("nm_")) %>%
    unnest(cols = c(tsibble_rt)) %>%
    filter(date_rt>Sys.Date()-90) %>%
    figure_tsibble_rt(y_breaks = 5,date_breaks = "14 days") +
    facet_wrap(~nm_dist,scales = "free_y") +
    theme(legend.position = "none")
}

#' @describeIn incidence_pull figure 3: dot-whiskers plot of each strata
#' @inheritParams incidence_pull

nested_figure_03 <- function(data) {
  data %>%
    # select(nm_depa,data) %>%
    # unnest(cols = c("data")) %>%
    # select(starts_with("nm_"),n_pos_clean,current_rt) %>%
    select(starts_with("nm_"),cd_dist,n_pos_clean,current_rt) %>%
    unnest(cols = c("current_rt")) %>%
    cdc_dotwhiskers_ggplot(nest_level = str_c(nm_dist," - ",cd_dist),
                           high_level = nm_depa_nest) +
    geom_vline(aes(xintercept=1),lty=2,color="red") +
    labs(caption = "* A 14 días hacia atrás por la mediana del tiempo de rezago\nentre inicio de síntomas y confirmación de caso.",
         x="Rt actual",y="Distrito - Ubigeo",size="Número\nde casos")
}

#' @describeIn incidence_pull figure 4: map of Rt per strata
#' @inheritParams incidence_pull

nested_figure_04 <- function(data) {

  # rescue department name
  fig04_depa <- data %>%
    select(nm_depa_nest) %>%
    distinct() %>% pull(nm_depa_nest)

  # extract key current rt values
  a_input <- data %>%
    select(cd_dist,nm_dist,current_rt) %>%
    unnest(cols = c("current_rt")) %>%
    select(-date_rt)

  # # # extract extreme rt values in case of outliers
  # # # to use in ggplot
  # a_limit <- if_else(condition = (a_input %>% pull(rt_estimate) %>% max()) > 2,
  #                    true = (a_input %>% pull(rt_estimate) %>% max() %>% ceiling()),
  #                    false = 2)
  # # a_limit %>% return()

  # plot
  ubigeo_geometria %>%
    filter(nm_depa==fig04_depa) %>%
    left_join(a_input) %>%
    ggplot() +
    geom_sf(aes(fill=rt_estimate)) +
    scale_fill_continuous_diverging("Blue-Red 3",mid=1 #,
                                    # limits = c(0,2)
                                    # limits = c(0,a_limit)
    )

}

#' @describeIn incidence_pull table 1: growth rate and doubling time. includes halving time if there is a split
#' @inheritParams incidence_pull

nested_table_01 <- function(data) {
  data %>%
    select(cd_dist,starts_with("nm_"),one_incidence_tidy,date_split_peak) %>%
    unnest(cols = c(one_incidence_tidy)) %>%
    mutate(parameter=if_else(parameter=="r","growth_rate",parameter)) %>%
    cdc_datatable_html()
}

#' @describeIn incidence_pull table 2: goodness of fit per strata
#' @inheritParams incidence_pull

nested_table_02 <- function(data) {
  data %>%
    select(cd_dist,starts_with("nm_"),one_incidence_glance) %>%
    unnest(cols = c(one_incidence_glance)) %>%
    select(-(df:df.residual),-sigma,-statistic) %>%
    cdc_datatable_html()
}

#' @describeIn incidence_pull table 3: Rt per strata
#' @inheritParams incidence_pull

nested_table_03 <- function(data) {
  data %>%
    select(cd_dist,starts_with("nm_"),current_rt) %>%
    unnest(cols = c(current_rt)) %>%
    cdc_datatable_html()
}
