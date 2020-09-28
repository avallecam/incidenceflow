#' @export create_nest_dynamics
#' @export create_nest_summary
#' @export create_nest_summary_map
#'

#### purrr workflow ---------------------
create_nest_dynamics <- function(linelist,
                                 dictionary,
                                 strata_major,
                                 strata_minor,
                                 strata_minor_code,
                                 date_incidence_case,
                                 date_of_analysis_today=FALSE,
                                 interval_set = 1,
                                 time_delay_set = 7,
                                 disease_mean_si = 3.96,
                                 disease_std_si = 4.75,
                                 issue_number_set = 40) {

  linelist <- linelist %>%
    # preidentify stratification variables # -------------------------------------------- extra
    rename(strata_major={{strata_major}},
           strata_minor={{strata_minor}},
           date_incidence_case={{date_incidence_case}})

  dictionary <- dictionary %>%
    # preidentify stratification variables
    rename(strata_major={{strata_major}},
           strata_minor={{strata_minor}},
           strata_minor_code={{strata_minor_code}})

  # develop
  linelist %>%
    # clean
    incidence_nest_clean(
      nest_level = strata_minor, # ----------------------------------------- cambio
      custom_pre_cleaning = covid_pre_cleaning,
      issue_number = issue_number_set) %>%
    # join a ubigeo dictionary #diccionario_strata_major_minor
    left_join(dictionary) %>% # -------------------------- cambio
    # create date
    mutate(data_dates=map(
      .x = data,
      .f = incidence_dates_mutate,
      variable_date = date_incidence_case,
      date_of_analysis_today = date_of_analysis_today,
      time_delay_days = time_delay_set
    )) %>%
    unnest(cols = c(data_dates)) %>%
    # # set incidence v1
    mutate(i_daily_to_lastone = pmap(
      .l = select(., data = data, last_date = date_lastone),
      .f = possibly(incidence_pull,NA_real_),
      variable_date = date_incidence_case,
      interval=7)) %>%
    # # set incidence v2
    # mutate(i_daily_to_delay = pmap(
    #   .l = select(., data = data, last_date = date_lastlag_weeks),
    #   .f = possibly(incidence_pull,NA_real_),
    #   variable_date = date_incidence_case,
    #   interval=7)) %>%
    # # set incidence v3
    # mutate(i_daily_to_lastone_d = pmap(
    #   .l = select(., data = data, last_date = date_lastone),
    #   .f = possibly(incidence_pull,NA_real_),
    #   variable_date = date_incidence_case,
  #   interval=1)) %>%
  # set incidence v4
  mutate(i_daily_to_delay_d = pmap(
    .l = select(., data = data, last_date = date_lastlag_days),
    .f = possibly(incidence_pull,NA_real_),
    variable_date = date_incidence_case,
    interval = interval_set)) %>%
    # fit incidence
    mutate(incidence_fit=map(.x = i_daily_to_delay_d,
                             .f = possibly(incidence::fit,NA_real_),
                             quiet = T)) %>%
    # fit incidence split
    mutate(incidence_split=map(.x = i_daily_to_delay_d,
                               .f = possibly(incidence::fit_optim_split,
                                             list(split=lubridate::ymd(00000000))),
                               quiet = T)) %>%
    mutate(incidence_split_fit=map(.x = incidence_split,.f = pluck("fit"))) %>%

    # extract real peak
    mutate(date_split_peak=map(.x = incidence_split,.f = pluck("split"))) %>%
    unnest(cols = c(date_split_peak)) %>%
    mutate(incidence_split_real=if_else(is.na(date_split_peak),"n","y")) %>%

    # tidy + glance for fit and incidence
    mutate(incidence_tidy=map(.x = incidence_fit,
                              .f = possibly(tidy_incidence,tibble()))) %>%
    mutate(incidence_glance=map(.x = incidence_fit,
                                .f = possibly(glance_incidence,tibble()))) %>%
    mutate(incidence_split_tidy=map(.x = incidence_split_fit,
                                    .f = possibly(tidy_incidence,tibble()))) %>%
    mutate(incidence_split_glance=map(.x = incidence_split_fit,
                                      .f = possibly(glance_incidence,tibble()))) %>%

    # unify tidy and glance
    mutate(one_incidence_fit=if_else(incidence_split_real=="y",
                                     incidence_split_fit,incidence_fit)) %>%
    mutate(one_incidence_tidy=if_else(incidence_split_real=="y",
                                      incidence_split_tidy,incidence_tidy)) %>%
    mutate(one_incidence_glance=if_else(incidence_split_real=="y",
                                        incidence_split_glance,incidence_glance)) %>%
    # select relevant results
    select(strata_major,strata_minor,strata_minor_code, # -------------------------------- cambio
           data,
           n_pre_clean, n_pos_clean,
           i_daily_to_lastone,
           # i_daily_to_lastone_d,
           # i_daily_to_delay,
           i_daily_to_delay_d,
           starts_with("one_"),
           date_split_peak,
           date_firstone,
           date_lastone,
           date_lastlag_days#,
           # starts_with("cd_"),starts_with("nm_")
    ) %>%

    # output 01: create incidence + fit figure
    mutate(incidence_fit_figure=pmap(
      .l = select(.,data = i_daily_to_lastone,
                  fit = one_incidence_fit,
                  date_start = date_firstone,
                  date_end = date_lastlag_days,
                  nombre_area = strata_minor), # ------------------------------------------ cambio
      .f = possibly(dynamic_output_figure,NA_real_),
      date_breaks = "7 day")) %>%

    # # output 02: create incidence only figure
    # mutate(incidence_only_figure=pmap(
    #   .l = select(.,data = i_daily_to_lastone_d,
    #               # fit = one_incidence_fit,
    #               date_start = date_firstone,
    #               date_end = date_lastlag_weeks,
    #               nombre_area = strata_minor), # ------------------------------------------ cambio
    #   .f = possibly(incidence_output_figure,NA_real_),
    #   date_breaks = "7 day")) %>%

  # estimate effect reproductive number
  mutate(incidente_rt = map(
    .x = i_daily_to_delay_d, # interval = 1
    .f = possibly(~estimate_R(incid = .x,
                              method = "parametric_si",
                              config = make_config(list(mean_si = disease_mean_si,
                                                        std_si = disease_std_si))),
                  NA_real_))
  ) %>%
    # return rt time serie
    mutate(tsibble_rt=map(.x = incidente_rt, .f = possibly(epiestim_tibble_rt,NA_real_))) %>%
    mutate(current_rt=map(.x = tsibble_rt, .f = possibly(epiestim_current_rt,NA_real_))) %>%
    mutate(last5wk_rt=map(.x = tsibble_rt, .f = possibly(epiestim_last5wk_rt,NA_real_))) %>%
    mutate(rt_figure=pmap(.l = select(., epiestim_tibble_r=tsibble_rt, title_rt=strata_minor),
                          .f = possibly(figure_tsibble_rt,NA_real_))) %>%

    select(-starts_with("i_"),
           -one_incidence_fit,
           -incidente_rt,#-tsibble_rt,
           date_lastlag_days)

}

create_nest_summary <- function(nest_dynamics,time_limit_fig02=90) {

  nest_dynamics %>%
    # mutate(nm_pais="peru") %>%
    # mutate(strata_major=nm_pais,
    #        strata_minor=nm_depa) %>% # -------------------------------------------- extra
    group_by(strata_major) %>%
    nest() %>%
    ungroup() %>%
    mutate(fig01=map(data,nested_figure_01,
                     strata=strata_minor)) %>%
    mutate(fig02=map(data,nested_figure_02,
                     strata=strata_minor,
                     date_lastone=date_lastone,
                     limit_figure=time_limit_fig02)) %>%
    mutate(fig03=map(data,nested_figure_03,
                     strata_major=strata_major,
                     strata_minor=strata_minor,
                     strata_minor_code=strata_minor_code)) %>%
    # mutate(fig04=map(data,nested_figure_04,
    #                  geometry = geometry, # -------------------- # cambio
    #                  strata_major=strata_major,
    #                  strata_minor=strata_minor)) %>%
    # pull(fig01) %>% pluck()
    mutate(tab01=map(data,nested_table_01)) %>%
    mutate(tab02=map(data,nested_table_02)) %>%
    mutate(tab03=map(data,nested_table_03)) %>%
    mutate(tab04=map(data,nested_table_04))

}

create_nest_summary_map <- function(nest_dynamics,
                                    geometry,
                                    strata_major=nm_pais,
                                    strata_minor=nm_depa) {

  geometry <- geometry %>%
    # preidentify stratification variables
    rename(strata_major={{strata_major}},
           strata_minor={{strata_minor}})

  create_nest_summary(nest_dynamics = nest_dynamics) %>%
    mutate(fig04=map(data,nested_figure_04,
                     geometry = geometry, # -------------------- # cambio
                     strata_major=strata_major,
                     strata_minor=strata_minor))

}

