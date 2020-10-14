#' @export create_nest_dynamics
#' @export create_nest_summary
#' @export create_nest_summary_map
#' @export create_nest_doubling
#' @export create_nest_halving
#' @export create_nest_rt
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

  # future::plan(future::multisession, workers = future::availableCores())

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

    # %>%
    # mutate(week_incidence_case=aweek::date2week(date_incidence_case,
    #                                             week_start = "Sunday",
    #                                             floor_day = T))

    mutate(data=map(.x = data,
                    .f = mutate,
                    week_incidence_case=aweek::date2week(date_incidence_case,
                                                         week_start = "Sunday",
                                                         floor_day = T))) %>%
    mutate(tsibble_day=map(.x = data,
                           .f = count,
                           date_incidence_case)) %>%
    mutate(tsibble_wik=map(.x = data,
                           .f = count,
                           week_incidence_case)) %>%
    mutate(tsibble_wik=map(.x = tsibble_wik,
                           .f = mutate,
                           date_incidence_case = aweek::week2date(week_incidence_case,
                                                                  week_start = "Sunday"))) %>%
    mutate(tsibble_wik=map(.x = tsibble_wik,
                           .f = select,
                           starts_with("strata_"),ends_with("_incidence_case"),
                           everything())) %>%

    select(-starts_with("i_"),
           -one_incidence_fit,
           -incidente_rt,#-tsibble_rt,
           date_lastlag_days)

}

create_nest_summary <- function(nest_dynamics,time_limit_fig02=90) {

  # future::plan(future::multisession, workers = future::availableCores())

  out <- tibble()

  nested_major <- nest_dynamics %>%

    # mutate(nm_pais="peru") %>%
    # mutate(strata_major=nm_pais,
    #        strata_minor=nm_depa) %>% # -------------------------------------------- extra
    group_by(strata_major) %>%
    nest() %>%
    ungroup()

  for (i in 1:nrow(nested_major)) { #i=1

    out <- nested_major %>%

      slice(i) %>%

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
      mutate(tab04=map(data,nested_table_04)) %>%

      # union all outputs
      union_all(out)

  }

  return(out)

}

create_nest_summary_map <- function(nest_dynamics,
                                    geometry,
                                    strata_major=nm_pais,
                                    strata_minor=nm_depa) {

  geometry <- geometry %>%
    # preidentify stratification variables
    rename(strata_major={{strata_major}},
           strata_minor={{strata_minor}})

  # add
  out <- tibble()
  # add
  nested_major <- create_nest_summary(nest_dynamics = nest_dynamics)

  for (i in 1:nrow(nested_major)) {
    out <- nested_major %>%

      #add
      slice(i) %>%

      mutate(fig04=map(data,nested_figure_04,
                       geometry = geometry, # -------------------- # cambio
                       strata_major=strata_major,
                       strata_minor=strata_minor)) %>%

      # union all outputs
      union_all(out)
  }

  return(out)

}


create_nest_doubling <- function(nest_dynamics) {

  growth_fit <- nest_dynamics %>%
    select(starts_with("strata_"),n_pre_clean,n_pos_clean,one_incidence_glance) %>%
    unnest(one_incidence_glance) %>% #count(names)
    mutate(mark=if_else(is.na(names),"1",names)) %>% #count(mark)
    # filter(is.na(names)) %>%
    # filter(magrittr::is_in(mark,c("1"))) %>%
    select(strata_minor_code,mark,contains("r."),p.value)

  dt_national_up <- nest_dynamics %>%
    select(starts_with("strata_"),n_pre_clean,n_pos_clean,one_incidence_tidy) %>%
    unnest(one_incidence_tidy) %>% #count(mark)
    filter(magrittr::is_in(mark,c("1"))) %>%
    filter(parameter=="doubling") %>%
    left_join(growth_fit) %>%
    arrange(estimate) #%>%
  # cdcper::cdc_datatable_html()

  return(dt_national_up)
}

create_nest_halving <- function(nest_dynamics) {

  growth_fit <- nest_dynamics %>%
    select(starts_with("strata_"),n_pre_clean,n_pos_clean,one_incidence_glance) %>%
    unnest(one_incidence_glance) %>% #count(names)
    mutate(mark=if_else(is.na(names),"1",names)) %>% #count(mark)
    # filter(is.na(names)) %>%
    # filter(magrittr::is_in(mark,c("1"))) %>%
    select(strata_minor_code,mark,contains("r."),p.value)

  dt_national_dowm <- nest_dynamics %>%
    select(starts_with("strata_"),n_pre_clean,n_pos_clean,one_incidence_tidy) %>%
    unnest(one_incidence_tidy) %>% #count(mark)
    filter(magrittr::is_in(mark,c("after"))) %>%
    filter(parameter=="halving") %>%
    left_join(growth_fit) %>%
    arrange(estimate) #%>%
  #cdcper::cdc_datatable_html()

  return(dt_national_dowm)
}

create_nest_rt <- function(nest_dynamics) {
  rt_national <- nest_dynamics %>%
    select(starts_with("strata_"),n_pre_clean,n_pos_clean,current_rt) %>%
    unnest(current_rt) %>%
    arrange(desc(rt_estimate)) #%>%
  # cdcper::cdc_datatable_html()

  return(rt_national)
}



# rt_write_rds <- function(nest_summary,
#                          rute,
#                          name) {
#
#   for (i in 1:nrow(nest_summary)) {
#
#     ii <- if_else(str_length(as.character(i))==1,str_c("0",i),as.character(i))
#
#     nest_summary %>%
#       select(-starts_with("data")) %>%
#       slice(i) %>%
#       pivot_longer(cols = -strata_major,
#                    names_to = "object",
#                    values_to = "x") %>%
#       mutate(path=str_c({{rute}},"rt-",{{name}},"-",ii,"-",strata_major,"-",object,".rds")) %>%
#       rowwise() %>%
#       mutate(write=list(write_rds(x = x,path = path)))
#
#   }
#
# }

# rt_write_rds <- function(nest_summary,
#                          rute,
#                          name) {
#
#   for (i in 1:nrow(nest_summary)) {
#
#     # i=1
#     # rute = ""
#     # name = "admx"
#     ii <- if_else(str_length(as.character(i))==1,str_c("0",i),as.character(i))
#
#     nest_summary %>%
#       select(-starts_with("data")) %>%
#       slice(i) %>%
#       pivot_longer(cols = -strata_major,
#                    names_to = "object",
#                    values_to = "x") %>%
#       mutate(index=ii) %>%
#       # mutate(path=str_c({{rute}},"rt-",{{name}},"-",ii,"-",strata_major,"-",object,".rds")) %>%
#       mutate(path=str_c(rute,"rt-",name,"-",index,"-",strata_major,"-",object,".rds")) %>%
#       #   purrr::map2(.x = .$x, .y = .$path, .f = write_rds)
#       #   select(x,path)
#       # purrr::map2(.x = a$x,.y = a$path,.f = write_rds)
#       # purrr::pmap(.l = select(.,x,path),.f = write_rds)
#       rowwise() %>%
#       mutate(write=list(write_rds(x = x,path = path)))
#
#   }
#
# }
