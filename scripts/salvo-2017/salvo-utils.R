
# Tables ------------------------------------------------------------------

coef_table <- function(fit) {
  
  stations <- map(fit, ~.$station) %>% 
    flatten_chr()
  
  map(fit, ~.$coef) %>% 
    transpose() %>%
    map(flatten_dbl) %>% 
    as_tibble() %>% 
    add_column(Station = stations, .before = 1) %>% 
    mutate_if(is.numeric, funs(round), digits = 2)
}


rmse_table <- function(fit) {
  
  stations <- map(fit, ~.$station) %>% 
    flatten_chr()
  
  map(fit, ~.$rmse) %>% 
    flatten_dbl() %>% 
    set_names(stations) %>%
    data.frame(RMSE = .) %>% 
    tibble::rownames_to_column(var = "Station") %>% 
    mutate_if(is.numeric, funs(round), digits = 2)
}


# Plots -------------------------------------------------------------------

gam_plot <- function(fit, smooth, xlab) {
  
  x <- fit$model[,smooth$term]
  
  if (smooth$by == "NA") {
    by.level = "NA"
  } else {
    by.level = smooth$by.level
  }
  
  range = tibble(x = x, by = by.level)
  names(range) = c(smooth$term, smooth$by)
  
  par <- smooth$first.para:smooth$last.para
  mat <- PredictMat(smooth, range)
  y <- (mat %*% fit$coefficients[par]) %>% as.numeric
  
  se <- ((mat %*% fit$Vp[par, par, drop = FALSE]) * mat) %>%
    rowSums %>% 
    sqrt
  
  df <- tibble(
    label = smooth$label,
    x.var = smooth$term,
    x.val = x,
    value = y,
    se = se
  )
  
  ggplot(df, aes(x.val, value)) +
    geom_ribbon(aes(ymin = value - 2*se, ymax = value + 2*se), 
                fill = "grey80") +
    geom_line(color = 'blue', size = 1) +
    #geom_point(aes(x = x.val, y = min(value-2*se)-sd(value))) +
    labs(y = 's(.)', x = xlab) +
    theme_bw()
  
}

bs_df_gam_plot <- function(i) {
  
  fit <- read_rds(
    paste0("scripts/salvo-2017/gam-fits/gam_fit_", i, ".rds")
  )
  
  smooth <- fit$smooth[[1]]
  
  x <- fit$model[,smooth$term]
  
  if (smooth$by == "NA") {
    by.level = "NA"
  } else {
    by.level = smooth$by.level
  }
  
  range = tibble(x = x, by = by.level)
  names(range) = c(smooth$term, smooth$by)
  
  par <- smooth$first.para:smooth$last.para
  mat <- PredictMat(smooth, range)
  y <- (mat %*% fit$coefficients[par]) %>% as.numeric
  
  se <- ((mat %*% fit$Vp[par, par, drop = FALSE]) * mat) %>%
    rowSums %>% 
    sqrt
  
  tibble(
    x.val = x,
    value = y,
    int = i
  )
}

pred_obs_plot <- function(obs, pred) {
  
  tibble(x = obs, y = pred) %>% 
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    labs(x = "Valores observados", y = "Valores preditos") +
    theme_bw()
  
}


# Bootstrapping -----------------------------------------------------------

get_days_sample <- function(df) {
  
  days <-
    df %>%
    distinct(date)
  
  m <- nrow(days)
  
  days %>% 
    sample_n(size = m, replace = TRUE)
  
}

bootstrapping <- function(i, df, df_share_bs, 
                          fit_func, ...) {
  
  df_share_bs <- select(df_share_bs, share_bs = i, date)
  
  days <- get_days_sample(df)
  
  df <- df %>% 
    left_join(df_share_bs, by = "date") %>% 
    mutate(share_gas = share_bs) %>% 
    left_join(x = days, y = ., by = "date")
  
  #df <- map_dfr(days, filter_df, df = df)
  
  fit_func(data = df, i = i, ...)
  
}


