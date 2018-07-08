# Generic functions for bootstrapping

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
