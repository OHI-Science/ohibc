rescale_bprime_crit <- function(fish_stat_df,
                                overfished_th, underfished_th,
                                bmax, bmax_val) {

  bmax_adj <- (bmax - underfished_th) / (1 - bmax_val) + underfished_th
  message('bmax_adj = ', bmax_adj, '; bmax_val = ', bmax_val)

  fish_stat_df <- fish_stat_df %>%
    # group_by(stock) %>% ### grouping by stock will set b_max by max per stock, instead of max overall
    mutate(b_max     = max(b_bmsy, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(bPrime = NA,
           bPrime = ifelse(b_bmsy < overfished_th,
                           b_bmsy / overfished_th,       ### overfished stock
                           bPrime),
           bPrime = ifelse(b_bmsy >= overfished_th & b_bmsy < underfished_th,
                           1,                          ### appropriately fished stock
                           bPrime),
           bPrime = ifelse(b_bmsy >= underfished_th,
                           (bmax_adj - b_bmsy) / (bmax_adj - underfished_th), ### underfished stock
                           bPrime))

  return(fish_stat_df)
}


### The critical threshold should not affect healthy fisheries.
f_gradient <- function(f, over_f, under_f, fmax, fmin_val) {
  x <- ifelse(f <= over_f & f > under_f,                1, 0)
  x <- ifelse(f <= under_f, (f * (1 - fmin_val) / under_f + fmin_val), x)
  x <- ifelse(f > over_f,  (fmax - f) / (fmax - over_f), x)
  x <- ifelse(f > fmax, 0, x)
  return(x)
}

rescale_fprime_crit <- function(df,
                                bcrit, overfished_th,
                                underfishing_th, overfishing_th,
                                fmax, fmin_val) {

  bcritslope = 1/(overfished_th - bcrit) ### from (Bcrit, 0) to (overfished_th, 1)

  df <- df %>%
    mutate(fPrime = ifelse(b_bmsy < overfished_th & f_fmsy < fmax,
                           f_gradient(f_fmsy + (overfished_th - b_bmsy) * bcritslope,
                                      over_f = overfishing_th,
                                      under_f = underfishing_th,
                                      fmax = fmax,
                                      fmin_val = fmin_val),
                           NA),
           fPrime = ifelse(b_bmsy >= overfished_th & f_fmsy < fmax,
                           f_gradient(f_fmsy,
                                      over_f = overfishing_th,
                                      under_f = underfishing_th,
                                      fmax = fmax,
                                      fmin_val = fmin_val),
                           fPrime),
           fPrime = ifelse(is.na(fPrime), 0, fPrime)
    )

  return(df)
}


