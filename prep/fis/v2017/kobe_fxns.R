
rescale_bprime_crit <- function(fish_stat_df,
                                overfished_th  = 0.8,
                                underfished_th = 1.5,
                                bmax = 3.0,
                                bmax_val = 0) {

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
                           bPrime),
           bPrime = ifelse(b_bmsy >= bmax, bmax_val, bPrime),
           bPrime = ifelse(bPrime < 0, 0, bPrime))

  return(fish_stat_df)
}


### The critical threshold should not affect healthy fisheries.
f_gradient <- function(f, over_f, under_f, fmax, fmin_val) {
  x <- ifelse(f < over_f & f > under_f,                1, NA)
  x <- ifelse(f <= under_f, (f * (1 - fmin_val) / under_f + fmin_val), x)
  x <- ifelse(f >= over_f,  (fmax - f) / (fmax - over_f), x)
  x <- ifelse(f > fmax, NA, x)
  return(x)
}

rescale_fprime_crit <- function(df,
                                Bcrit = 0.4,
                                Bcautious = 0.8,
                                underfishing_th = 0.8,
                                overfishing_th = 1.2,
                                fmax  = 2.0,
                                fmin_val = 0) {

  bcritslope = 1/(Bcautious - Bcrit) ### from (Bcrit, 0) to (Bcautious, 1)
  message('bcritslope = ', round(bcritslope, 3))

  df <- df %>%
    mutate(fPrime = ifelse(b_bmsy < Bcautious & f_fmsy < fmax,
                           f_gradient(f_fmsy + (Bcautious - b_bmsy) * bcritslope,
                                      over_f = overfishing_th,
                                      under_f = underfishing_th,
                                      fmax = fmax,
                                      fmin_val = .3),
                           NA),
           fPrime = ifelse(b_bmsy >= Bcautious & f_fmsy < fmax,
                           f_gradient(f_fmsy,
                                      over_f = overfishing_th,
                                      under_f = underfishing_th,
                                      fmax = fmax,
                                      fmin_val = .3),
                           fPrime),
           fPrime = ifelse(is.na(fPrime), 0, fPrime)
    )

  return(df)
}

generate_kobe_df <- function(f_fmsy_max = 2.5,
                             b_bmsy_max = 3.0,
                             reso       = 0.01,
                             bmax_val   = 0,
                             fmin_val   = 0,
                             weighting_b = 1) {

  kobe_raw <- data.frame(stock  = 1,
                     f_fmsy = rep(seq(0, f_fmsy_max, reso), each  = round(b_bmsy_max/reso) + 1),
                     b_bmsy = rep(seq(0, b_bmsy_max, reso), times = round(f_fmsy_max/reso) + 1))

  kobe <- kobe_raw %>%
    rescale_bprime_crit(bmax_val = bmax_val) %>%
    rescale_fprime_crit(fmin_val = fmin_val) %>%
    mutate(x_geom  = (fPrime * bPrime),
           x_arith = (fPrime + bPrime) / 2)

  return(kobe)
}

