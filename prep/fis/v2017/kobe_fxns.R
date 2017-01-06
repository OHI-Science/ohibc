
rescale_bprime <- function(fish_stat_df,
                           overfished_th  = 0.8,
                           underfished_th = 1.5) {

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
                           (b_max - b_bmsy) / (b_max - underfished_th), ### underfished stock
                           bPrime))

  return(fish_stat_df)
}



rescale_fprime <- function(fish_stat_df,
                           overfished_th  = 0.8,
                           underfishing_th = 0.8,
                           overfishing_th = 1.2) {

  fish_stat_df <- fish_stat_df %>%
    # group_by(stock) %>% ### grouping by stock will set f_max by max per stock, instead of max overall
    mutate(f_max     = max(f_fmsy, na.rm = TRUE),
           f_max_mod = f_max - overfishing_th) %>%
    ungroup() %>%
    mutate(fPrime = NA,
           fPrime = ifelse(b_bmsy < overfished_th & f_fmsy >= (b_bmsy + 1.5),
                           0,                     ### overfished, gross overfishing
                           fPrime),
           fPrime = ifelse(b_bmsy < overfished_th & f_fmsy >= (b_bmsy + 0.2) & f_fmsy < (b_bmsy + 1.5),
                           (b_bmsy + 1.5 - f_fmsy)/1.5, ### overfished, overfishing
                           fPrime),
           fPrime = ifelse(b_bmsy < overfished_th & (f_fmsy >= (b_bmsy - 0.2) & f_fmsy < (b_bmsy + 0.2)),
                           1,                     ### overfished, moderate fishing
                           fPrime),
           fPrime = ifelse(b_bmsy < overfished_th & f_fmsy < (b_bmsy - 0.2),
                           f_fmsy/(b_bmsy - 0.2), ### overfished, low fishing
                           fPrime),
           fPrime = ifelse(b_bmsy >= overfished_th & f_fmsy < underfishing_th,
                           f_fmsy/underfishing_th, ### NOT overfished, low fishing
                           fPrime),
           fPrime = ifelse(b_bmsy >= overfished_th & (f_fmsy >= underfishing_th & f_fmsy < overfishing_th),
                           1,                     ### NOT overfished, OK fishing
                           fPrime),
           fPrime = ifelse(b_bmsy >= overfished_th & f_fmsy >= overfishing_th,
                           (f_max - f_fmsy) / f_max_mod,  ### NOT overfished, overfishing
                           fPrime))

  return(fish_stat_df)
}


rescale_bprime_crit <- function(fish_stat_df,
                                overfished_th  = 0.8,
                                underfished_th = 1.5) {

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
                           (b_max - b_bmsy) / (b_max - underfished_th), ### underfished stock
                           bPrime))

  return(fish_stat_df)
}


### The critical threshold should not affect healthy fisheries.

rescale_fprime_crit <- function(df,
                                Bcrit = 0.4,
                                Bcrit_tol = c(.1, .1), ### [1] is - tol, [2] is + tol
                                overfished_th  = 0.8,
                                underfishing_th = 0.8,
                                overfishing_th = 1.2) {

  bcritplus = Bcrit + Bcrit_tol[2]; bcritminus = Bcrit - Bcrit_tol[1]
  bcritslope = 1/(1 - Bcrit)
  bcritslope1 = 1/(1 - bcritminus)
  # message('bcritplus = ', bcritplus, '; bcritminus = ', bcritminus, '; bcritslope = ', round(bcritslope, 3))

  df <- df %>%
    # group_by(stock) %>% ### grouping by stock will set f_max by max per stock, instead of max overall
    mutate(f_max     = max(f_fmsy, na.rm = TRUE),
           f_max_mod = f_max - overfishing_th) %>%
    ungroup() %>%
    mutate(fPrime = NA,
           fPrime = ifelse(b_bmsy < bcritminus,
                           0,                     ### overfished, gross overfishing
                           fPrime),
           fPrime = ifelse(b_bmsy < overfished_th &
                             b_bmsy >= bcritminus &
                             f_fmsy >= bcritslope * (b_bmsy - bcritminus),
                           (f_max - f_fmsy) / (f_max - (overfishing_th * (b_bmsy - bcritminus) / .7)) *  ((b_bmsy - bcritminus) / .7),
                           fPrime),
           fPrime = ifelse(b_bmsy < overfished_th &
                             f_fmsy >= bcritslope * (b_bmsy - bcritplus) &
                             f_fmsy < bcritslope * (b_bmsy - bcritminus),
                           1,                     ### overfished, moderate fishing
                           fPrime),
           fPrime = ifelse(b_bmsy < overfished_th & f_fmsy < bcritslope * (b_bmsy - bcritplus),
                           f_fmsy/(bcritslope * (b_bmsy - bcritplus)), ### overfished, low fishing
                           fPrime),
           fPrime = ifelse(b_bmsy >= overfished_th & f_fmsy < underfishing_th,
                           f_fmsy/underfishing_th, ### NOT overfished, low fishing
                           fPrime),
           fPrime = ifelse(b_bmsy >= overfished_th & f_fmsy >= underfishing_th & f_fmsy < overfishing_th,
                           1,                     ### NOT overfished, OK fishing
                           fPrime),
           fPrime = ifelse(b_bmsy >= overfished_th & f_fmsy >= overfishing_th,
                           (f_max - f_fmsy) / f_max_mod,  ### NOT overfished, overfishing
                           fPrime))

  return(df)
}

generate_kobe_df <- function(f_fmsy_max = 2.5,
                             b_bmsy_max = 3.5,
                             reso       = 0.01) {

  kobe <- data.frame(stock  = 1,
                     f_fmsy = rep(seq(0, f_fmsy_max, reso), each  = b_bmsy_max/reso + 1),
                     b_bmsy = rep(seq(0, b_bmsy_max, reso), times = f_fmsy_max/reso + 1))

  kobe <- kobe %>%
    rescale_bprime_crit(overfished_th = 0.8) %>%
    rescale_fprime_crit(overfished_th = 0.8) %>%
    mutate(x = (fPrime + bPrime)/2)

  return(kobe)
}

generate_kobe_geom_df <- function(f_fmsy_max = 2.5,
                             b_bmsy_max = 3.5,
                             reso       = 0.01) {

  kobe <- data.frame(stock  = 1,
                     f_fmsy = rep(seq(0, f_fmsy_max, reso), each  = b_bmsy_max/reso + 1),
                     b_bmsy = rep(seq(0, b_bmsy_max, reso), times = f_fmsy_max/reso + 1))

  kobe <- kobe %>%
    rescale_bprime_crit(overfished_th = 0.8) %>%
    rescale_fprime_crit(overfished_th = 0.8) %>%
    mutate(x = (fPrime * bPrime))

  return(kobe)
}
