require(tidyverse)
require(RPostgres)
require(bizdays)
require(zoo)
setwd("D:/ColorFinanceWords")


meta <- read.csv('./data/event_data.csv') %>% 
  mutate(datadate = as.Date(datadate),
         callDate = as.Date(callDate))

sample <- meta
n_pull <- -1

wrds_user <- "lzy2lzz"
wrds_password <- "playerpassword"

wrds <- dbConnect(Postgres(),
                  host     = "wrds-pgdata.wharton.upenn.edu",
                  port     = 9737,
                  user     = wrds_user,
                  password = wrds_password,
                  dbname   = "wrds",
                  sslmode  = "require")

# load data from wrds
## price, return, volume, shares outstandings and exchange code
res <- dbSendQuery(wrds, "select date, permno, prc, ret, vol, shrout, hexcd
                          from crsp.dsf 
                          where date between '01/31/2004' and '12/31/2022'")

crsp_dsf <- dbFetch(res, n = n_pull); dbClearResult(res)

## value weighted market index
res <- dbSendQuery(wrds, "select date, vwretd
                          from crsp.dsi
                          where date between '01/31/2005' and '12/31/2022'")

crsp_dsi <- dbFetch(res, n = n_pull); dbClearResult(res)

## book_value and earnings per share
res <- dbSendQuery(wrds, "select gvkey, datadate, epspxq, ceqq
                          from comp.fundq
                          where datadate between '01/31/2004' and '12/31/2022'")

comp_fundq <- dbFetch(res, n = n_pull); dbClearResult(res)

## sic code
res <- dbSendQuery(wrds, "select date, permno, siccd
                          from crsp.dse
                          where date between '01/31/2004' and '12/31/2022'")

crsp_dse <- dbFetch(res, n = n_pull); dbClearResult(res)

res <- dbSendQuery(wrds, "select permno, siccd
                          from crsp.stocknames")

crsp_stocknames <- dbFetch(res, n = n_pull); dbClearResult(res)

## ff alpha
beta_suite <- read_csv('data/ff_alpha.csv')

# Dependent Variable: event period excess return
date_sample <- crsp_dsi['date'] %>% distinct()
cal <- create.calendar('cal', weekdays = c("saturday", "sunday"), financial = T)

date_1 <- meta['callDate'] %>% 
  distinct() %>% 
  mutate(date = callDate - 1) %>% 
  mutate(date = preceding(date, cal))

date_2 <- date_1 %>% 
  mutate(date = callDate)

date_3 <- date_1 %>% 
  mutate(date = callDate + 1) %>% 
  mutate(date = following(date, cal))

date_4 <- date_1 %>% 
  mutate(date = callDate + 2) %>% 
  mutate(date = following(date, cal))

tw <- date_1 %>% 
  bind_rows(date_2, date_3, date_4)

excess_return <- meta %>% 
  select(permno, gvkey, callDate, datadate) %>% 
  left_join(tw, by='callDate') %>% 
  distinct() %>% 
  left_join(select(crsp_dsf, date, permno, ret), by=c('permno', 'date')) %>% 
  left_join(crsp_dsi, by=c('date')) %>% 
  mutate(ret = ret + 1,
         vwretd = vwretd + 1) %>% 
  group_by(permno, callDate) %>% 
  summarise(filing.period.excess.return = prod(ret, na.rm = T) - prod(vwretd, na.rm = T))

# Independent variables
## size
size <- crsp_dsf %>% 
  select(permno, date, prc) %>% 
  left_join(select(crsp_dsf, permno, date, shrout),
            by=c('permno', 'date')) %>% 
  mutate(size = abs(prc)*shrout) %>% 
  select(permno, date, size) %>% 
  group_by(permno) %>% 
  fill(size, .direction = 'down') %>% 
  drop_na()

## book_to_market
book_value <- meta %>% 
  left_join(select(comp_fundq, gvkey, datadate, ceqq) %>% 
              fill(ceqq, .direction = 'down') %>% 
              mutate(gvkey = as.numeric(gvkey)),
            by=c('gvkey', 'datadate')) %>% 
  mutate(last_year = year(datadate) - 1)

market_value <- size %>% 
  mutate(last_year = year(date) - 1) %>% 
  distinct() %>% 
  group_by(permno, last_year) %>% 
  summarise(market_value = last(size, order_by = date))

book_to_market <- market_value %>% 
  left_join(book_value, by=c('permno', 'last_year')) %>% 
  mutate(bm = ceqq / market_value,
         callDate = as.Date(callDate)) %>% 
  select(permno, callDate, bm)

## share turnover
share_turnover <- crsp_dsf %>% 
  select(date, permno, vol) %>% 
  group_by(permno) %>% 
  mutate(vol = lag(vol, n=6L)) %>% 
  mutate(vol_sum = rollsum(vol, 246L, na.pad = T, align = 'right')) %>% 
  left_join(select(crsp_dsf, permno, date, shrout) %>% 
              group_by(permno) %>%
              fill(shrout, .direction = 'down') %>% 
              ungroup(),
            by=c('permno', 'date')) %>% 
  mutate(share.turnover = vol_sum / shrout) %>% 
  select(date, permno, share.turnover)

## pre ffalpha (from Beta Suite by WRDS)
ffalpha <- beta_suite %>% 
  group_by(PERMNO) %>% 
  transmute(permno = PERMNO,
            date = DATE,
            Pre_FFAlpha = lag(alpha, 6L)) %>% 
  ungroup() %>% 
  select(-PERMNO)

## nasdaq dummy
nasdaq <- crsp_dsf %>% 
  select(permno, date, hexcd) %>% 
  mutate(nasdaq.dummy = if_else(hexcd == 3, 1, 0)) %>% 
  select(-hexcd) %>% 
  fill(nasdaq.dummy, 0)

## standardized unexpected earnings
sue <- comp_fundq %>% 
  select(gvkey, datadate, epspxq) %>% 
  group_by(gvkey) %>% 
  mutate(epspxq_last = lag(epspxq, 4L, order_by = datadate)) %>% 
  mutate(ue = epspxq - epspxq_last) %>% 
  mutate(std_ue = sd(ue, na.rm = T)) %>% 
  mutate(mean_ue = mean(ue, na.rm = T)) %>% 
  mutate(sue = (ue - mean_ue) / std_ue) %>% 
  select(gvkey, datadate, sue) %>% 
  mutate(gvkey = as.numeric(gvkey))

## sic code
sic <- crsp_stocknames %>% 
  distinct() %>% 
  mutate(FF.49 = case_when(
    (siccd >= 100 & siccd <= 199) | (siccd >= 200 & siccd <= 299) | (siccd >= 700 & siccd <= 799) | (siccd >= 910 & siccd <= 919) | (siccd == 2048) ~ 1,
    (siccd >= 2000 & siccd <= 2009) | (siccd >= 2010 & siccd <= 2019) | (siccd >= 2020 & siccd <= 2029) | (siccd >= 2030 & siccd <= 2039) | (siccd >= 2040 & siccd <= 2046) | (siccd >= 2050 & siccd <= 2059) | (siccd >= 2060 & siccd <= 2063) | (siccd >= 2070 & siccd <= 2079) | (siccd >= 2090 & siccd <= 2092) | (siccd == 2095) | (siccd >= 2098 & siccd <= 2099) ~ 2,
    (siccd >= 2064 & siccd <= 2068) | (siccd == 2086) | (siccd == 2087) | (siccd == 2096) | (siccd == 2097) ~ 3,
    (siccd == 2080) | (siccd == 2082) | (siccd == 2083) | (siccd == 2084) | (siccd == 2085) ~ 4,
    (siccd >= 2100 & siccd <= 2199) ~ 5,
    (siccd >= 920 & siccd <= 999) | (siccd >= 3650 & siccd <= 3651) | (siccd == 3652) | (siccd == 3732) | (siccd >= 3930 & siccd <= 3931) | (siccd >= 3940 & siccd <= 3949) ~ 6,
    (siccd >= 7800 & siccd <= 7829) | (siccd >= 7830 & siccd <= 7833) | (siccd >= 7840 & siccd <= 7841) | (siccd == 7900) | (siccd >= 7910 & siccd <= 7911) | (siccd >= 7920 & siccd <= 7929) | (siccd >= 7930 & siccd <= 7933) | (siccd >= 7940 & siccd <= 7949) | (siccd == 7980) | (siccd >= 7990 & siccd <= 7999) ~ 7,
    (siccd >= 2700 & siccd <= 2709) | (siccd >= 2710 & siccd <= 2719) | (siccd >= 2720 & siccd <= 2729) | (siccd >= 2730 & siccd <= 2739) | (siccd >= 2740 & siccd <= 2749) | (siccd >= 2770 & siccd <= 2771) | (siccd >= 2780 & siccd <= 2789) | (siccd >= 2790 & siccd <= 2799) ~ 8,
    (siccd == 2047) | (siccd >= 2391 & siccd <= 2392) | (siccd >= 2510 & siccd <= 2519) | (siccd >= 2590 & siccd <= 2599) | (siccd >= 2840 & siccd <= 2843) | (siccd == 2844) | (siccd >= 3160 & siccd <= 3161) | (siccd >= 3170 & siccd <= 3171) | (siccd == 3172) | (siccd >= 3190 & siccd <= 3199) | (siccd == 3229) | (siccd == 3260) | (siccd >= 3262 & siccd <= 3263) | (siccd == 3269) | (siccd >= 3230 & siccd <= 3231) | (siccd >= 3630 & siccd <= 3639) | (siccd >= 3750 & siccd <= 3751) | (siccd == 3800) | (siccd >= 3860 & siccd <= 3861) | (siccd >= 3870 & siccd <= 3873) | (siccd >= 3910 & siccd <= 3911) | (siccd == 3914) | (siccd == 3915) | (siccd >= 3960 & siccd <= 3962) | (siccd == 3991) | (siccd == 3995) ~ 9,
    (siccd >= 2300 & siccd <= 2390) | (siccd >= 3020 & siccd <= 3021) | (siccd >= 3100 & siccd <= 3111) | (siccd >= 3130 & siccd <= 3131) | (siccd >= 3140 & siccd <= 3149) | (siccd >= 3150 & siccd <= 3151) | (siccd >= 3963 & siccd <= 3965) ~ 10,
    (siccd >= 8000 & siccd <= 8099) ~ 11,
    (siccd == 3693) | (siccd >= 3840 & siccd <= 3849) | (siccd >= 3850 & siccd <= 3851) ~ 12,
    siccd %in% c(2830, 2831, 2833, 2834, 2835, 2836) ~ 13,
    (siccd >= 2800 & siccd <= 2809) | (siccd >= 2810 & siccd <= 2819) | (siccd >= 2820 & siccd <= 2829) | (siccd >= 2850 & siccd <= 2859) | (siccd >= 2860 & siccd <= 2869) | (siccd >= 2870 & siccd <= 2879) | (siccd >= 2890 & siccd <= 2899) ~ 14,
    siccd %in% c(3031, 3041) | (siccd >= 3050 & siccd <= 3053) | (siccd >= 3060 & siccd <= 3069) | (siccd >= 3070 & siccd <= 3079) | (siccd >= 3080 & siccd <= 3089) | (siccd >= 3090 & siccd <= 3099) ~ 15,
    (siccd >= 2200 & siccd <= 2269) | (siccd >= 2270 & siccd <= 2279) | (siccd >= 2280 & siccd <= 2284) | (siccd >= 2290 & siccd <= 2295) | (siccd == 2297) | (siccd == 2298) | (siccd == 2299) | (siccd >= 2393 & siccd <= 2395) | (siccd >= 2397 & siccd <= 2399) ~ 16,
    (siccd >= 800 & siccd <= 899) | (siccd >= 2400 & siccd <= 2439) | (siccd >= 2450 & siccd <= 2459) | (siccd >= 2490 & siccd <= 2499) | (siccd >= 2660 & siccd <= 2661) | (siccd >= 2950 & siccd <= 2952) | (siccd == 3200) | (siccd >= 3210 & siccd <= 3211) | (siccd >= 3240 & siccd <= 3241) | (siccd >= 3250 & siccd <= 3259) | (siccd == 3261) | (siccd == 3264) | (siccd >= 3270 & siccd <= 3275) | (siccd >= 3280 & siccd <= 3281) | (siccd >= 3290 & siccd <= 3293) | (siccd >= 3295 & siccd <= 3299) | (siccd >= 3420 & siccd <= 3429) | (siccd >= 3430 & siccd <= 3433) | (siccd >= 3440 & siccd <= 3441) | (siccd == 3442) | (siccd == 3446) | (siccd == 3448) | (siccd == 3449) | (siccd >= 3450 & siccd <= 3451) | (siccd == 3452) | (siccd >= 3490 & siccd <= 3499) | (siccd == 3996) ~ 17,
    (siccd >= 1500 & siccd <= 1511) | (siccd >= 1520 & siccd <= 1529) | (siccd >= 1530 & siccd <= 1539) | (siccd >= 1540 & siccd <= 1549) | (siccd >= 1600 & siccd <= 1699) | (siccd >= 1700 & siccd <= 1799) ~ 18,
    (siccd == 3300) | (siccd >= 3310 & siccd <= 3317) | (siccd >= 3320 & siccd <= 3325) | (siccd >= 3330 & siccd <= 3339) | (siccd >= 3340 & siccd <= 3341) | (siccd >= 3350 & siccd <= 3357) | (siccd >= 3360 & siccd <= 3369) | (siccd >= 3370 & siccd <= 3379) | (siccd >= 3390 & siccd <= 3399) ~ 19,
    (siccd == 3400) | (siccd == 3443) | (siccd == 3444) | (siccd >= 3460 & siccd <= 3469) | (siccd >= 3470 & siccd <= 3479) ~ 20,
    (siccd >= 3510 & siccd <= 3519) | (siccd >= 3520 & siccd <= 3529) | (siccd == 3530) | (siccd == 3531) | (siccd == 3532) | (siccd == 3533) | (siccd == 3534) | (siccd == 3535) | (siccd == 3536) | (siccd == 3538) | (siccd >= 3540 & siccd <= 3549) | (siccd >= 3550 & siccd <= 3559) | (siccd >= 3560 & siccd <= 3569) | (siccd == 3580) | (siccd == 3581) | (siccd == 3582) | (siccd == 3585) | (siccd == 3586) | (siccd == 3589) | (siccd >= 3590 & siccd <= 3599) ~ 21,
    (siccd == 3600) | (siccd >= 3610 & siccd <= 3613) | (siccd >= 3620 & siccd <= 3621) | (siccd >= 3623 & siccd <= 3629) | (siccd >= 3640 & siccd <= 3644) | (siccd == 3645) | (siccd == 3646) | (siccd == 3648) | (siccd == 3660) | (siccd == 3690) | (siccd >= 3691 & siccd <= 3692) | (siccd == 3699) ~ 22,
    (siccd == 2296) | (siccd == 2396) | (siccd >= 3010 & siccd <= 3011) | (siccd == 3537) | (siccd == 3647) | (siccd == 3694) | (siccd == 3700) | (siccd == 3710) | (siccd == 3711) | (siccd == 3713) | (siccd == 3714) | (siccd == 3715) | (siccd == 3716) | (siccd == 3792) | (siccd >= 3790 & siccd <= 3791) | (siccd == 3799) ~ 23,
    (siccd == 3720) | (siccd == 3721) | (siccd >= 3723 & siccd <= 3724) | (siccd == 3725) | (siccd >= 3728 & siccd <= 3729) ~ 24,
    (siccd >= 3730 & siccd <= 3731) | (siccd >= 3740 & siccd <= 3743) ~ 25,
    (siccd >= 3760 & siccd <= 3769) | (siccd == 3795) | (siccd >= 3480 & siccd <= 3489) ~ 26,
    (siccd >= 1040 & siccd <= 1049) ~ 27,
    (siccd >= 1000 & siccd <= 1009) | (siccd >= 1010 & siccd <= 1019) | (siccd >= 1020 & siccd <= 1029) | (siccd >= 1030 & siccd <= 1039) | (siccd >= 1050 & siccd <= 1059) | (siccd >= 1060 & siccd <= 1069) | (siccd >= 1070 & siccd <= 1079) | (siccd >= 1080 & siccd <= 1089) | (siccd >= 1090 & siccd <= 1099) | (siccd >= 1100 & siccd <= 1119) | (siccd >= 1400 & siccd <= 1499) ~ 28,
    (siccd >= 1200 & siccd <= 1299) ~ 29,
    (siccd == 1300) | (siccd >= 1310 & siccd <= 1319) | (siccd >= 1320 & siccd <= 1329) | (siccd >= 1330 & siccd <= 1339) | (siccd >= 1370 & siccd <= 1379) | (siccd == 1380) | (siccd == 1381) | (siccd == 1382) | (siccd == 1389) | (siccd >= 2900 & siccd <= 2912) | (siccd >= 2990 & siccd <= 2999) ~ 30,
    (siccd == 4900) | (siccd >= 4910 & siccd <= 4911) | (siccd >= 4920 & siccd <= 4922) | (siccd == 4923) | (siccd >= 4924 & siccd <= 4925) | (siccd >= 4930 & siccd <= 4931) | (siccd == 4932) | (siccd == 4939) | (siccd >= 4940 & siccd <= 4942) ~ 31,
    (siccd == 4800) | (siccd >= 4810 & siccd <= 4813) | (siccd >= 4820 & siccd <= 4822) | (siccd >= 4830 & siccd <= 4839) | (siccd >= 4840 & siccd <= 4841) | (siccd >= 4880 & siccd <= 4889) | (siccd == 4890) | (siccd == 4891) | (siccd == 4892) | (siccd == 4899) ~ 32,
    (siccd >= 7020 & siccd <= 7021) | (siccd >= 7030 & siccd <= 7033) | (siccd == 7200) | (siccd >= 7210 & siccd <= 7212) | (siccd == 7214) | (siccd >= 7215 & siccd <= 7216) | (siccd == 7217) | (siccd == 7219) | (siccd >= 7220 & siccd <= 7221) | (siccd >= 7230 & siccd <= 7231) | (siccd >= 7240 & siccd <= 7241) | (siccd >= 7250 & siccd <= 7251) | (siccd >= 7260 & siccd <= 7269) | (siccd >= 7270 & siccd <= 7290) | (siccd == 7291) | (siccd >= 7292 & siccd <= 7299) | (siccd == 7395) | (siccd == 7500) | (siccd >= 7520 & siccd <= 7529) | (siccd >= 7530 & siccd <= 7539) | (siccd >= 7540 & siccd <= 7549) | (siccd == 7600) | (siccd == 7620) | (siccd == 7622) | (siccd == 7623) | (siccd == 7629) | (siccd >= 7630 & siccd <= 7631) | (siccd >= 7640 & siccd <= 7641) | (siccd >= 7690 & siccd <= 7699) | (siccd >= 8100 & siccd <= 8199) | (siccd >= 8200 & siccd <= 8299) | (siccd >= 8300 & siccd <= 8399) | (siccd >= 8400 & siccd <= 8499) | (siccd >= 8600 & siccd <= 8699) | (siccd >= 8800 & siccd <= 8899) | (siccd >= 7510 & siccd <= 7515) ~ 33,
    (siccd >= 2750 & siccd <= 2759) | (siccd == 3993) | (siccd == 7218) | (siccd == 7300) | (siccd >= 7310 & siccd <= 7319) | (siccd >= 7320 & siccd <= 7329) | (siccd >= 7330 & siccd <= 7339) | (siccd >= 7340 & siccd <= 7342) | (siccd == 7349) | (siccd >= 7350 & siccd <= 7351) | (siccd == 7352) | (siccd == 7353) | (siccd == 7359) | (siccd >= 7360 & siccd <= 7369) | (siccd == 7374) | (siccd == 7376) | (siccd == 7377) | (siccd == 7378) | (siccd == 7379) | (siccd == 7380) | (siccd >= 7381 & siccd <= 7382) | (siccd == 7383) | (siccd == 7384) | (siccd == 7385) | (siccd == 7389) | (siccd >= 7390 & siccd <= 7391) | (siccd == 7392) | (siccd == 7393) | (siccd == 7394) | (siccd == 7396) | (siccd == 7397) | (siccd == 7399) | (siccd == 7519) | (siccd == 8700) | (siccd >= 8710 & siccd <= 8713) | (siccd >= 8720 & siccd <= 8721) | (siccd >= 8730 & siccd <= 8734) | (siccd >= 8740 & siccd <= 8748) | (siccd >= 8900 & siccd <= 8910) | (siccd == 8911) | (siccd >= 8920 & siccd <= 8999) | (siccd >= 4220 & siccd <= 4229) ~ 34,
    (siccd >= 3570 & siccd <= 3579) | (siccd == 3680) | (siccd == 3681) | (siccd == 3682) | (siccd == 3683) | (siccd == 3684) | (siccd == 3685) | (siccd == 3686) | (siccd == 3687) | (siccd == 3688) | (siccd == 3689) | (siccd == 3695) ~ 35,
    (siccd >= 7370 & siccd <= 7372) | (siccd == 7375) | (siccd == 7373) ~ 36,
    (siccd == 3622) | (siccd == 3661) | (siccd == 3662) | (siccd == 3663) | (siccd == 3664) | (siccd == 3665) | (siccd == 3666) | (siccd == 3669) | (siccd >= 3670 & siccd <= 3679) | (siccd == 3810) | (siccd == 3812) ~ 37,
    (siccd == 3811) | (siccd == 3820) | (siccd == 3821) | (siccd == 3822) | (siccd == 3823) | (siccd == 3824) | (siccd == 3825) | (siccd == 3826) | (siccd == 3827) | (siccd == 3829) | (siccd >= 3830 & siccd <= 3839) ~ 38,
    (siccd >= 2520 & siccd <= 2549) | (siccd >= 2600 & siccd <= 2639) | (siccd >= 2670 & siccd <= 2699) | (siccd >= 2760 & siccd <= 2761) | (siccd >= 3950 & siccd <= 3955) ~ 39,
    (siccd >= 2440 & siccd <= 2449) | (siccd >= 2640 & siccd <= 2659) | (siccd >= 3220 & siccd <= 3221) | (siccd >= 3410 & siccd <= 3412) ~ 40,
    (siccd >= 4000 & siccd <= 4013) | (siccd >= 4040 & siccd <= 4049) | (siccd == 4100) | (siccd >= 4110 & siccd <= 4119) | (siccd >= 4120 & siccd <= 4121) | (siccd >= 4130 & siccd <= 4131) | (siccd >= 4140 & siccd <= 4142) | (siccd >= 4150 & siccd <= 4151) | (siccd >= 4170 & siccd <= 4173) | (siccd >= 4190 & siccd <= 4199) | (siccd == 4200) | (siccd >= 4210 & siccd <= 4219) | (siccd >= 4230 & siccd <= 4231) | (siccd >= 4240 & siccd <= 4249) | (siccd >= 4400 & siccd <= 4499) | (siccd >= 4500 & siccd <= 4599) | (siccd >= 4600 & siccd <= 4699) | (siccd == 4700) | (siccd >= 4710 & siccd <= 4712) | (siccd >= 4720 & siccd <= 4729) | (siccd >= 4730 & siccd <= 4739) | (siccd >= 4740 & siccd <= 4749) | (siccd == 4780) | (siccd == 4782) | (siccd == 4783) | (siccd == 4784) | (siccd == 4785) | (siccd == 4789) ~ 41,
    (siccd == 5000) | (siccd >= 5010 & siccd <= 5015) | (siccd >= 5020 & siccd <= 5023) | (siccd >= 5030 & siccd <= 5039) | (siccd >= 5040 & siccd <= 5042) | (siccd == 5043) | (siccd == 5044) | (siccd == 5045) | (siccd == 5046) | (siccd == 5047) | (siccd == 5048) | (siccd == 5049) | (siccd >= 5050 & siccd <= 5059) | (siccd == 5060) | (siccd == 5063) | (siccd == 5064) | (siccd == 5065) | (siccd >= 5070 & siccd <= 5078) | (siccd == 5080) | (siccd == 5081) | (siccd == 5082) | (siccd == 5083) | (siccd == 5084) | (siccd == 5085) | (siccd >= 5086 & siccd <= 5087) | (siccd == 5088) | (siccd == 5090) | (siccd >= 5091 & siccd <= 5092) | (siccd == 5093) | (siccd == 5094) | (siccd == 5099) | (siccd == 5100) | (siccd >= 5110 & siccd <= 5113) | (siccd >= 5120 & siccd <= 5122) | (siccd >= 5130 & siccd <= 5139) | (siccd >= 5140 & siccd <= 5149) | (siccd >= 5150 & siccd <= 5159) | (siccd >= 5160 & siccd <= 5169) | (siccd >= 5170 & siccd <= 5172) | (siccd >= 5180 & siccd <= 5182) | (siccd >= 5190 & siccd <= 5199) ~ 42,
    (siccd == 5200) | (siccd >= 5210 & siccd <= 5219) | (siccd >= 5220 & siccd <= 5229) | (siccd >= 5230 & siccd <= 5231) | (siccd >= 5250 & siccd <= 5251) | (siccd >= 5260 & siccd <= 5261) | (siccd >= 5270 & siccd <= 5271) | (siccd == 5300) | (siccd >= 5310 & siccd <= 5311) | (siccd == 5320) | (siccd >= 5330 & siccd <= 5331) | (siccd == 5334) | (siccd >= 5340 & siccd <= 5349) | (siccd >= 5390 & siccd <= 5399) | (siccd == 5400) | (siccd >= 5410 & siccd <= 5411) | (siccd == 5412) | (siccd >= 5420 & siccd <= 5429) | (siccd >= 5430 & siccd <= 5439) | (siccd >= 5440 & siccd <= 5449) | (siccd >= 5450 & siccd <= 5459) | (siccd >= 5460 & siccd <= 5469) | (siccd >= 5490 & siccd <= 5499) | (siccd == 5500) | (siccd >= 5510 & siccd <= 5529) | (siccd >= 5530 & siccd <= 5539) | (siccd >= 5540 & siccd <= 5549) | (siccd >= 5550 & siccd <= 5559) | (siccd >= 5560 & siccd <= 5569) | (siccd >= 5570 & siccd <= 5579) | (siccd >= 5590 & siccd <= 5599) | (siccd >= 5600 & siccd <= 5699) | (siccd == 5700) | (siccd >= 5710 & siccd <= 5719) | (siccd >= 5720 & siccd <= 5722) | (siccd >= 5730 & siccd <= 5733) | (siccd == 5734) | (siccd == 5735) | (siccd == 5736) | (siccd == 5750) | (siccd >= 5751 & siccd <= 5799) | (siccd == 5900) | (siccd >= 5910 & siccd <= 5912) | (siccd >= 5920 & siccd <= 5929) | (siccd >= 5930 & siccd <= 5932) | (siccd == 5940) | (siccd == 5941) | (siccd == 5942) | (siccd == 5943) | (siccd == 5944) | (siccd == 5945) | (siccd == 5946) | (siccd == 5947) | (siccd == 5948) | (siccd == 5949) | (siccd >= 5950 & siccd <= 5959) | (siccd >= 5960 & siccd <= 5969) | (siccd >= 5970 & siccd <= 5979) | (siccd >= 5980 & siccd <= 5989) | (siccd == 5990) | (siccd == 5992) | (siccd == 5993) | (siccd == 5994) | (siccd == 5995) | (siccd == 5999) ~ 43,
    (siccd >= 5800 & siccd <= 5819) | (siccd >= 5820 & siccd <= 5829) | (siccd >= 5890 & siccd <= 5899) | (siccd == 7000) | (siccd >= 7010 & siccd <= 7019) | (siccd >= 7040 & siccd <= 7049) | (siccd == 7213) ~ 44,
    (siccd >= 6000 & siccd <= 6000) | (siccd >= 6010 & siccd <= 6019) | (siccd >= 6020 & siccd <= 6020) | (siccd >= 6021 & siccd <= 6021) | (siccd >= 6022 & siccd <= 6022) | (siccd >= 6023 & siccd <= 6024) | (siccd >= 6025 & siccd <= 6025) | (siccd >= 6026 & siccd <= 6026) | (siccd >= 6027 & siccd <= 6027) | (siccd >= 6028 & siccd <= 6029) | (siccd >= 6030 & siccd <= 6036) | (siccd >= 6040 & siccd <= 6059) | (siccd >= 6060 & siccd <= 6062) | (siccd >= 6080 & siccd <= 6082) | (siccd >= 6090 & siccd <= 6099) | (siccd == 6100) | (siccd >= 6110 & siccd <= 6111) | (siccd >= 6112 & siccd <= 6113) | (siccd >= 6120 & siccd <= 6129) | (siccd >= 6130 & siccd <= 6139) | (siccd >= 6140 & siccd <= 6149) | (siccd >= 6150 & siccd <= 6159) | (siccd >= 6160 & siccd <= 6169) | (siccd >= 6170 & siccd <= 6179) | (siccd >= 6190 & siccd <= 6199) ~ 45,
    (siccd >= 6300 & siccd <= 6300) | (siccd >= 6310 & siccd <= 6319) | (siccd >= 6320 & siccd <= 6329) | (siccd >= 6330 & siccd <= 6331) | (siccd >= 6350 & siccd <= 6351) | (siccd >= 6360 & siccd <= 6361) | (siccd >= 6370 & siccd <= 6379) | (siccd >= 6390 & siccd <= 6399) | (siccd >= 6400 & siccd <= 6411) ~ 46,
    (siccd >= 6500 & siccd <= 6500) | (siccd >= 6510 & siccd <= 6510) | (siccd >= 6512 & siccd <= 6512) | (siccd >= 6513 & siccd <= 6513) | (siccd >= 6514 & siccd <= 6514) | (siccd >= 6515 & siccd <= 6515) | (siccd >= 6517 & siccd <= 6519) | (siccd >= 6520 & siccd <= 6529) | (siccd >= 6530 & siccd <= 6531) | (siccd >= 6532 & siccd <= 6532) | (siccd >= 6540 & siccd <= 6541) | (siccd >= 6550 & siccd <= 6553) | (siccd >= 6590 & siccd <= 6599) | (siccd >= 6610 & siccd <= 6611) ~ 47,
    (siccd >= 6200 & siccd <= 6299) | (siccd == 6700) | (siccd >= 6710 & siccd <= 6719) | (siccd >= 6720 & siccd <= 6722) | (siccd == 6723) | (siccd == 6724) | (siccd == 6725) | (siccd == 6726) | (siccd >= 6730 & siccd <= 6733) | (siccd >= 6740 & siccd <= 6779) | (siccd >= 6790 & siccd <= 6792) | (siccd == 6793) | (siccd == 6794) | (siccd == 6795) | (siccd == 6798) | (siccd == 6799) ~ 48,
    (siccd >= 4950 & siccd <= 4959) | (siccd >= 4960 & siccd <= 4961) | (siccd >= 4970 & siccd <= 4971) | (siccd >= 4990 & siccd <= 4991) ~ 49
    )) %>% 
  select(permno, FF.49) %>% 
  distinct()


# Merge datasets
meta <- meta %>% 
  left_join(excess_return, by=c('permno', 'callDate')) %>% 
  distinct() %>%  
  left_join(size, by=c('permno', 'callDate' = 'date')) %>% 
  distinct() %>% 
  left_join(book_to_market, by=c('permno', 'callDate')) %>% 
  distinct() %>% 
  left_join(share_turnover, by=c('permno', 'callDate' = 'date')) %>% 
  distinct() %>% 
  left_join(ffalpha, by=c('permno', 'callDate' = 'date')) %>% 
  distinct() %>% 
  left_join(nasdaq, by=c('permno', 'callDate' = 'date')) %>% 
  distinct() %>% 
  left_join(sue, by=c('gvkey', 'datadate')) %>% 
  distinct() %>% 
  left_join(sic, by='permno') %>% 
  distinct() %>% 
  mutate(yearQuarter = str_c(fyearq, '-', quarter(datadate)))

meta <- meta %>% 
  group_by(permno) %>% 
  distinct(eventid, .keep_all = T)

summary(meta)

meta <- meta %>% 
  fill(size, share.turnover, Pre_FFAlpha, nasdaq.dummy, sue, .direction = 'downup')

save(meta, file = './data/meta_event.RData')

meta <- meta %>% drop_na()
