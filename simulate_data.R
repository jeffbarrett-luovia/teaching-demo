#change to something else to save test data
out_file <- "training_data.csv"

n <- 10000
n_cases <- round(n * 0.3)

# --- Covariates ---
age <- round(rnorm(n, mean = 55, sd = 10))
age <- pmax(30, pmin(age, 85))

male <- rbinom(n, 1, 0.5)

total_chol <- round(rnorm(n, mean = 5.5, sd = 1.1), 1)  # mmol/L
total_chol <- pmax(2.5, total_chol)

hdl <- round(rnorm(n, mean = ifelse(male == 1, 1.2, 1.5), sd = 0.35), 2)  # mmol/L
hdl <- pmax(0.5, hdl)

smoking <- rbinom(n, 1, 0.25)

sbp <- round(rnorm(n, mean = 130, sd = 20))  # mmHg
sbp <- pmax(90, sbp)

# --- Log-odds ratios (approximate Framingham / literature values) ---
beta_age     <-  0.05   # per year
beta_male    <-  0.5
beta_tc      <-  0.3    # per mmol/L
beta_hdl     <- -0.6    # per mmol/L (protective)
beta_smoking <-  0.7
beta_sbp     <-  0.015  # per mmHg

lp_raw <- beta_age * age + beta_male * male + beta_tc * total_chol +
          beta_hdl * hdl + beta_smoking * smoking + beta_sbp * sbp

# Calibrate intercept so expected prevalence = 30%
f <- function(b0) mean(plogis(b0 + lp_raw)) - n_cases / n
intercept <- uniroot(f, c(-20, 20))$root

prob <- plogis(intercept + lp_raw)

# Sample exactly 300 cases, weighted by model probability
heart_attack <- rep(0L, n)
heart_attack[sample(seq_len(n), n_cases, prob = prob)] <- 1L

# --- Null predictors (beta = 0, independent of outcome) ---
height_cm      <- round(rnorm(n, mean = ifelse(male == 1, 176, 163),
                                  sd  = ifelse(male == 1, 7, 6)), 1)
activity_min   <- round(pmax(0, rnorm(n, mean = 30, sd = 15)))
creatinine_umol <- round(rnorm(n, mean = ifelse(male == 1, 85, 70),
                                   sd  = ifelse(male == 1, 15, 12)))

# --- Assemble and write ---
dat <- data.frame(
  heart_attack    = heart_attack,
  age             = age,
  male            = male,
  total_chol_mmol = total_chol,
  hdl_mmol        = hdl,
  smoking         = smoking,
  sbp             = sbp,
  height_cm       = height_cm,
  activity_min    = activity_min,
  creatinine_umol = creatinine_umol
)

write.csv(dat, out_file, row.names = FALSE)

cat("N =", nrow(dat), " Events =", sum(dat$heart_attack), "\n")
cat("Intercept =", round(intercept, 3), "\n\n")
cat("True log-ORs:\n")
cat("  age          ", beta_age, "\n")
cat("  male         ", beta_male, "\n")
cat("  total_chol   ", beta_tc, "\n")
cat("  hdl          ", beta_hdl, "\n")
cat("  smoking      ", beta_smoking, "\n")
cat("  sbp          ", beta_sbp, "\n")
cat("  height        0\n")
cat("  activity_min  0\n")
cat("  creatinine    0\n")
