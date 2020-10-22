library(tidyverse)
library(lubridate)
library(Hmisc)

df <- read.csv("ffq_mac.csv")
df <- rename(df, id = "idalt")
colnames(df)

### set "999" == NA
ninetona <- function(x) {
  ifelse(x == 9999, NA, x)
}

df[-(1:2)] <- lapply(df[-(1:2)], ninetona)

summary(df$kcal)
table(df$kcal)
hist(df$kcal)

### plot distribution of kcal by sex
kcals_plot <- ggplot(data = df, aes(x=kcal, color = sexo)) +
  geom_histogram(fill = "white", alpha=0.5, position="identity") +
  facet_grid(. ~ sexo) +
  xlab("kcal") +
  theme(text=element_text(size=16, family="Arial")) +
  theme_bw() #+
ggsave(plot = kcals_plot, file = "kcals.png", width = 5, height = 3, dpi = 600)

kcals_log_plot <- ggplot(data = df, aes(x=log(kcal), color = sexo)) +
  geom_histogram(fill = "white", alpha=0.5, position="identity") +
  facet_grid(. ~ sexo) +
  xlab("kcal") +
  theme(text=element_text(size=16, family="Arial")) +
  theme_bw() #+
ggsave(plot = kcals_log_plot, file = "kcals_log.png", width = 5, height = 3, dpi = 600)

table(df$sexo)
df$sex <- ifelse(df$sexo == "Masculino", 0, 
                           ifelse(df$sexo == "Feminino", 1, 
                                  df$sexo))
table(df$peso)
table(df$altura)
table(df$imc)

### add date of birth
idata <- read.csv(file="growth_clean.csv", header = T)
dob <- idata %>% select("dob", "id")
df <- merge(dob, df, by="id")

## calculate age
date.vars <- names(df %>% select("dob", "date1"))
df[date.vars]<- lapply(df[date.vars], as.Date)

age <- function(dob, age.day = today(), units = "years", floor = F) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

df$ageiny <- round(age(df$dob, df$date1),digits=1)
table(df$ageiny)

#### calculate energy requirements 
df$er <- (7.377 - 0.073 * df$ageiny + 0.0806 * df$peso + 0.0135 * df$altura - 1.363 * df$sex) * 239.006
table(df$er)
summary(df$er)

### calculate ei/er ratio
df$ei_er <- df$kcal/df$er
df$z_ei_er <- scale(df$ei_er)

## filter those with z-scores > 2
df_filt2SD <- subset(df, df$z_ei_er < 2)

### regression between ei and er
eier_lm <- lm(df$kcal ~ df$er)
summary(eier_lm)
cor.test(df$kcal, df$er, method = "spearman")

## subset to only annual consumption
dd_year <- (df %>% select("id", "sex", "kcal", "imc", contains("_yr")))

#### SAVE CSV ####
write.csv(dd_year, file="dd_year.csv", row.names = F, na = "")
