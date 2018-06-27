library(ggplot2)
library(plyr)
library(readr)
library(dplyr)
library(reshape2)
library(gridExtra)
library(jsonlite)
library(digest)
library(tidyverse)
library(forcats)

sme_dad <- function()
{
    WORKDIRECTORY= "/Users/junhe/BoxSync/workdir/dad-weight/"
    THISFILE     = 'plot.r'
    setwd(WORKDIRECTORY)
    source(THISFILE)
}


read_data <- function() {
    d = read_csv("./weight-sheet-export.csv")

    d = d %>% filter(!(is.na(weightKG.Dad) & is.na(weightLB.Jun))) %>%
        mutate(weightJIN.Dad = weightKG.Dad * 2) %>%
        mutate(weightJIN.Jun = (weightLB.Jun / 2.20462) * 2) %>%
        mutate(weekday.date = interaction(weekday, date, sep = "\n"))
}

plot <- function(d) {
    d = d %>% gather(weightJIN.Jun, weightJIN.Dad, key = "who", value = "weightJIN")
    d = d %>% mutate(who = factor(who)) %>%
        mutate(who = fct_recode(who, "何军" = "weightJIN.Jun")) %>%
        mutate(who = fct_recode(who, "老爸" = "weightJIN.Dad"))

    font = "Yuanti SC"
    p = ggplot(d, aes(x = weekday.date, y = weightJIN, color = who)) +
        geom_point(size=4) +
        geom_line(aes(group = who), size=3) +
        expand_limits(y = c(150, 170)) +
        ylab("体重（斤）") + 
        xlab("") + 
        theme(plot.title=element_text(size=20, family=font), 
              plot.subtitle=element_text(size=12, family=font), 
              axis.text.y=element_text(size=28),
              text=element_text(size=18, family=font)
              ) +
        theme(legend.title=element_blank())
    print(p)
}

plot_jun_only <- function(d) {
    print(d)
    font = "Yuanti SC"
    p = ggplot(d, aes(x = weekday.date, y = weightLB.Jun)) +
        geom_point(size=4) +
        geom_line(aes(group = 1), size=3) +
        expand_limits(y = c(150, 180)) +
        theme(plot.title=element_text(size=20, family=font), 
              plot.subtitle=element_text(size=12, family=font), 
              axis.text.y=element_text(size=28),
              text=element_text(size=18, family=font)
              ) +
        theme(legend.title=element_blank())
    print(p)
}



main <- function()
{
    d = read_data()
    plot_jun_only(d)
    plot(d)
}

main()

