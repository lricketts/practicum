# This script imports Opportunity Insights Economic Tracker data and conducts
# a data analysis of the effect of stay at home lockdowns on economic activity 
# within select counties in the St. Louis metro area.

stem.dir <- "C:/Users/Lowell/Google Drive/Documents/Coursework/Graduate/Practicum"
in.dir <- paste0(stem.dir, "/data")
out.dir <- paste0(stem.dir, "/output")

library(plyr)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)
library(lfe)
library(jtools)
library(ggstance)
library(broom.mixed)



themeMod <- theme_bw() + 
        theme(axis.line = element_line(color = "black"),
              panel.grid = element_blank(),
              panel.background = element_blank(),
              strip.text = element_text(size = 10, face = "bold"),
              axis.title = element_text(size = 10),
              title = element_text(size = 12, face = "bold"),
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              legend.position = "bottom",
              plot.caption = element_text(hjust = -.05),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))

footnote <- "Source: Opportunity Insights Economic Tracker (https://tracktherecovery.org/)."

# Written by Lowell Ricketts

# Date Created: 1/30/2021
# Last Modified: 4/25/2021

#### Import Data ####

## Affinity Solutions (Consumer Spending)
aff.usa <- read.csv(paste0(in.dir, "/Affinity - National - Daily.csv"))
aff.city <- read.csv(paste0(in.dir, "/Affinity - City - Daily.csv"))
aff.state <- read.csv(paste0(in.dir, "/Affinity - State - Daily.csv"))
# NOTE: Only spend_all is available at the county level
aff.county <- read.csv(paste0(in.dir, "/Affinity - County - Daily.csv"),
                       stringsAsFactors = F)

## Womply (Small Business Revenue)
#womp.rev.usa <- read.csv(paste0(in.dir, "/Womply Revenue - National - Daily.csv"))
#womp.rev.city <- read.csv(paste0(in.dir, "/Womply Revenue - City - Daily.csv"))
#womp.rev.state <- read.csv(paste0(in.dir, "/Womply Revenue - State - Daily.csv"))
#womp.rev.county <- read.csv(paste0(in.dir, "/Womply Revenue - County - Daily.csv"))

## Womply (Small Business Merchants)
#womp.merch.usa <- read.csv(paste0(in.dir, "/Womply Merchants - National - Daily.csv"))
#womp.merch.city <- read.csv(paste0(in.dir, "/Womply Merchants - City - Daily.csv"))
#womp.merch.state <- read.csv(paste0(in.dir, "/Womply Merchants - State - Daily.csv"))
#womp.merch.county <- read.csv(paste0(in.dir, "/Womply Merchants - County - Daily.csv"))

## Womply (Small Business Revenues & Merchants)
womp.usa <- read.csv(paste0(in.dir, "/Womply - National - Daily.csv"))
womp.city <- read.csv(paste0(in.dir, "/Womply - City - Daily.csv"))
womp.state <- read.csv(paste0(in.dir, "/Womply - State - Daily.csv"))
womp.county <- read.csv(paste0(in.dir, "/Womply - County - Daily.csv"),
                        stringsAsFactors = F)

## Paychex, Intuit, Earnin and Kronos (Combined Employment)
comb.emp.usa <- read.csv(paste0(in.dir, "/Employment Combined - National - Daily.csv"))
comb.emp.city <- read.csv(paste0(in.dir, "/Employment Combined - City - Daily.csv"))
comb.emp.state <- read.csv(paste0(in.dir, "/Employment Combined - State - Daily.csv"))
# NOTE: Coverage is very sparse within STL metro area, even for larger counties
comb.emp.county <- read.csv(paste0(in.dir, "/Employment Combined - County - Daily.csv"),
                            stringsAsFactors = F)

## Department of Labor; State Agencies (Unemployment insurance claims)
labor.ui.usa <- read.csv(paste0(in.dir, "/UI Claims - National - Weekly.csv"))
labor.ui.city <- read.csv(paste0(in.dir, "/UI Claims - City - Weekly.csv"))
labor.ui.state <- read.csv(paste0(in.dir, "/UI Claims - State - Weekly.csv"))
labor.ui.county <- read.csv(paste0(in.dir, "/UI Claims - County - Weekly.csv"),
                            stringsAsFactors = F)

## Burning Glass (Job Postings; NOT AVAILABLE AT COUNTY LEVEL)
burn.usa <- read.csv(paste0(in.dir, "/Burning Glass - National - Weekly.csv"))
burn.city <- read.csv(paste0(in.dir, "/Burning Glass - City - Weekly.csv"))
burn.state <- read.csv(paste0(in.dir, "/Burning Glass - State - Weekly.csv"))

## New York Times (COVID case and deaths)
covid.usa <- read.csv(paste0(in.dir, "/COVID - National - Daily.csv"))
covid.city <- read.csv(paste0(in.dir, "/COVID - City - Daily.csv"))
covid.state <- read.csv(paste0(in.dir, "/COVID - State - Daily.csv"))
covid.county <- read.csv(paste0(in.dir, "/COVID - County - Daily.csv"))

## Google (Mobility)
google.usa <- read.csv(paste0(in.dir, "/Google Mobility - National - Daily.csv"))
google.city <- read.csv(paste0(in.dir, "/Google Mobility - City - Daily.csv"))
google.state <- read.csv(paste0(in.dir, "/Google Mobility - State - Daily.csv"))
google.county <- read.csv(paste0(in.dir, "/Google Mobility - County - Daily.csv"))



## Policy Milestones

policy <- read.csv(paste0(in.dir, "/Policy Milestones - State.csv"))

# Geographic IDs 
fips.county <- read.csv(paste0(in.dir, "/GeoIDs - County.csv"))
fips.state <- read.csv(paste0(in.dir, "/GeoIDs - State.csv"))
# NOTE: the cities file offers an unexpected list like Wichita but not St. Louis; 
# Honolulu but not Seattle)
fips.city <- read.csv(paste0(in.dir, "/GeoIDs - City.csv"))


#### Explore St. Louis Metro Consumer Spending ####

# NOTE: Opportunity Insights finds that the shock to consumer spending is the
# initial shock that propogates through the economy.

aff.county <- inner_join(aff.county, fips.county, by = c("countyfips"))

# NOTE: No Affinity observations for Crawford, MO; Warren, MO; Bond, IL; Calhoun, IL
# These are relatively small counties so coverage is good for the mahority of the 
# population.

aff.stl.counties <- aff.county %>% 
        filter((stateabbrev == "IL" & countyname %in% 
                        c("Bond", "Calhoun", "Clinton", "Jersey", 
                          "Macoupin", "Madison", "Monroe", "St Clair")) |
                       (stateabbrev == "MO" & countyname %in% 
                                c("Crawford", "Franklin", "Jefferson", "Lincoln", 
                                  "St Charles", "St Louis", "Warren", "St Louis City"))
) %>% arrange(countyfips, year, month, day) %>%
        mutate(date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d"),
               geoname = paste0(countyname, ", ", stateabbrev),
               spend_all = ifelse(spend_all == ".", NA, spend_all),
               spend_all = as.numeric(spend_all))

png(paste0(out.dir, "/Consumer Spending, STL MSA Counties.png"), 
    width = 1200, height = 700)

g1 <- ggplot(aff.stl.counties) +
        #geom_line(aes(y = spend_all, x = date, color = as.factor(geoname)), size = 1.25) +
        geom_line(aes(y = spend_all, x = date), size = 1) +
        facet_wrap(~as.factor(geoname)) +
        themeMod + labs(caption = footnote) + labs(x = "Date", y = "Spending Relative to Jan. 4-31, 2020 (SA, 7-day MA)") + 
        ggtitle("Credit/Debit Card Spending, St. Louis MSA Counties") +
        geom_hline(yintercept = 0) #+ coord_cartesian(ylim = c(0, 30)) +

g1

dev.off(); g1




#### Explore St. Louis Metro Small Business Outcomes ####

# NOTE: 12 of 16 STL metro counties
womp.stl.counties <- womp.county %>% 
        inner_join(fips.county, by = c("countyfips")) %>%
        filter((stateabbrev == "IL" & countyname %in% 
                        c("Bond", "Calhoun", "Clinton", "Jersey", 
                          "Macoupin", "Madison", "Monroe", "St Clair")) |
                       (stateabbrev == "MO" & countyname %in% 
                                c("Crawford", "Franklin", "Jefferson", "Lincoln", 
                                  "St Charles", "St Louis", "Warren", "St Louis City"))
        ) %>% arrange(countyfips, year, month, day) %>%
        mutate(date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d"),
               geoname = paste0(countyname, ", ", stateabbrev),
               revenue_all = as.numeric(revenue_all),
               merchants_all = as.numeric(merchants_all))

png(paste0(out.dir, "/Small Business Revenue, STL MSA Counties.png"), 
    width = 1200, height = 700)

g2 <- ggplot(womp.stl.counties) +
        geom_line(aes(y = revenue_all, x = date), size = 1) +
        facet_wrap(~as.factor(geoname)) +
        themeMod + labs(caption = footnote) + labs(x = "Date", y = "Net Small Bus. Revenue Relative to Jan. 4-31, 2020 (SA, 7-day MA)") + 
        ggtitle("Small Business Revenue, St. Louis MSA Counties") +
        geom_hline(yintercept = 0) #+ coord_cartesian(ylim = c(0, 30)) +

g2

dev.off(); g2


png(paste0(out.dir, "/Small Business Openings, STL MSA Counties.png"), 
    width = 1200, height = 700)

g3 <- ggplot(womp.stl.counties) +
        geom_line(aes(y = merchants_all, x = date), size = 1) +
        facet_wrap(~as.factor(geoname)) +
        themeMod + labs(caption = footnote) + 
        labs(x = "Date", y = "% Change in Small Bus. Open Relative to Jan. 4-31, 2020 (SA, 7-day MA)") + 
        ggtitle("Small Business Openings, St. Louis MSA Counties") +
        geom_hline(yintercept = 0)

g3

dev.off(); g3

#### Explore St. Louis Metro Labor Market Data ####

# WARNING: Only 3 of 16 counties are available.
# UPDATE: We now have 6 counties.
comb.emp.stl.counties <- comb.emp.county %>% 
        inner_join(fips.county, by = c("countyfips")) %>%
        filter((stateabbrev == "IL" & countyname %in% 
                        c("Bond", "Calhoun", "Clinton", "Jersey", 
                          "Macoupin", "Madison", "Monroe", "St Clair")) |
                       (stateabbrev == "MO" & countyname %in% 
                                c("Crawford", "Franklin", "Jefferson", "Lincoln", 
                                  "St Charles", "St Louis", "Warren", "St Louis City"))
        ) %>% arrange(countyfips, year, month, day) %>%
        mutate(date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d"),
               geoname = paste0(countyname, ", ", stateabbrev),
               emp_combined = ifelse(emp_combined == ".", NA, emp_combined),
               emp_combined = as.numeric(emp_combined),
               emp_combined_inclow = ifelse(emp_combined_inclow == ".", NA, emp_combined_inclow),
               emp_combined_inclow = as.numeric(emp_combined_inclow),
               emp_combined_incmiddle = ifelse(emp_combined_incmiddle == ".", NA, emp_combined_incmiddle),
               emp_combined_incmiddle = as.numeric(emp_combined_incmiddle),
               emp_combined_inchigh = ifelse(emp_combined_inchigh == ".", NA, emp_combined_inchigh),
               emp_combined_inchigh = as.numeric(emp_combined_inchigh)
               ) %>%
    filter(!is.na(emp_combined))



png(paste0(out.dir, "/Combined Employment, STL MSA Counties.png"), 
    width = 1200, height = 700)

g4 <- ggplot(comb.emp.stl.counties) +
        geom_line(aes(y = emp_combined, x = date), size = 1) +
        facet_wrap(~as.factor(geoname)) +
        themeMod + labs(caption = footnote) + 
        labs(x = "Date", y = "Employment Level; Relative to Jan. 4-31, 2020") + 
        ggtitle("Employment Level for All Workers, St. Louis MSA Counties") +
        geom_hline(yintercept = 0)

g4

dev.off(); g4


labor.ui.stl.counties <- labor.ui.county %>% 
        inner_join(fips.county, by = c("countyfips")) %>%
        filter((stateabbrev == "IL" & countyname %in% 
                        c("Bond", "Calhoun", "Clinton", "Jersey", 
                          "Macoupin", "Madison", "Monroe", "St Clair")) |
                       (stateabbrev == "MO" & countyname %in% 
                                c("Crawford", "Franklin", "Jefferson", "Lincoln", 
                                  "St Charles", "St Louis", "Warren", "St Louis City"))
        ) %>% arrange(countyfips, year, month, day_endofweek) %>%
        mutate(date = as.Date(paste0(year, "-", month, "-", day_endofweek), format = "%Y-%m-%d"),
               geoname = paste0(countyname, ", ", stateabbrev),
               initclaims_rate_regular = as.numeric(initclaims_rate_regular))



png(paste0(out.dir, "/Unemployment Insurance Initial Claims, STL MSA Counties.png"), 
    width = 1200, height = 700)

g5 <- ggplot(labor.ui.stl.counties) +
        geom_line(aes(y = initclaims_rate_regular, x = date), size = 1) +
        facet_wrap(~as.factor(geoname)) +
        themeMod + labs(caption = footnote) + 
        labs(x = "Date", y = "UI Claims per 100 people in 2019 Labor Force") + 
        ggtitle("Unemployment Insurance Initial Claims (Regular UI Only), St. Louis MSA Counties") +
        geom_hline(yintercept = 0)

g5

dev.off(); g5
        

#### Explore St. Louis Metro Mobility Data ####

# NOTE: 15 of 16 metro counties, missing Calhoun, IL
google.stl.counties <- google.county %>% 
        inner_join(fips.county, by = c("countyfips")) %>%
        filter((stateabbrev == "IL" & countyname %in% 
                        c("Bond", "Calhoun", "Clinton", "Jersey", 
                          "Macoupin", "Madison", "Monroe", "St Clair")) |
                       (stateabbrev == "MO" & countyname %in% 
                                c("Crawford", "Franklin", "Jefferson", "Lincoln", 
                                  "St Charles", "St Louis", "Warren", "St Louis City"))
        ) %>% arrange(countyfips, year, month, day)

#### Exploratory Analysis of Treatment and Controls ####

stl.policy <- read.csv(paste0(in.dir, "/stl policy dates.csv"))

# Policy dates gleaned from executive orders, health orders, and media reporting
stl.policy <- stl.policy %>% mutate(
    stayathome_start = as.Date(stayathome_start, format = "%m/%d/%Y"),
    stayathome_end = as.Date(stayathome_end, format = "%m/%d/%Y"),
    saferathome_start = as.Date(saferathome_start, format = "%m/%d/%Y"),
    saferathome_end = as.Date(saferathome_end, format = "%m/%d/%Y")
    )

aff.sub <- aff.stl.counties %>% filter(geoname %in% 
                                           c("St Louis City, MO", "St Louis, MO", 
                                             "St Charles, MO", "Jefferson, MO")) %>%
    inner_join(stl.policy, by = c("countyfips"))        


png(paste0(out.dir, "/Consumer Spending Event Study.png"), 
    width = 1200, height = 700)

g6a <- ggplot(subset(aff.sub, date <= "2020-09-01"), aes(y = spend_all, x = date)) +
    geom_line(size = 1) +
    geom_vline(aes(xintercept = stayathome_start), color = "red", size = 1) +
    geom_vline(aes(xintercept = stayathome_end), color = "green", size = 1) +
    facet_wrap(~as.factor(geoname)) +
    themeMod + labs(caption = footnote) + labs(x = "Date", y = "Spending Relative to Jan. 4-31, 2020 (SA, 7-day MA)") + 
    ggtitle("Credit/Debit Card Spending, Select St. Louis MSA Counties") +
    geom_hline(yintercept = 0)

#g6a
#dev.off(); g6a


g6b <- ggplot(subset(aff.sub, date >= "2020-03-01" & date <= "2020-07-01"), aes(y = spend_all, x = date, color = as.factor(geoname))) +
    geom_line(size = 1) +
    geom_vline(aes(xintercept = stayathome_start), color = "red", size = 1) +
    geom_vline(aes(xintercept = stayathome_end), color = "green", size = 1) +
    themeMod + labs(caption = footnote) + labs(x = "Date", y = "Spending Relative to Jan. 4-31, 2020 (SA, 7-day MA)") + 
    ggtitle("Credit/Debit Card Spending, Select St. Louis MSA Counties") +
    geom_hline(yintercept = 0) + scale_color_viridis(discrete = T) +
    scale_x_date(labels = date_format("%Y-%m"), breaks = "1 month")

g6b
dev.off(); g6b

womp.sub <- womp.stl.counties %>% filter(geoname %in% 
                                           c("St Louis City, MO", "St Louis, MO", 
                                             "St Charles, MO", "Jefferson, MO")) %>%
    inner_join(stl.policy, by = c("countyfips"))        


png(paste0(out.dir, "/Small Business Openings Event Study.png"), 
    width = 1200, height = 700)

g7a <- ggplot(subset(womp.sub, date <= "2020-09-01"), aes(y = merchants_all, x = date)) +
    geom_line(size = 1) +
    geom_vline(aes(xintercept = stayathome_start), color = "red", size = 1) +
    geom_vline(aes(xintercept = stayathome_end), color = "green", size = 1) +
    facet_wrap(~as.factor(geoname)) +
    themeMod + labs(caption = footnote) + labs(x = "Date", y = "% Change in Small Bus. Open Relative to Jan 4-31, 2020 (SA, 7-day MA)") + 
    ggtitle("Small Business Openings, Select St. Louis MSA Counties") +
    geom_hline(yintercept = 0)

#g7a
#dev.off(); g7a


g7b <- ggplot(subset(womp.sub, date >= "2020-03-01" & date <= "2020-07-01"), aes(y = merchants_all, x = date, color = as.factor(geoname))) +
    geom_line(size = 1) +
    geom_vline(aes(xintercept = stayathome_start), color = "red", size = 1) +
    geom_vline(aes(xintercept = stayathome_end), color = "green", size = 1) +
    themeMod + labs(caption = footnote) + labs(x = "Date", y = "% Change in Small Bus. Open Relative to Jan 4-31, 2020 (SA, 7-day MA)") + 
    ggtitle("Small Business Openings, Select St. Louis MSA Counties") +
    geom_hline(yintercept = 0) + scale_color_viridis(discrete = T) +
    scale_x_date(labels = date_format("%Y-%m"), breaks = "1 month")
    

g7b
dev.off(); g7b

png(paste0(out.dir, "/Small Business Revenue Event Study.png"), 
    width = 1200, height = 700)

g8a <- ggplot(subset(womp.sub, date <= "2020-09-01"), aes(y = revenue_all, x = date)) +
    geom_line(size = 1) +
    geom_vline(aes(xintercept = stayathome_start), color = "red", size = 1) +
    geom_vline(aes(xintercept = stayathome_end), color = "green", size = 1) +
    facet_wrap(~as.factor(geoname)) +
    themeMod + labs(caption = footnote) + labs(x = "Date", y = "Net Small Bus. Revenue Relative to Jan. 4-31, 2020 (SA, 7-day MA)") + 
    ggtitle("Small Business Revenue, Select St. Louis MSA Counties") +
    geom_hline(yintercept = 0) +
    scale_x_date(labels = date_format("%Y-%m"), breaks = "1 month")


#g8a
#dev.off(); g8a

g8b <- ggplot(subset(womp.sub, date >= "2020-03-01" & date <= "2020-07-01"), aes(y = revenue_all, x = date, color = as.factor(geoname))) +
    geom_line(size = 1) +
    geom_vline(aes(xintercept = stayathome_start), color = "red", size = 1) +
    geom_vline(aes(xintercept = stayathome_end), color = "green", size = 1) +
    themeMod + labs(caption = footnote) + labs(x = "Date", y = "Net Small Bus. Revenue Relative to Jan. 4-31, 2020 (SA, 7-day MA)") + 
    ggtitle("Small Business Revenue, Select St. Louis MSA Counties") +
    scale_color_viridis(discrete = T) + geom_hline(yintercept = 0) +
    scale_x_date(labels = date_format("%Y-%m"), breaks = "1 month")


g8b
dev.off(); g8b


labor.ui.sub <- labor.ui.stl.counties %>% filter(geoname %in% 
                                             c("St Louis City, MO", "St Louis, MO", 
                                               "St Charles, MO", "Jefferson, MO")) %>%
    inner_join(stl.policy, by = c("countyfips"))        



png(paste0(out.dir, "/UI Initial Claims Event Study.png"), 
    width = 1200, height = 700)

g9a <- ggplot(subset(labor.ui.sub, date <= "2020-09-01"), aes(y = initclaims_rate_regular, x = date)) +
    geom_line(size = 1) +
    geom_vline(aes(xintercept = stayathome_start), color = "red", size = 1) +
    geom_vline(aes(xintercept = stayathome_end), color = "green", size = 1) +
    facet_wrap(~as.factor(geoname)) +
    themeMod + labs(caption = footnote) + labs(x = "Date", y = "UI Claims per 100 people in 2019 Labor Force") + 
    ggtitle("Unemployment Insurance Initial Claims (Regular UI Only), Select St. Louis MSA Counties") +
    geom_hline(yintercept = 0)

#g9a
#dev.off(); g9a


g9b <- ggplot(subset(labor.ui.sub, date >= "2020-03-01" & date <= "2020-07-01"), aes(y = initclaims_rate_regular, x = date, color = as.factor(geoname))) +
    geom_line(size = 1) +
    geom_vline(aes(xintercept = stayathome_start), color = "red", size = 1) +
    geom_vline(aes(xintercept = stayathome_end), color = "green", size = 1) +
    themeMod + labs(caption = footnote) + labs(x = "Date", y = "UI Claims per 100 people in 2019 Labor Force") + 
    ggtitle("Unemployment Insurance Initial Claims (Regular UI Only), Select St. Louis MSA Counties") +
    geom_hline(yintercept = 0) + scale_color_viridis(discrete = T) +
    scale_x_date(labels = date_format("%Y-%m"), breaks = "1 month")



g9b
dev.off(); g9b



comb.emp.sub <- comb.emp.stl.counties %>% filter(geoname %in% 
                                                     c("St Louis City, MO", "St Louis, MO", 
                                                       "St Charles, MO", "Jefferson, MO")) %>%
    inner_join(stl.policy, by = c("countyfips"))    


png(paste0(out.dir, "/Combined Employment Event Study.png"), 
    width = 1200, height = 700)

g10a <- ggplot(subset(comb.emp.sub, date <= "2020-09-01"), aes(y = emp_combined, x = date)) +
    geom_line(size = 1) +
    geom_vline(aes(xintercept = stayathome_start), color = "red", size = 1) +
    geom_vline(aes(xintercept = stayathome_end), color = "green", size = 1) +
    facet_wrap(~as.factor(geoname)) +
    themeMod + labs(caption = footnote) + labs(x = "Date", y = "Employment Level; Relative to Jan. 4-31, 2020") +
    ggtitle("Employment Level for All Workers, Select St. Louis MSA Counties") +
    geom_hline(yintercept = 0)

#g10a
#dev.off(); g10a


g10b <- ggplot(subset(comb.emp.sub, date >= "2020-03-01" & date <= "2020-07-01"), aes(y = emp_combined, x = date, color = as.factor(geoname))) +
    geom_line(size = 1) +
    geom_vline(aes(xintercept = stayathome_start), color = "red", size = 1) +
    geom_vline(aes(xintercept = stayathome_end), color = "green", size = 1) +
    themeMod + labs(caption = footnote) + labs(x = "Date", y = "Employment Level; Relative to Jan. 4-31, 2020") +
    ggtitle("Employment Level for All Workers, Select St. Louis MSA Counties") +
    geom_hline(yintercept = 0) + scale_color_viridis(discrete = T) +
    scale_x_date(labels = date_format("%Y-%m"), breaks = "1 month")


g10b
dev.off(); g10b


#### Difference-in-Differences Analysis of Spring 2020 STL-Area Lockdowns ####

# We need to isolate the period where the state stay at home order had expired
# in St. Charles County and Jefferson County but a stay at home order remained
# in effect for St. Louis City and St. Louis County.

treat.begin <- stl.policy$stayathome_start[stl.policy$countyname == "St. Louis"]
treat.diverge <- stl.policy$stayathome_end[stl.policy$countyname == "Jefferson"]
treat.end.stlcity <- stl.policy$stayathome_end[stl.policy$countyname == "St. Louis City"]
treat.end.stlcounty <- stl.policy$stayathome_end[stl.policy$countyname == "St. Louis"]

# NOTE: It seems to make the most sense to use the time period where
# there is no stay at home order and business closures in effect within
# St. Louis City and St. Louis County versus St. Charles and Jefferson
# counties (and the broader state).

aff.did <- aff.sub %>% mutate(
    # Define time, group, and interaction for DID binaries
    time = ifelse(date >= treat.diverge & date <= treat.end.stlcounty, 1, 0),
    #time = ifelse(date >= treat.begin & date <= treat.end.stlcounty, 1, 0),
    #treated = case_when(
    #    geoname == "St Louis City, MO" & (date >= treat.diverge & date <= treat.end.stlcity) ~ 1,
    #    geoname == "St Louis, MO" & (date >= treat.diverge & date <= treat.end.stlcounty) ~ 1,
    #    T ~ 0
    #    ),
    treated = ifelse(geoname %in% c("St Louis City, MO", "St Louis, MO"), 1, 0),
    did = time * treated
) %>%
    # Limit sample to pre-September 2020 as the fall surge began to take hold after,
    # according to the NYT case counts for Missouri
    filter(!is.na(spend_all), date <= "2020-09-01")

aff.didreg = lm(spend_all ~ treated + time + did, data = aff.did)

summary(aff.didreg)

# DiD regressions with Womply data
womp.did <- womp.sub %>% mutate(
    time = ifelse(date >= treat.diverge & date <= treat.end.stlcounty, 1, 0),
    treated = ifelse(geoname %in% c("St Louis City, MO", "St Louis, MO"), 1, 0),
    did = time * treated
) %>%
    #filter(!is.na(merchants_all), date <= "2020-09-01")
    # Try a narrower time range for the sample which avoids a potential summer surge
    filter(!is.na(merchants_all), date >= "2020-03-01" & date <= "2020-07-01")

# Small business revenue
womp.rev.didreg = lm(revenue_all ~ treated + time + did, data = womp.did)

summary(womp.rev.didreg)

felm.womp.rev <- felm(revenue_all ~  did | treated + time | 0 | treated, data = womp.did)

summary(felm.womp.rev)



# Small business openings
womp.merch.didreg = lm(merchants_all ~ treated + time + did, data = womp.did)

summary(womp.merch.didreg)

felm.womp.merch <- felm(merchants_all ~  did | treated + time | 0 | treated, data = womp.did)

summary(felm.womp.merch)


comb.emp.did <- comb.emp.sub %>% mutate(
    time = ifelse(date >= treat.diverge & date <= treat.end.stlcounty, 1, 0),
    treated = ifelse(geoname %in% c("St Louis City, MO", "St Louis, MO"), 1, 0),
    did = time * treated
) %>%
    filter(!is.na(emp_combined), date <= "2020-09-01")

comb.emp.didreg = lm(emp_combined ~ treated + time + did, data = comb.emp.did)

summary(comb.emp.didreg)

# No significant effects found for treated, time, or DiD
labor.ui.did <- labor.ui.sub %>% mutate(
    time = ifelse(date >= treat.diverge & date <= treat.end.stlcounty, 1, 0),
    treated = ifelse(geoname %in% c("St Louis City, MO", "St Louis, MO"), 1, 0),
    did = time * treated
) %>%
    filter(!is.na(initclaims_rate_regular), date <= "2020-09-01")

labor.ui.didreg = lm(initclaims_rate_regular ~ treated + time + did, data = labor.ui.did)

summary(labor.ui.didreg)


# Compare DiD regressions across response variables
summary(aff.didreg)
summary(womp.rev.didreg)
summary(womp.merch.didreg)
summary(comb.emp.didreg) # Recall that St. Louis County is the only treated in this sample
summary(labor.ui.didreg)

### Export coefficient visualizations

# Womply Small Business
plot_summs(womp.merch.didreg, womp.rev.didreg, 
           model.names = c("Businesses Open", "Business Revenue"))


plot_summs(aff.didreg, comb.emp.didreg,
           model.names = c("Card Spending", "Combined Employment"))

plot_summs(labor.ui.didreg)
