#########################################################################
#########################################################################
#########################################################################
# 
# Code for sourcing data from various databases
# 
# The first batch of code loads a number of packages.
# If you have the latest version of R, it should offer to install
# packages that are missing, alternatively you can just install them
# yourself. 
# 
#########################################################################
#########################################################################

#########################################################################
# Package installations
#########################################################################
install.packages("tidyverse")
install.packages("RJSONIO")
install.packages("reshape")
install.packages("rowr")
install.packages("date")

install.packages("Matrix")
install.packages("expm")
install.packages("matlib")

install.packages("plotrix")
install.packages("xtable")
install.packages("rayshader")

install.packages("pipeR")
install.packages("extrafont")
install.packages("foreign")
install.packages("haven")
install.packages("Formula")
install.packages("Hmisc")

install.packages("mFilter")
install.packages("networkD3")
install.packages("caTools")
install.packages("data.table")
install.packages("quantmod")

install.packages("kableExtra")
install.packages("cowplot")

install.packages("readxl")
install.packages("openxlsx")
install.packages("gdata")

install.packages("plm")

install.packages("bookdown")
install.packages("blogdown")
install.packages("checkpoint")
install.packages("devtools")

install.packages("codetools")
install.packages("tikzDevice")
install.packages("shiny")

# Data sourcing code

# Danmarks Statistik 
devtools::install_github("mikkelkrogsholm/statsDK")

# Packages for data sourcing
install.packages("pdfetch") # For Eurostat data - full datasets
install.packages("countrycode") # For getting country names from country 2 or 3 letter codes
install.packages("ecb") # For ECB data
install.packages("eurostat") # For Eurostat data - individual variables
install.packages("OECD") # For OECD data
# Dependencies
# ‘e1071’, ‘units’, ‘classInt’, ‘sf’, ‘RefManageR’
install.packages("Quandl") # Read CRAN for API details

# Packages for relational database editing
install.packages("RMySQL")

#########################################################################
#########################################################################
#########################################################################
# Load packages
#########################################################################
#########################################################################
#########################################################################


# You might need to install a few extra packages, in addition to 
# the ones listed above

sapply(c("reshape2", "quantmod", "Hmisc", "mFilter", "xtable", "plotrix", "networkD3",
         "Formula", "knitr", "reshape", "plm", "countrycode", "tidyr", "dplyr", "eurostat",
         "plyr", "foreign","ecb", "tidyverse", "stringr", "rstudioapi", "ggplot2", "corpcor",
         "perturb","data.table", "rowr", "statsDK", "date", "kableExtra",
         "readxl", "OECD", "viridis", "fpp", "rsdmx", "roll", "scales", "randomcoloR",
         "grid", "cowplot"), require, character.only = TRUE)

#########################################################################
#########################################################################
# Set active working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#########################################################################
#########################################################################
# Create some useful file paths
# 

data_path <- "/Users/robertayretonbaileysmith/OneDrive - Aalborg Universitet/AAU/07 Supervision Work/00 Materials/Rmarkdown projects/data"
image_path <- "/Users/robertayretonbaileysmith/OneDrive - Aalborg Universitet/AAU/07 Supervision Work/00 Materials/Rmarkdown projects/images"


#########################################################################
#########################################################################
# Set up some extra features for plots that will be used later

plot_line_width = 0.85

#########################################################################
# Set colour palettes
#########################################################################

blackpalette <- c("0, 0, 0",
                  "125, 125, 125",
                  "75, 75, 75",
                  "225, 30, 0")
bluepalette <- c("0, 50, 130",
                 "0, 170, 255",
                 "0, 200, 255",
                 "0, 55, 255")
redpalette <- c("255, 45, 0",
                "255, 200, 0",
                "255, 155, 0",
                "255, 100, 0")

blackpalette_five <- c("0, 0, 0",
                       "185, 190, 200",
                       "115, 115, 115",
                       "75, 75, 75",
                       "225, 30, 0")
bluepalette_five <- c("0, 50, 130",
                      "0, 150, 255",
                      "0, 175, 255",
                      "0, 200, 255",
                      "0, 55, 255")
redpalette_five <- c("255, 45, 0",
                     "255, 200, 0",
                     "255, 175, 0",
                     "255, 145, 0",
                     "255, 100, 0")

blackpalette_six <- c("0, 50, 130",
                      "0, 0, 0",
                      "185, 190, 200",
                      "115, 115, 115",
                      "75, 75, 75",
                      "225, 30, 0")
bluepalette_six <- c("0, 50, 130",
                     "0, 100, 255",
                     "0, 130, 255",
                     "0, 165, 255",
                     "0, 200, 255",
                     "0, 55, 255")
redpalette_six <- c("255, 45, 0",
                    "255, 240, 0",
                    "255, 210, 0",
                    "255, 180, 0",
                    "255, 155, 0",
                    "255, 100, 0")


bluepalette <- c("0, 50, 130",
                 "0, 170, 255",
                 "0, 200, 255",
                 "0, 55, 255")
redpalette <- c("255, 45, 0",
                "255, 200, 0",
                "255, 155, 0",
                "255, 100, 0")

randompalette <- c("91, 163, 111",
                   "84, 135, 158",
                   "76, 99, 143",
                   "204, 157, 2",
                   "156, 0, 0",
                   "110, 99, 194",
                   "11, 132, 176",
                   "237, 133, 28",
                   "23, 87, 11",
                   "49, 163, 79")

blackpalette <- sapply(strsplit(blackpalette, ", "), function(x)
    rgb(x[1], x[2], x[3], maxColorValue=255))
bluepalette <- sapply(strsplit(bluepalette, ", "), function(x)
    rgb(x[1], x[2], x[3], maxColorValue=255))
redpalette <- sapply(strsplit(redpalette, ", "), function(x)
    rgb(x[1], x[2], x[3], maxColorValue=255))

blackpalette_five <- sapply(strsplit(blackpalette_five, ", "), function(x)
    rgb(x[1], x[2], x[3], maxColorValue=255))
bluepalette_five <- sapply(strsplit(bluepalette_five, ", "), function(x)
    rgb(x[1], x[2], x[3], maxColorValue=255))
redpalette_five <- sapply(strsplit(redpalette_five, ", "), function(x)
    rgb(x[1], x[2], x[3], maxColorValue=255))

blackpalette_six <- sapply(strsplit(blackpalette_six, ", "), function(x)
    rgb(x[1], x[2], x[3], maxColorValue=255))
bluepalette_six <- sapply(strsplit(bluepalette_six, ", "), function(x)
    rgb(x[1], x[2], x[3], maxColorValue=255))
redpalette_six <- sapply(strsplit(redpalette_six, ", "), function(x)
    rgb(x[1], x[2], x[3], maxColorValue=255))

randompalette <- sapply(strsplit(randompalette, ", "), function(x)
    rgb(x[1], x[2], x[3], maxColorValue=255))


#########################################################################
# Define random colours for plots and theme settings
#########################################################################

random_srv_palette <- c("91, 163, 111",
                        "84, 135, 158",
                        "156, 0, 0",
                        "204, 157, 2",
                        "110, 99, 194",
                        "11, 132, 176",
                        "76, 99, 143",
                        "237, 133, 28",
                        "23, 87, 11",
                        "11, 132, 176",
                        "49, 163, 79")
random_srv_palette <- sapply(strsplit(random_srv_palette, ", "), function(x)
    rgb(x[1], x[2], x[3], maxColorValue = 255))

#########################################################################
# Set plot options
#########################################################################
# Create alternative legend placements inside the plots

legend_bottom_right_inside <- theme(legend.spacing = unit(0.02, "cm"),
                                    legend.background = element_rect(colour = "white", size = 0.1),
                                    legend.key.size = unit(0.5, 'lines'),
                                    legend.justification=c(1,0), 
                                    legend.position=c(1,0))

legend_top_right_inside <- theme(legend.spacing = unit(0.02, "cm"),
                                 legend.background = element_rect(colour = "white", size = 0.1),
                                 legend.key.size = unit(0.5, 'lines'),
                                 legend.justification=c(1,1), 
                                 legend.position=c(1,1))

legend_top_left_inside <- theme(legend.spacing = unit(0.02, "cm"),
                                legend.background = element_rect(colour = "white", size = 0.1),
                                legend.key.size = unit(0.5, 'lines'),
                                legend.justification=c(0,1), 
                                legend.position=c(0,1))

legend_bottom_left_inside <- theme(legend.spacing = unit(0.02, "cm"),
                                   legend.background = element_rect(colour = "white", size = 0.1),
                                   legend.key.size = unit(0.5, 'lines'),
                                   legend.justification=c(0,0), 
                                   legend.position=c(0,0))

#########################################################################
# Create percentage number format settings object for plots
#########################################################################
pct_scale_settings <- scales::percent_format(accuracy = NULL,
                                             scale = 100, 
                                             prefix = "", 
                                             suffix = "\\%",
                                             big.mark = " ", 
                                             decimal.mark = ".", 
                                             trim = TRUE)


#########################################################################
# Define dash types for plots
#########################################################################
#  0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash

plt_line_types_5 <- c("solid", "dashed", "dashed", "2222", "2222")
plt_line_types_6 <- c("solid", "dotdash", "dashed", "dashed", "2222", "2222")

#########################################################################
# Define additional plotting theme settings for server data
#########################################################################

theme_srv_extra <- theme_minimal() +
    theme(text = element_text(size=8))+
    theme(axis.text.x = element_text(angle=90, vjust=0.5))

#########################################################################
#########################################################################
#########################################################################
#########################################################################
# Collect data from different sources
#########################################################################
#########################################################################
#########################################################################
#########################################################################


#########################################################################
#########################################################################
# Danmarks statistik example
#########################################################################
#########################################################################


# Fetching interest rate data, and filter for mortgage products
#########################################################################
dk_mortgage_interest_raw_data <- data.table(sdk_retrieve_data(table_id = "DNRNURI", 
                                                              DATA = paste0(c("AL51EFFR", "AL51BIDS"),collapse = ","),
                                                              INDSEK = paste0(c("1400"),collapse = ","),
                                                              VALUTA = "z01", 
                                                              LØBETID1 = "ALLE",
                                                              RENTFIX = paste0(c("1A", "2A", "3A", "5A", "10A", "S10A"),collapse = ","),
                                                              LAANSTR = "ALLE",
                                                              Tid = "*")) %>%
    select(-c(VALUTA, LØBETID1, LAANSTR, INDSEK)) %>%
    #mutate(Value = str_replace_all(INDHOLD, pattern = "..", "")) %>%
    mutate(Value = as.double(INDHOLD, na.rm = TRUE)/100,
           RENTFIX = str_replace_all(RENTFIX, pattern = " - - ", "")) %>%
    mutate(RENTFIX = str_replace_all(RENTFIX, pattern = " - ", "")) %>%
    mutate(RENTFIX = str_replace_all(RENTFIX, pattern = "- ", "")) %>%
    mutate(RENTFIX = str_replace_all(RENTFIX, pattern = "Over 6 months and up to and including 1 year", "01 year")) %>%
    mutate(RENTFIX = str_replace_all(RENTFIX, pattern = "Over 1 year and up to and including 2 years", "02 year")) %>%
    mutate(RENTFIX = str_replace_all(RENTFIX, pattern = "Over 2 years and up to and including 3 years", "03 year")) %>%
    mutate(RENTFIX = str_replace_all(RENTFIX, pattern = "Over 3 years and up to and including 5 years", "05 year")) %>%
    mutate(RENTFIX = str_replace_all(RENTFIX, pattern = "Over 5 years and up to and including 10 years", "10 year")) %>%
    mutate(Interest_Fixation = str_replace_all(RENTFIX, pattern = "Over 10 years", "Fixed")) %>%
    mutate(Date = str_replace_all(TID, pattern = "M", "")) %>%
    mutate(Date = paste0(Date, "01")) %>%
    mutate(Date = as.Date(Date,format='%Y%m%d'))  %>%
    mutate(Interest_Fixation = factor(Interest_Fixation, order = TRUE, 
                                      levels = c("01 year",
                                                 "02 year",
                                                 "03 year", 
                                                 "05 year",
                                                 "10 year",
                                                 "Fixed"))) %>%
    select(-INDHOLD, -RENTFIX, -TID) %>%
    filter(!is.na(Value))


DK_yield_curves_rates <- dk_mortgage_interest_raw_data %>%
    filter(DATA != "Administration rate (per cent) (not indexed)",
           !is.na(Value)) %>%
    select(-DATA)


# Plot the data with GGPlot
#########################################################################
DK_rate_curves <- ggplot() +
    geom_line(data = DK_yield_curves_rates,
              mapping = aes(x = Date,
                            y = Value,
                            group = Interest_Fixation,
                            colour = Interest_Fixation),
              lwd = 0.5) +
    labs(x = "Interest fixation term", y = "Rate of interest",
         caption = "Source: Statistics Denmark (Danmarks Statistik), own calculations") +
    scale_colour_manual(values = randompalette) +
    #scale_colour_gradient(low = "#ffffff", high = "#050f80") +
    #facet_wrap(~Growth) +
    scale_y_continuous(labels = pct_scale_settings) +
    theme_srv_extra +
    theme(legend.direction = "vertical",
          legend.box = "horizontal") +
    legend_top_right_inside +
    guides(col = guide_legend(nrow = 3, 
                              byrow = FALSE,
                              title = "Interest fixation"))
DK_rate_curves

ggsave(file.path(image_path,"DK_rate_curves.pdf"), plot = DK_rate_curves, width = 10, height = 9, units = "cm")

# Social benefits data
#########################################################################
# Selection and review of possible data tables to download from DST
# =================================================================
# Search for table names
# 
tables <- sdk_retrieve_tables()

tables_long_income <- tables %>%
    unnest(variables) %>%
    filter(id == "NAN2")


# Retrieve the data for the table and adjust all descriptive
# variables to be usable as variable names in R
dk_indicators_raw_meta <- sdk_retrieve_metadata("NAN2")
dk_indicators_raw <- sdk_retrieve_data("NAN2")

dk_indicators_raw <- sdk_retrieve_data("NAN2", TID = "*", )

dk_indicators <- dk_indicators_raw %>%
    mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = "\\+", "_plus_")) %>%
    mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = "\\-", "_minus_")) %>%
    mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = "B\\.", "B")) %>%
    mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = "D\\.", "D")) %>%
    mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = "P\\.", "D")) %>%
    mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = "\\*", "--")) %>%
    mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = ", real", "--real")) %>%
    mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = " ", "_")) %>%
    mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = "/", "_")) %>%
    mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = "\\.", "-")) %>%
    mutate(TRANSAKT = str_replace_all(TRANSAKT, pattern = ",", "--")) %>%
    mutate(PRISENHED = str_replace_all(PRISENHED, pattern = "^2", "P_2")) %>%
    mutate(PRISENHED = str_replace_all(PRISENHED, pattern = ", ", "--")) %>%
    mutate(PRISENHED = str_replace_all(PRISENHED, pattern = ",", "--")) %>%
    mutate(PRISENHED = str_replace_all(PRISENHED, pattern = " ", "_"))

# Create lists of unique components of each of the id variables
TRANSAKT <- data.table(unique(dk_indicators$TRANSAKT))
PRISENHED <- data.table(unique(dk_indicators$PRISENHED))
TID <- data.table(unique(dk_indicators$TID))



##################################################################################
# Source data from Danmarks Statistik
##################################################################################

Benefits <- sdk_retrieve_data("ESSPROS1")
Benefits <- Benefits %>%
    mutate(FORANSTALT = str_replace_all(FORANSTALT, pattern = "\\. ", " ")) %>%
    mutate(FORANSTALT = str_replace_all(FORANSTALT, pattern = " n.e.c.", "")) %>%
    mutate(FORANSTALT = str_replace_all(FORANSTALT, pattern = " ", "_")) %>%
    mutate(FORANSTALT = str_replace_all(FORANSTALT, pattern = "/", "_")) %>%
    mutate(FORANSTALT = str_replace_all(FORANSTALT, pattern = "\\.", "-")) %>%
    mutate(FORANSTALT = paste0("Cat_", FORANSTALT),
           INDHOLD = as.numeric(INDHOLD))

# Create lists of unique components of each of the id variables
FORANSTALT <- data.table(unique(Benefits$FORANSTALT))
YDELSESTYPE <- data.table(unique(Benefits$YDELSESTYPE))
TID <- data.table(unique(Benefits$TID))

Totals <- data.table(c(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_1_")], 
                       FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_2_")],
                       FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_3_")],
                       FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_4_")],
                       FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_5_")],
                       FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_6_")],
                       FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_7_")],
                       FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_8_")],
                       FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_Sch")]))

# These are the underlying subcomponent groups, same as above
FORANSTALT_SICKNESS_HEALTH_CARE <- data.table(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_1")])
FORANSTALT_DISABILITY <- data.table(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_2")])
FORANSTALT_OLD_AGE <- data.table(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_3")])
FORANSTALT_SURVIVORS <- data.table(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_4")])
FORANSTALT_FAMILY_CHILDREN <- data.table(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_5")])
FORANSTALT_UNEMPLOYMENT <- data.table(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_6")])
FORANSTALT_HOUSING <- data.table(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_7")])
FORANSTALT_SOCIALE_EXCLUSION <- data.table(FORANSTALT$V1[str_detect(FORANSTALT$V1, pattern = "^Cat_8")])


# GDP in denmark
# =================================================================
# 
gdp_current <- dk_indicators %>%
    filter(TRANSAKT == "B1--g_Gross_domestic_product", 
           PRISENHED == "Current_prices",
           TID %in% c(min(Benefits$TID):max(Benefits$TID)))%>%
    mutate(gdp_current = as.numeric(INDHOLD)) %>%
    select(-PRISENHED, -TRANSAKT, -INDHOLD)

# Use the lists and data that was sourced to produce charts
#
# The descriptors here are limited to total expenditures -
#   these are also possible to change to other options such as
#   gross or net cash benefits, or average incomes etc.

# Total values for major categories in nominal values
Totals_plot_simple <- ggplot(data = Benefits %>%
                          filter(FORANSTALT %in% Totals$V1,
                                 YDELSESTYPE == "Total social expenditures") %>%
                          filter(FORANSTALT != "Cat_Schemes_total") %>%
                          select(-YDELSESTYPE) %>%
                          group_by(TID), mapping = aes(x = TID, y = INDHOLD, group = FORANSTALT, fill = FORANSTALT, colour = FORANSTALT)) +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal()
Totals_plot_simple

ggsave(file.path(image_path,"Totals_plot_simple.pdf"), plot = Totals_plot_simple, width = 10, height = 9, units = "cm")

# Total values as a percentage of overall total
Totals_plot_pct_simple <- ggplot(data = Benefits %>%
                              filter(FORANSTALT %in% c(Totals$V1),
                                     YDELSESTYPE == "Total social expenditures") %>%
                              select(-YDELSESTYPE) %>%
                              spread(., key = FORANSTALT, value = INDHOLD) %>%
                              mutate_at(vars(-TID, -Cat_Schemes_total), ~ (.x %>% (function(x) {x/Cat_Schemes_total}))) %>%
                              select(-Cat_Schemes_total) %>%
                              gather(., -TID, key = Spending_category, value = value) %>%
                              group_by(TID),
                          mapping = aes(x = TID,
                                        y = value,
                                        group = Spending_category,
                                        fill = Spending_category,
                                        colour = Spending_category)) +
    labs(x = "Year", y = "Percentage of total for all categories") +
    geom_line() +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(text = element_text(size = 10),
          axis.text.x = element_text(angle=90, vjust=0.5))
Totals_plot_pct_simple

ggsave(file.path(image_path,"Totals_plot_pct_simple.pdf"), plot = Totals_plot_pct_simple, width = 10, height = 9, units = "cm")

# Total values as a percentage of overall total
Totals_plot_pct_gdp_simple <- ggplot(data = Benefits %>%
                                  filter(FORANSTALT %in% c(Totals$V1),
                                         YDELSESTYPE == "Total social expenditures") %>%
                                  select(-YDELSESTYPE) %>%
                                  spread(., key = FORANSTALT, value = INDHOLD) %>%
                                  inner_join(., gdp_current, by = "TID") %>%
                                  select(-Cat_Schemes_total) %>%
                                  mutate_at(vars(-TID, -gdp_current), ~ (.x %>% (function(x) {x/gdp_current}))) %>%
                                  select(-gdp_current) %>%
                                  gather(., -TID, key = Spending_category, value = value) %>%
                                  group_by(TID),
                              mapping = aes(x = as.factor(TID),
                                            y = value,
                                            group = Spending_category,
                                            fill = Spending_category,
                                            colour = Spending_category)) +
    labs(x = "Year", y = "Percentage of GDP (nominal)") +
    geom_line() +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(text = element_text(size = 10),
          axis.text.x = element_text(angle=90, vjust=0.5))

Totals_plot_pct_gdp_simple

ggsave(file.path(image_path,"Totals_plot_pct_gdp_simple.pdf"), plot = Totals_plot_pct_gdp_simple, width = 10, height = 9, units = "cm")


# Subcategory: Old age spending, split into underlying components in nominal levels
Old_age_plot_simple <- ggplot(data = Benefits %>%
                           filter(FORANSTALT %in% FORANSTALT_OLD_AGE$V1,
                                  YDELSESTYPE == "Total social expenditures") %>%
                           select(-YDELSESTYPE) %>%
                           group_by(TID),
                       mapping = aes(x = TID, y = INDHOLD, group = FORANSTALT, fill = FORANSTALT, colour = FORANSTALT)) +
    geom_line() +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma)
Old_age_plot_simple

ggsave(file.path(image_path,"Old_age_plot_simple.pdf"), plot = Old_age_plot_simple, width = 10, height = 9, units = "cm")

# Subcategory: Old age spending, split into underlying components as percentage
#   of total Old age spending.
Old_age_plot_pct_simple <- ggplot(data = Benefits %>%
                               filter(FORANSTALT %in% FORANSTALT_OLD_AGE$V1,
                                      YDELSESTYPE == "Total social expenditures") %>%
                               select(-YDELSESTYPE) %>%
                               spread(., key = FORANSTALT, value = INDHOLD) %>%
                               mutate_at(vars(-TID, -Cat_3_OLD_AGE), ~ (.x %>% (function(x) {x/Cat_3_OLD_AGE}))) %>%
                               select(-Cat_3_OLD_AGE) %>%
                               gather(., -TID, key = Spending_category, value = Percent_of_category)%>%
                               group_by(TID),
                           mapping = aes(x = as.factor(TID),
                                         y = Percent_of_category,
                                         group = Spending_category,
                                         fill = Spending_category,
                                         colour = Spending_category)) +
    labs(x = "Year", y = "Percentage of total in category") +
    geom_line() +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(text = element_text(size = 10),
          axis.text.x = element_text(angle=90, vjust=0.5))
Old_age_plot_pct_simple

ggsave(file.path(image_path,"Old_age_plot_pct_simple.pdf"), plot = Old_age_plot_pct_simple, width = 10, height = 9, units = "cm")


# Nice fancy plots using the settings that were introduced in the beginning of the document
##################################################################################
Totals_plot_pct_fancy <- ggplot(data = Benefits %>%
                              filter(FORANSTALT %in% c(Totals$V1),
                                     YDELSESTYPE == "Total social expenditures") %>%
                              select(-YDELSESTYPE) %>%
                              spread(., key = FORANSTALT, value = INDHOLD) %>%
                              mutate_at(vars(-TID, -Cat_Schemes_total), ~ (.x %>% (function(x) {x/Cat_Schemes_total}))) %>%
                              select(-Cat_Schemes_total) %>%
                              gather(., -TID, key = Category, value = value) %>%
                              mutate(Category = str_replace_all(Category, pattern ="_", " "),) %>%
                              mutate(Category = str_to_sentence(Category, locale = "en")) %>%
                              group_by(TID),
                          mapping = aes(x = as.factor(TID),
                                        y = value,
                                        group = Category,
                                        fill = Category,
                                        colour = Category)) +
    labs(x = "Year", 
         y = "Per cent total SBEN",
         caption = "Source: Statistics Denmark (Danmarks Statistik), own calculations") +
    geom_line(lwd = plot_line_width) +
    scale_y_continuous(labels = pct_scale_settings) +
    theme_srv_extra +
    theme(legend.direction = "vertical",
          legend.box = "vertical") +
    legend_top_left_inside +
    guides(col = guide_legend(nrow = 9, byrow = FALSE))
Totals_plot_pct_fancy

ggsave(file.path(image_path,"Totals_plot_pct_fancy.pdf"), plot = Totals_plot_pct_fancy, width = 10, height = 9, units = "cm")


Old_age_plot_pct_fancy <- ggplot() +
    geom_line(mapping = aes(x = as.factor(TID),
                            y = Percent_of_category,
                            group = Category,
                            colour = Category),
              data = Benefits %>%
                  filter(FORANSTALT %in% FORANSTALT_OLD_AGE$V1,
                         YDELSESTYPE == "Total social expenditures") %>%
                  select(-YDELSESTYPE) %>%
                  spread(., key = FORANSTALT, value = INDHOLD) %>%
                  mutate_at(vars(-TID, -Cat_3_OLD_AGE), ~ (.x %>% (function(x) {x/Cat_3_OLD_AGE}))) %>%
                  select(-Cat_3_OLD_AGE) %>%
                  gather(., -TID, key = Category, value = Percent_of_category)%>%
                  mutate(Category = str_replace_all(Category, pattern ="_", " "),) %>%
                  group_by(TID)) +
    labs(x = "Year", y = "Per cent old age SBEN",
         caption = "Source: Statistics Denmark (Danmarks Statistik), own calculations") +
    geom_line(lwd = plot_line_width) +
    scale_y_continuous(labels = pct_scale_settings) +
    theme_srv_extra +
    theme(legend.direction = "vertical",
          legend.box = "vertical") +
    legend_top_left_inside +
    guides(col = guide_legend(nrow = 6, byrow = FALSE))
Old_age_plot_pct_fancy

ggsave(file.path(image_path,"Old_age_plot_pct_fancy.pdf"), plot = Old_age_plot_pct_fancy, width = 10, height = 9, units = "cm")


#########################################################################
#########################################################################
# OECD example
#########################################################################
#########################################################################

###########################################################################
# OECD Share prices
###########################################################################

OECD_shareprice_url <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/AUT+BEL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+IRL+ITA+LVA+LUX+NLD+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+EA19+EU28+A5M.SPASTT01+SPINTT01.ST+STSA+IXOB+IXOBSA+NCCU+NCCUSA+CXCU+CXCUSA.A/all?startTime=2010&endTime=2018"

country_codes <- read_excel(file.path(data_path,"Country codes 2 letter 3 letter.xlsx"))

OECD_shareprice <- as.data.frame(readSDMX(OECD_shareprice_url))
OECD_SP_all <- OECD_shareprice %>%
    filter(SUBJECT=="SPASTT01") %>%
    mutate(iso = LOCATION) %>%
    select(iso, obsValue, obsTime)


OECD_rebase <- OECD_SP_all %>%
    filter(obsTime == 2010) %>%
    mutate(rebase = obsValue) %>%
    select(iso, rebase)

OECD_SP_all_long <- OECD_SP_all %>%
    left_join(., OECD_rebase, by = "iso") %>%
    mutate(Values = (obsValue / rebase) * 100) %>%
    select(-rebase, -obsValue) %>%
    left_join(., country_codes, by = "iso") %>%
    mutate(Country = COUNTRY) %>%
    select(-iso, -COUNTRY) %>%
    filter(!is.na(Values),
           !is.na(geo)) %>%
    arrange(geo)

OECD_SP_all <- OECD_SP_all %>%
    left_join(., OECD_rebase, by = "iso") %>%
    mutate(Values = (obsValue / rebase) * 100) %>%
    select(-rebase, -obsValue) %>%
    left_join(., country_codes, by = "iso") %>%
    mutate(Country = COUNTRY) %>%
    select(-iso, -COUNTRY, -Country) %>%
    filter(!is.na(Values),
           !is.na(geo)) %>%
    arrange(geo) %>%
    spread(key = geo, value = Values)

OECD_share_prices_ts <- ts(OECD_SP_all[, -c(1)], start = 2010, end = 2018, freq=1)

DK_share_prices <- ggplot() +
    geom_line(data = OECD_SP_all_long %>%
                  mutate(Country = geo) %>%
                  select(-geo),
              mapping = aes(x = factor(obsTime),
                            y = Values,
                            colour = Country,
                            group = Country),
              lwd = plot_line_width) +
    labs(x = "Year", 
         y = "Share price index, 2010=100",
         caption = "Source: OECD") +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_color_manual(values = c("grey","grey","grey","grey","grey",
                                  "#ffc100","#691e36","grey","grey","grey",
                                  "grey","grey","grey","#497840","grey",
                                  "grey","#443eff","grey","#0bb3b0","grey",
                                  "grey","grey","grey","grey","grey",
                                  "grey","grey","grey")) +
    theme_srv_extra +
    theme(legend.direction = "vertical",
          legend.box = "vertical") +
    legend_bottom_right_inside +
    guides(col = guide_legend(nrow = 3, byrow = FALSE))
DK_share_prices

ggsave(file.path(image_path,"DK_share_prices.pdf"), plot = DK_share_prices, width = 10, height = 9, units = "cm")

#########################################################################
# OECD house prices
#########################################################################

# Step 1: Search for a phrase in the OECD database that might match the table or description of the table you are looking for

search_dataset("house price", data = get_datasets(), ignore.case = TRUE)

# This search returns just one table, id = "HOUSE_PRICES" title = "Analytical house prices indicators"

#########################################################################
# Takes for ever to download
#########################################################################

# OECD_house_price_raw <- get_dataset("HOUSE_PRICES")

# Check the structure of the data that you have just downloaded, and what each of the unique values are for each column (except the values column, there will be thousands of unique values there.)

str(OECD_house_price_raw)

# Each of these lines will provide a list of each of the attributes of the OECD_house_price_raw file
# that was just created in the "get_dataset" command above

unique(OECD_house_price_raw$COU)
unique(OECD_house_price_raw$IND)
unique(OECD_house_price_raw$TIME_FORMAT)
unique(OECD_house_price_raw$UNIT)
unique(OECD_house_price_raw$POWERCODE)
unique(OECD_house_price_raw$obsTime)

# This data can then be cleaned up according to the specific index or data type that you want.

OECD_house_prices_data <- OECD_house_price_raw %>%
    filter(
        IND == "RPI",         # for "real house price index"
        TIME_FORMAT == "P1Y", # for annual based data
        UNIT == "IDX",        # for a indexed values
        !nchar(obsTime,) > 4,
        COU != "TUR"
    ) %>%
    mutate(iso = COU) %>%
    select(iso, obsTime, obsValue) # remove all the unnecessary columns that you have just filtered to just one type

# Rebasing an index: House price index for Denmark, base 2010
#########################################################################

# The default year for the data might be 2006 or 2010. For my purposes,
# I want the index to be measured relative to the value in 2010. The dataset
# can therefore be "rebased" (i.e. given a new base year) to 2010 as follows.


# First we create a vector of values from the year that we want to base the values to.
# i.e. this will one log column of data that only has the value for 2010 in all rows.

OECD_rebase <- OECD_house_prices_data %>%
    filter(obsTime == 2000) %>%
    mutate(rebase = obsValue) %>%
    select(iso, rebase)

# We then divide the actual list of data by the new list. (i.e. each entry is divided by
# the value from 2010)

OECD_house_prices <- OECD_house_prices_data %>%
    left_join(., OECD_rebase, by = "iso") %>%
    mutate(Values = (obsValue / rebase) * 100) %>%
    select(-rebase, -obsValue) %>%
    left_join(., country_codes, by = "iso") %>%
    mutate(Country = COUNTRY) %>%
    select(-iso, -COUNTRY) %>%
    filter(!is.na(Values),
           !is.na(geo))

write.csv(OECD_house_prices, file.path(data_path,"OECD_house_price_data.csv"))


# Plot the house prices for Denmark

plot_OECD_house_prices <- ggplot(data = OECD_house_prices %>%
                                     filter(obsTime %in% (1990:2018),
                                            geo %in% c("CH", "DK", "GR")) %>%
                                     group_by(Country),
                                 mapping = aes(x = factor(obsTime),
                                               y = Values,
                                               group = geo,
                                               colour = geo)) +
    labs(x = "Year", y = "Real house prices, Base = 2000",
         caption = "Source: OECD data, data.oecd.org") +
    scale_y_continuous(labels = scales::comma_format()) +
    facet_wrap(~Country) +
    geom_line(lwd = plot_line_width) +
    geom_abline(intercept = 100, slope = 0, colour = "#6b6b6b") +
    theme_srv_extra +
    theme(legend.direction = "vertical",
          legend.box = "horizontal") +
    legend_bottom_right_inside

plot_OECD_house_prices

ggsave(file.path(image_path,"plot_OECD_house_prices.pdf"), plot = plot_OECD_house_prices, width = 10, height = 9, units = "cm")

#########################################################################
#########################################################################
# Eurostat example
#########################################################################
#########################################################################

# To get the full datasets, and then to be able to filter or edit those
# datasets after downloading them

#Source Financial balance sheets data
f_bs <- get_eurostat(id = "nasa_10_f_bs",  time_format = "num")
#Source Financial transactions data
f_tr <- get_eurostat(id = "nasa_10_f_tr",  time_format = "num")
#Source Revaluation account data
f_gl <- get_eurostat(id = "nasa_10_f_gl",  time_format = "num")
#Source Other changes in volume data
f_oc <- get_eurostat(id = "nasa_10_f_oc",  time_format = "num")
#Source Non-financial transactions data
nf_tr <- get_eurostat(id = "nasa_10_nf_tr",  time_format = "num")
#Source Balance sheets for non-financial assets data
nfa_bs <- get_eurostat(id = "nama_10_nfa_bs",  time_format = "num")

FIN_BS <- f_bs %>%
    filter(geo == "DK", unit == "MIO_NAC", co_nco == "NCO")

FIN_TR <- f_tr %>%
    filter(geo == "DK", unit == "MIO_NAC", co_nco == "NCO")

FIN_CG <- f_gl %>%
    filter(geo == "DK", unit == "MIO_NAC", co_nco == "NCO")

FIN_OC <- f_oc %>%
    filter(geo == "DK", unit == "MIO_NAC", co_nco == "NCO")

NFTR <- nf_tr %>%
    filter(geo == "DK", unit == "CP_MNAC")

NFBS <- nfa_bs %>%
    filter(geo == "DK", unit == "CP_MNAC")



# Source Non-financial transactions data
nf_tr <- get_eurostat(id = "nasa_10_nf_tr",  time_format = "num")
gdp_esa_95 <- get_eurostat(id = "namq_gdp_k",  time_format = "num")


# To get disposable income for the household sector
Yd_h_data <- nf_tr %>% # We make a new dataset from the original download, but leave the original in tact
    filter(unit == "CP_MNAC", # This is the currency unit that we want - current prices in the national currency
           na_item == "B6G", # This is the national accounts code for disposable income
           direct == "RECV", # We can choose paid or received.. income is received
           values != 0, # We can remove all zero entries with the (not) symbol "!"
           sector == "S14") %>% # We choose the sector to filter (households = S14)
    arrange(geo, sector, time) %>% # This line just sorts according to the columns specified
    mutate(Yd_hh = values,
           year = time,
           geo = as.character(geo)) %>% # These lines are just to create new, renamed variables
    select(-unit, -values, -na_item, -direct, -time, -sector) # These are the columns we want to "de-select" / remove

write.csv(Yd_h_data, file.path(data_path,"Yd_h_data.csv")) # We then write it to the data path as a simple text file

#########################################################################
#########################################################################
# QUANDL example
#########################################################################
#########################################################################

Australia=Quandl("WGEM/AUS_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Austria=Quandl("WGEM/AUT_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Belgium=Quandl("WGEM/BEL_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Canada=Quandl("WGEM/CAN_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Switzerland=Quandl("WGEM/CHE_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Chile=Quandl("WGEM/CHL_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Germany=Quandl("WGEM/DEU_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Denmark=Quandl("WGEM/DNK_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Spain=Quandl("WGEM/ESP_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
France=Quandl("WGEM/FRA_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
United_Kingdom=Quandl("WGEM/GBR_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Greece=Quandl("WGEM/GRC_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Hungary=Quandl("WGEM/HUN_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Ireland=Quandl("WGEM/IRL_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Italy=Quandl("WGEM/ITA_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Japan=Quandl("WGEM/JPN_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Latvia=Quandl("WGEM/LVA_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Netherlands=Quandl("WGEM/NLD_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Norway=Quandl("WGEM/NOR_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Poland=Quandl("WGEM/POL_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Portugal=Quandl("WGEM/PRT_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Slovakia=Quandl("WGEM/SVK_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
Sweden=Quandl("WGEM/SWE_REER", api_key="bf2dQBGfsyzKNi-ccxAn")
United_States=Quandl("WGEM/USA_REER", api_key="bf2dQBGfsyzKNi-ccxAn")

Australia_1 <- Australia  %>% as_tibble() %>%
    mutate(
        datasource= "AUS"
    )
Austria_1 <- Austria  %>% as_tibble() %>%
    mutate(
        datasource= "AUT"
    )
Belgium_1 <- Belgium  %>% as_tibble() %>%
    mutate(
        datasource= "BEL"
    )
Canada_1 <- Canada  %>% as_tibble() %>%
    mutate(
        datasource= "CAN"
    )
Switzerland_1 <- Switzerland  %>% as_tibble() %>%
    mutate(
        datasource= "CHE"
    )
Chile_1 <- Chile  %>% as_tibble() %>%
    mutate(
        datasource= "CHL"
    )
Germany_1 <- Germany  %>% as_tibble() %>%
    mutate(
        datasource= "DEU"
    )
Denmark_1 <- Denmark  %>% as_tibble() %>%
    mutate(
        datasource= "DNK"
    )
Spain_1 <- Spain  %>% as_tibble() %>%
    mutate(
        datasource= "ESP"
    )
France_1 <- France  %>% as_tibble() %>%
    mutate(
        datasource= "FRA"
    )
United_Kingdom_1 <- United_Kingdom  %>% as_tibble() %>%
    mutate(
        datasource= "GBR"
    )
Greece_1 <- Greece  %>% as_tibble() %>%
    mutate(
        datasource= "GRC"
    )
Hungary_1 <- Hungary  %>% as_tibble() %>%
    mutate(
        datasource= "HUN"
    )
Ireland_1 <- Ireland  %>% as_tibble() %>%
    mutate(
        datasource= "IRL"
    )
Italy_1 <- Italy  %>% as_tibble() %>%
    mutate(
        datasource= "ITA"
    )
Japan_1 <- Japan  %>% as_tibble() %>%
    mutate(
        datasource= "JPN"
    )
Latvia_1 <- Latvia  %>% as_tibble() %>%
    mutate(
        datasource= "LVA"
    )
Netherlands_1 <- Netherlands  %>% as_tibble() %>%
    mutate(
        datasource= "NLD"
    )
Norway_1 <- Norway  %>% as_tibble() %>%
    mutate(
        datasource= "NOR"
    )
Poland_1 <- Poland  %>% as_tibble() %>%
    mutate(
        datasource= "POL"
    )
Portugal_1 <- Portugal  %>% as_tibble() %>%
    mutate(
        datasource= "PRT"
    )
Slovakia_1 <- Slovakia  %>% as_tibble() %>%
    mutate(
        datasource= "SVK"
    )
Sweden_1 <- Sweden  %>% as_tibble() %>%
    mutate(
        datasource= "SWE"
    )
United_States_1 <- United_States  %>% as_tibble() %>%
    mutate(
        datasource= "USA"
    )



#Bind downloaded series together for export, reshaping or analysis 
rex <- rbind(Australia_1, Austria_1, Belgium_1, Canada_1, Switzerland_1, Chile_1, Germany_1, Denmark_1, Spain_1, France_1, United_Kingdom_1, Greece_1, Hungary_1, Ireland_1, Italy_1, Japan_1, Latvia_1, Netherlands_1, Norway_1, Poland_1, Portugal_1, Slovakia_1, Sweden_1, United_States_1)


write.csv(rex, filepath(data_path,"rex.csv"))

#########################################################################
#########################################################################
# QUANDL example
#########################################################################
#########################################################################




#########################################################################
#########################################################################
# QUANDL example
#########################################################################
#########################################################################


