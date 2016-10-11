#### Load libraries for the project ####
library(data.table)   # Data Loading
library(dplyr)        # Data Manupulation
library(caTools)      # Data Manupulation
library(ggplot2)      # Visualization
library(ggmap)        # Visualization
library(maps)         # Visualization
library(RColorBrewer) # Visualization

# load UDFs
sapply(paste0('UDF/', list.files('UDF')), source)

## load csv
collision <- read.csv('vehicular_crash_data.csv', stringsAsFactors = F)

## transform date and time
work_dt <- collision %>%
  mutate(YEAR = year(REPORTDATE),
         MONTH = month(REPORTDATE), 
         DAY = mday(REPORTDATE)
  ) %>%
  as.data.table 

# Data group for yearly comparison (each period June - May)
work_dt[MONTH < 6, PERIOD := paste(YEAR - 1, YEAR, sep = '-')] 
work_dt[MONTH >= 6, PERIOD := paste(YEAR, YEAR + 1, sep = '-')]

# Remove older data
work_dt <- work_dt[PERIOD !='2010-2011']
work_dt <- work_dt[PERIOD !='2011-2012']

# Group by 12-month period for analysis 

g <- ggplot(data = sample_set(work_dt),
            aes(x = factor(month(REPORTDATE)), 
                y = YEAR))
g +
  geom_tile(aes(fill = PERIOD), color = 'white') +
  labs(x = 'Month') +
  # ggtitle('12 Month Periods For Yearly Comparison') +
  theme(legend.position = 'top',
        legend.title = element_text(face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# Count of accidents per period 

accident_n_dt <- work_dt %>%
  group_by(PERIOD) %>%
  summarise(N = n())

ggplot(data = accident_n_dt) +
  geom_bar(aes(x = PERIOD, y = N, fill = PERIOD), stat = 'identity') +
  labs(y = NULL, 
       title = 'Number of Motor Vehicle Collisions for Each 12-Month Period') +
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(x = PERIOD, y = N - 2000, label = N), size = 5)

# Accident count percentage change

accident_p_vec <- c(0, tail(accident_n_dt$N, nrow(accident_n_dt) - 1) / 
                      head(accident_n_dt$N, nrow(accident_n_dt) - 1) - 1)


# Number of active vehicle registrations in DC 
# http://dmv.dc.gov/page/vehicle-registration-historical-data

reg_dc <- data.table(Year = seq(2012, 2015),
                     Reg_num = c(284424,
                                 286715,
                                 292245,
                                 299276))

# Active registration count percentage change

reg_p_vec <- c(0, tail(reg_dc$Reg_num, nrow(reg_dc)-1) /
                 head(reg_dc$Reg_num, nrow(reg_dc)-1) - 1)

temp <- data.frame(PERIOD = rep(sort(unique(work_dt$PERIOD)), 2),
                   TYPE   = rep(c('Motor Vehicle Collision',
                                  'Vehicle Registration in DC'), each = 4),
                   VALUE  = c(accident_p_vec, reg_p_vec))

ggplot(data = temp,
       aes(x = PERIOD, y = VALUE, color = TYPE, group = TYPE)) + 
  geom_point(size = 3) +
  geom_line(size = 2, alpha = .8) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = 'none') +
  labs(title = 'Yearly Growth Rate', y = '% Growth Since Last Year') +
  facet_wrap( ~ TYPE, nrow = 2)

# Subsets for each topic

# base data
right_dt  <- work_dt %>%
  select(-starts_with('CRIMEID'), 
         -starts_with('ISREPORTONSCENE'), 
         -matches('TYPE|ADDRESS_ID|INVOLVED|IES|STREET|OBJECTID|PHOTOS|RUNTIME|WEATHER|MAJORCRASH|JUNCTION|RELATED|TRAFFIC|WORKZONE|LIGHT|SPEEDLIMIT|FIRSTHARMFUL|DCLOCATION|IMAGE|SOURCE|ISC|ISD'),
         ID = `CRASHID`)

# injuries
injur_dt <- melt_count(work_dt, 'ends', 'INJURIES', 'stat') %>%
  merge(right_dt, by ='ID')

#remove NAs
injur_dt <- injur_dt[!is.na(injur_dt$INJURIES_V),]

# injury count by type 

injur_n_dt <- injur_dt %>%
  group_by(PERIOD, INJURIES) %>%
  summarise(Injury = sum(INJURIES_V)) %>%
  filter(!grepl('PERSONS', INJURIES)) %>% #Number of persons is the total of the two stats
  arrange(desc(Injury)) %>%
  mutate(INJURIES = INJURIES %>%
           gsub('NUMBER OF ', '', .) %>%
           gsub(' INJURIES', '', .) %>%
           factor(levels = c('MINORINJURIES',
                             'MAJORINJURIES')))

# injury total by period

injur_n_label_dt <- injur_n_dt %>%
  group_by(PERIOD) %>%
  summarise(N = sum(Injury))

# visualize

g <- ggplot(data = injur_n_dt) +
  geom_bar(aes(x = PERIOD, y = Injury, fill = reorder(INJURIES, Injury)),
           stat = 'identity') + 
  scale_fill_manual(values = c("#468966", "#77C4D3")) +
  guides(fill = guide_legend(ncol = 3, title = 'Injury Type')) +
  theme(legend.position = 'top',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# stacked bar
g + geom_text(data = injur_n_label_dt,
              aes(x = PERIOD, y = N + 500, label = N), size = 5)

# separate bars

g + facet_wrap(~ INJURIES)

# calculate injury per accident

temp <- injur_n_label_dt %>% 
  mutate(injur_rate = N / accident_n_dt$N)

# visualize injury per accident

ggplot(data = temp, aes(x = PERIOD, y = injur_rate)) +
  geom_line(group = 1, size = 2, alpha = .5, color = 'red') +
  geom_label(aes(label = round(injur_rate, 3))) +
  labs(x = NULL, y = 'Injury Per Accident') +
  theme(legend.position = 'none', 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  coord_cartesian(ylim = c(0, .7))

# fatalities
kill_dt <- melt_count(work_dt, 'matches', 'FATALITIES', 'stat') %>% 
  merge(right_dt, by = 'ID')

#remove NAs
kill_dt <- kill_dt[!is.na(kill_dt$FATALITIES_V),]

# deaths total by period

kill_n_label_dt <- kill_dt %>%
  group_by(PERIOD) %>%
  summarise(N = sum(FATALITIES_V))

#visualize

ggplot(data = kill_n_label_dt, aes(x = PERIOD, y = N + 5, label = N), size = 5) +
  geom_bar(fill="#CC0000", stat="identity") +
    guides(fill=FALSE) +
    xlab("") + ylab("Fatalities") +
    ggtitle("Vehicular Crash Fatalities in DC") +
  geom_text(aes(y = N + 2), size = 5)

# deaths per 1000 accidents 

temp <- data.frame(PERIOD = kill_n_label_dt$PERIOD,
                   d_rate = kill_n_label_dt$N / accident_n_dt$N * 1000)

# Visualize deaths per 1000 accidents

ggplot(data = temp, aes(x = PERIOD, y = d_rate)) + 
  geom_point(shape = 16, size = 3, color = 'red', alpha = .5) +
  geom_line(aes(group = 1), size = 2, color = 'red', alpha = .5) +
  labs(x = NULL, y = 'Deaths Per 1000 Accidents') +
  theme(legend.position = 'none') +
  geom_label(aes(y = d_rate + .03, label = round(d_rate, 2)))

                   
## Find deadly spots 

# Accident count by location 

mvc_count <- work_dt %>%
  filter(!(is.na(X) | is.na(Y))) %>%
  mutate(LAT = round(Y, 3),
         LON = round(X, 3)) %>%
  group_by(LAT, LON) %>%
  summarise(N = n())

# Top injuries by location 

mvc_inj <- injur_dt %>%
  filter(!(is.na(X) | is.na(Y))) %>%
  mutate(LAT = round(Y, 3),
         LON = round(X, 3)) %>%
  select(INJURIES_V, LAT, LON) %>%
  group_by(LAT, LON) %>%
  summarise(INJURIES_V = sum(INJURIES_V)) %>%
  arrange(desc(INJURIES_V)) %>%
  filter(INJURIES_V > 50)

# Top deaths by location 

mvc_kill <- kill_dt %>%
  filter(!(is.na(X) | is.na(Y))) %>%
  mutate(LAT = round(Y, 3),
         LON = round(X, 3)) %>%
  select(FATALITIES_V, LAT, LON) %>%
  group_by(LAT, LON) %>%
  summarise(FATALITIES_V = sum(FATALITIES_V)) %>%
  arrange(desc(FATALITIES_V)) 

# Load DC map 

dc_map <- get_map(location = find_map_cent(mvc_count$LON,
                                           mvc_count$LAT),
                  source = 'google',
                  maptype = 'roadmap',
                  color = 'bw',
                  zoom = 12)
save(dc_map, file = 'dc_map.RData')
load('dc_map.RData')

ggmap(dc_map, extent = 'device') +
  geom_point(data = mvc_count, alpha = .5,
             aes(x = LON, y = LAT, color = N, size = N)) +
  scale_color_gradient(low= 'white', high = 'red') +
  theme(legend.justification=c(1,1), legend.position=c(.95,.95),
        legend.background = element_rect(color = 'black',
                                         fill = alpha('white', 0.8)),
        legend.title = element_blank()) +
  guides(size = FALSE) +
  ggtitle('Accident Frequencies  by Location')

