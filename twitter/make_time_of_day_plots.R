### make_time_plots.R #############################################################################
# Plot trends by time of day.

### PREAMBLE ######################################################################################
library(BoutrosLab.plotting.general);
library(lubridate);
library(chron);

year <- 2016;

selected.trend <- '#WednesdayWisdom';

### create.time.of.day.plot #######################################################################
# Description:
#   Plot trending topic rank by time of day.
# Input variables:
#   trends          long format data frame containing all trends.
#   selected.trend  string given trend of interest
# Output variable:
#   time.of.day.plot    plot as a trellis object 
create.time.of.day.plot <- function(trends, selected.trend) {
    
    ### PROCESS DATA ##########################################################
    
    trend.data <- trends[selected.trend == trends$trend, ];
    trend.data <- trend.data[order(trend.data$torontotime), ];
    
    n <- nrow(trend.data);
    
    time.diff <- difftime(
        trend.data$torontotime[2:n],
        trend.data$torontotime[1:(n-1)],
        units = 'mins'
        );
    
    # add first to make same length as data frame
    time.diff <- c(0, time.diff);
    
    # find consecutive segments
    trend.data$consecutive <- ifelse(
        time.diff < 5, # using 5-minutes as cutoff seems pretty safe
        1, 
        0
        );
    
    # split consecutive ranks that pass midnight, to avoid having ugly lines across plot
    newday <- c(
        FALSE,
        trend.data$date[2:n] != trend.data$date[1:(n - 1)]
        );
    
    trend.data$consecutive[newday] <- 0;
    
    # add segment information to data frame
    trend.data$group <- NA;
    
    last.index <- 1;
    group.count <- 1;
    
    for (index in which(0 == trend.data$consecutive)) {
        trend.data$group[last.index:(index - 1)] <- group.count;
        
        last.index <- index;
        group.count <- group.count + 1;
    }
    
    ### BUILD PLOT ############################################################
    
    time.of.day.plot <- create.scatterplot(
        y.coord ~ as.numeric(timeofday), 
        trend.data,
        # groups
        groups = as.factor(trend.data$group), 
        type = 'l',
        col = 'gray15',
        # x-axis
        xlimits = c(0, 1),
        xat = seq(0, 1, length.out = 9), 
        xaxis.lab = c('12 am', '3 am', '6 am', '9 am', 
                      '12 pm', '3 pm', '6 pm', '9 pm', '12 am'),
        xaxis.cex = 0.7, 
        xlab.label = '',
        # y-axis
        ylimits = c(0.5, 10.5),
        yat = 1:10,
        yaxis.lab = 10:1,
        ylab.label = 'Rank', 
        ylab.cex = 1.2, 
        yaxis.cex = 0.6,
        # turn off box
        # style = 'Nature'
        # add text
        add.text = TRUE,
        text.labels = selected.trend,
        text.x = 0.012,
        text.y = 10,
        text.cex = 0.61,
        text.col = 'firebrick'
        );
    
    return(time.of.day.plot);
}


### PROCESS DATA ##################################################################################

if(!exists('trends')) {
    trends <- read.table(
        paste0(
            '~/assorted-analyses/data/',
            year,
            '_trends.txt'
        ),
        header = TRUE
    );
    

    trends$time <- strptime(
        trends$time,
        format = '%Y-%m-%d %H:%M:%S',
        tz = 'CET'
    );
    
    # convert timezones, server is CET 
    trends$torontotime <- with_tz(
        trends$time,
        tzone = 'America/Toronto'
        );
    
    # extract time of day without date
    trends$timeofday <- times(
        strftime(
            trends$torontotime, 
            format = '%H:%M:%S'
            )
        );
    
    trends$y.coord <- 11 - trends$rank;
    
    trends$date <- as.Date(trends$torontotime);
    
    trends$weekday <- weekdays(trends$torontotime);
    
}

### MAKE PLOTS ####################################################################################

all.plots <- list();

weekday.trends <- c(
    '#SundayFunday', 
    '#FridayFeeling',
    '#ThursdayThoughts', 
    '#WednesdayWisdom',
    '#TravelTuesday', 
    '#mondaymotivation'
    );

for (trend in weekday.trends) {
    all.plots[[trend]] <- create.time.of.day.plot(
        trends, 
        selected.trend = trend
        );
}

# assemble into a single plot
multiplot <- create.multiplot(
    all.plots, 
    plot.layout = c(1, length(all.plots)), 
    style = 'Nature',
    # technicalities
    filename = '~/assorted-analyses/plots/day_trend_multiplot.png',
    resolution = 300,
    height = 8,
    width = 7,
    # x-axis
    xlimits = c(0, 1),
    xat = seq(0, 1, length.out = 9), 
    xaxis.lab = c('12 am', '3 am', '6 am', '9 am', 
                  '12 pm', '3 pm', '6 pm', '9 pm', '12 am'),
    xaxis.cex = 0.7, 
    xlab.label = 'Time of Day',
    xlab.cex = 1.0, 
    # y-axis
    ylimits = c(0.5, 10.5),
    yat = 1:10,
    yaxis.lab = 10:1,
    ylab.label = 'Rank', 
    ylab.cex = 1.0, 
    yaxis.cex = 0.6
    );
