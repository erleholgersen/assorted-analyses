### plot_trend_recurrence.R #######################################################################
# Plot recurrence of trend over time.

# Pre-requisites: 
#   prepare_long_form_data.R 

### PREAMBLE ######################################################################################
library(BoutrosLab.plotting.general);
library(lubridate);

# year of interest
year <- 2016;

# trend of interest
trend <- '#onstorm';

### READ IN DATA ##################################################################################

if(!exists('trends')) {
    trends <- read.table(
        file.path(
            '~/assorted-analyses/data/', 
            paste0(
                year,
                '_trends.txt'
                )
            ),
        header = TRUE
        );
    
    unique.times <- unique(trends$time);
    
    trends$time <- strptime(
        trends$time,
        format = '%Y-%m-%d %H:%M:%S',
        tz = 'CET'
        );
    
    trends$torontotime <- with_tz(
        trends$time,
        tzone = 'America/Toronto'
        );
    
    
    trends$y.coord <- 11 - trends$rank;
    
}

### SUBSET OUT TREND OF INTEREST ##################################################################

trend.data <- trends[trend == trends$trend, ];

trend.data <- trend.data[order(trend.data$torontotime), ];

n <- nrow(trend.data);

time.diff <- difftime(
    trend.data$torontotime[2:n],
    trend.data$torontotime[1:(n-1)],
    units = 'mins'
);

# add first to make same length as data frame
time.diff <- c(0, time.diff);

trend.data$consecutive <- ifelse(
    time.diff < 5, # using 5-minutes as cutoff seems pretty safe
    1, 
    0
    );

# add group to data frame, show us where to 
trend.data$group <- NA;

last.index <- 1;
group.count <- 1;

for (index in which(0 == trend.data$consecutive)) {
    trend.data$group[last.index:(index - 1)] <- group.count;
    
    last.index <- index;
    group.count <- group.count + 1;
}


### PLOT ##########################################################################################

# convert to numeric since BPG has trouble with POSIXct
trend.data$x.coord <- as.numeric(trend.data$torontotime);

# specify x-axis limits
xlimits <- as.numeric(
    c(
        as.POSIXct('2016-01-01'),
        as.POSIXct('2017-01-01')
        )
    );

# specify x-axis ticks
xat <- as.numeric(
    c(
        as.POSIXct('2016-01-15'),
        as.POSIXct('2016-02-14'),
        as.POSIXct('2016-03-15'),
        as.POSIXct('2016-04-15'),
        as.POSIXct('2016-05-15'),
        as.POSIXct('2016-06-15'),
        as.POSIXct('2016-07-15'),
        as.POSIXct('2016-08-15'),
        as.POSIXct('2016-09-15'),
        as.POSIXct('2016-10-15'),
        as.POSIXct('2016-11-15'),
        as.POSIXct('2016-12-15')
        )
    );

# make plot!
create.scatterplot(
    y.coord ~ x.coord, 
    trend.data, 
    type = 'l',
    groups = trend.data$group,
    # technicalities
    filename = paste0(
        '~/assorted-analyses/plots/', 
        trend,
        '.png'
        ),
    width = 9,
    height = 3,
    resolution = 300,
    # x-axis
    xlimits = xlimits,
    xat = xat,
    xaxis.lab = c(
        'Jan.', 'Feb.', 'March', 'April', 'May', 'June', 'July', 'Aug.',
        'Sept.', 'Oct.', 'Nov.', 'Dec.'),
    xaxis.cex = 0.8,
    xaxis.tck = 0,
    xlab.label = '',
    # y-axis
    ylimits = c(0.5, 10.5),
    yat = seq(1, 10), 
    yaxis.lab = seq(10, 1),
    yaxis.cex = 0.8,
    ylab.label = 'Rank',
    ylab.cex = 1.1,
    # title
    main = trend,
    main.cex = 1.25,
    # style
    style = 'Nature'
    );
