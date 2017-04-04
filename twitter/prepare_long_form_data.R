### prepare_long_form_data.R ######################################################################
# Extract relevant data from database and convert to long format.

# Usage:
#   1) ssh -L 3300:localhost:3306 username@host
#   2) Run script

### PREAMBLE ######################################################################################
library(reshape2);
library(RMySQL);
library(yaml);

# clear workspace in an effort to combat bugs
rm(list = ls(all = TRUE));

options(stringsAsFactors = FALSE);

# read database settings
settings <- yaml.load_file('twitter/database_settings.yaml');

# year we're interested in
year <- 2016;

### GET DATA FROM DATABASE ########################################################################

mydb <- dbConnect(
  MySQL(), 
  user = settings$username, 
  password = settings$password, 
  dbname = 'wuergh', 
  host = '127.0.0.1',
  port = 3300
  );

trend.request <- dbSendQuery(
  mydb, 
  paste('SELECT * FROM tt WHERE YEAR(attime) =', year)
  );

trends <- fetch(
  trend.request, 
  n = -1
  );

### REFORMAT ######################################################################################

# convert to long format
trends.long <- melt(
  trends, 
  id.vars = 'attime'
  );

# make rank numeric
trends.long$rank <- as.numeric(
  substr(
    trends.long$variable,
    2,
    nchar(as.character(trends.long$variable))
    )
  );

# remove old variable containing rank information
trends.long$variable <- NULL;

names(trends.long) <- c('time', 'trend', 'rank');

### SAVE TO FILE ##################################################################################

write.table(
    trends.long,
    file.path(
        '~/assorted-analyses/data', 
        paste0(
            year,
            '_trends.txt'
            )
        ),
    row.names = FALSE
    );
