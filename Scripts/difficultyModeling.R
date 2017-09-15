# pulled rawdat.txt from http://donjon.bin.sh/5e/calc/enc_size.html
library(tidyverse)

# read in data
raw <- readLines('~/Downloads/rawdat.txt')

# difficulty level for each table
difficulty <- raw[1:length(raw) %% 22 == 1] %>%
    gsub(pattern = '##', replacement = '') %>%
    strsplit(split = ',') %>%
    sapply(`[`, 1)

# number of players for each table
nPlayers <- raw[1:length(raw) %% 22 == 1] %>%
    gsub(pattern = '##', replacement = '') %>%
    strsplit(split = ',') %>%
    sapply(`[`, 2)# %>%
#as.numeric()

# monster levels (columns in the original tables)
monsterLevels <- strsplit(raw[2], '\t')[[1]]
xp <- c(10, 25, 50, 100, 200, 450, 700, 1100, 1800, 2300, 2900,
        3900, 5000, 5900, 7200, 8400, 10000, 11500, 13000, 15000,
        18000, 20000, 22000, 25000)

# collect the data here
dat <- matrix(nrow = 0, ncol = 6,
              dimnames = list(c(), c('difficulty', 'nPlayers', 'mLevel', 'pLevel', 'nMonsters', 'xpPmonster')))

# go through by player level (rows of the original tables)
for(i in 1:20)
{
    # pull all rows where player level == i
    if(i == 20)
    {
        modi <- 0
    }else{
        modi <- i + 2
    }

    tmp <- raw[1:length(raw) %% 22 == modi] %>%
        strsplit(split = '\t')

    # split columns (i.e. by monster level)
    for(j in 1:24)
    {
        dat <- rbind(dat,
                     cbind(difficulty, nPlayers, monsterLevels[j], i,
                           sapply(tmp, `[`, j + 1), xp[j]))

    }
}

# convert to a tidy table!
dat <- as_data_frame(dat) %>%
    mutate(nPlayers = as.numeric(nPlayers),
           mLevel = ifelse(mLevel == '1/8', 1/8,
                           ifelse(mLevel == '1/4', 1/4,
                                  ifelse(mLevel == '1/2', 1/2, as.numeric(mLevel)))),
           pLevel = as.numeric(pLevel),
           xpPmonster = as.numeric(xpPmonster),
           twodp = 1:dim(dat)[1] %in% grep('-', nMonsters, fixed = TRUE)) %>% # these ranges have two data points
    filter(nMonsters != '' & !is.na(nMonsters))

# split up ranged values
lower <- filter(dat, twodp) %>%
    mutate(nMonsters = {strsplit(nMonsters, split = '-', fixed = TRUE) %>%
            sapply(`[`, 1)})
upper <- filter(dat, twodp) %>%
    mutate(nMonsters = {strsplit(nMonsters, split = '-', fixed = TRUE) %>%
            sapply(`[`, 2)})

# put these all back together
dat <- filter(dat, !twodp) %>%
    rbind(lower, upper) %>%
    mutate(nMonsters = as.numeric(nMonsters)) %>%
    select(-twodp)

with(dat, plot(jitter(pLevel), log(xpPmonster*nMonsters)))

# try making response log(xpPmonster*nMonsters),
# that is: total summed XP of all monsters
dat <- mutate(dat,
              lTxp = log(xpPmonster * nMonsters),
              difficulty = ifelse(difficulty == 'medium', '0medium', difficulty)) # center at medium

model0 <- lm(lTxp ~ difficulty + nPlayers*pLevel, data = dat)
model1 <- lm(lTxp ~ difficulty + nPlayers + nPlayers:pLevel, data = dat)
