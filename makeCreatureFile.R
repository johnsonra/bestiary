# makeCreatureFile.R
# Read in data from ../_posts and covert to an RData file
# Randy Johnson

library(tidyverse)

# get files in _posts
files <- system('ls _posts', intern = TRUE)
dat <- paste0('_posts/', files) %>%
       map(readLines)

# function to extract names
get.name <- function(d)
{
    grep('title', d, value = TRUE) %>%
        strsplit(split = '"') %>%
        sapply(`[`, 2)
}

# (lawful, chaotic, neutral)
get.attitude <- function(d)
{
    retval <- grep("**", d, value = TRUE, fixed = TRUE)[[1]] %>%
              strsplit(split = ", ") %>%
              unlist()

    # a few have extra commas
    retval <- retval[length(retval)] %>%
              gsub(pattern = '**', replacement = '', fixed = TRUE)

    if(retval %in% c('any alignment', 'any evil', 'any non-good alignment'))
        return('any')

    if(retval %in% c('any non-lawful alignment'))
        return('non-lawful')

    if(retval %in% c('any chaotic alignment'))
        return('chaotic')

    if(retval == 'construct')
        return('')

    return({strsplit(retval, ' ') %>%
            sapply(`[`, 1)})
}

# (good, evil, neutral)
get.morality <- function(d)
{
    retval <- grep("**", d, value = TRUE, fixed = TRUE)[[1]] %>%
        strsplit(split = ", ") %>%
        unlist()

    # a few have extra commas
    retval <- retval[length(retval)] %>%
        gsub(pattern = '**', replacement = '', fixed = TRUE)

    if(retval %in% c('any alignment', 'any non-lawful alignment', 'any chaotic alignment'))
        return('any')

    if(retval %in% c('any evil'))
        return('evil')

    if(retval %in% c('any non-good alignment'))
        return('non-good')

    if(retval %in% c('neutral', 'unaligned'))
        return('neutral')

    if(retval == 'construct')
        return('')

    return({strsplit(retval, ' ') %>%
            sapply(`[`, 2)})
}

# (Gargantuan, Huge, Large, Medium, Small, Tiny)
get.size <- function(d)
{
    retval <- grep("**", d, value = TRUE, fixed = TRUE)[[1]] %>%
        strsplit(split = ", ") %>%
        unlist()

    # a few have extra commas
    retval <- retval[-length(retval)] %>%
        paste(collapse = ', ') %>%
        gsub(pattern = '**', replacement = '', fixed = TRUE) %>%
        strsplit(split = ' ') %>%
        sapply(`[`, 1)
}

# (aberration, beast, celestial, construct, dragon, elemental, fey, fiend, giant, humanoid,
#  monstrosity, ooze, plant, swarm, undead)
get.type <- function(d)
{
    retval <- grep("**", d, value = TRUE, fixed = TRUE)[[1]] %>%
        strsplit(split = ", ") %>%
        unlist()

    # a few have extra commas
    retval <- retval[-length(retval)] %>%
        paste(collapse = ', ') %>%
        gsub(pattern = '**', replacement = '', fixed = TRUE) %>%
        strsplit(split = ' ') %>%
        sapply(`[`, 2)
}

# tags
get.tags <- function(d)
{
    grep("tags", d, value = TRUE)[[1]] %>%
        strsplit(split = '[', fixed = TRUE) %>%
        sapply(`[`, 2) %>%
        gsub(pattern = ']', replacement = '', fixed = TRUE) %>%
        strsplit(split = ', ') %>%
        unlist()
}

# Armor Class
get.ac <- function(d)
{
    grep("**Armor Class**", d, value = TRUE, fixed = TRUE)[[1]] %>%
        strsplit(split = '** ', fixed = TRUE) %>%
        sapply(`[`, 2) %>%
        strsplit(split = ' ', fixed = TRUE) %>%
        sapply(`[`, 1) %>%
        as.numeric()
}

# summary Hit Points
get.hp.summary <- function(d)
{
    retval <- grep("**Hit Points**", d, value = TRUE, fixed = TRUE)[[1]] %>%
        gsub(pattern = "**Hit Points** ", replacement = "", fixed = TRUE)

    if(retval != "0")
        return(retval)

    retval <- grep("***Hit Points.***", d, value = TRUE, fixed = TRUE)[[1]] %>%
        gsub(pattern = "***Hit Points.*** ", replacement = "", fixed = TRUE)

    return(retval)
}

# mean Hit Points
get.mean.hp <- function(d)
{
    grep("**Hit Points**", d, value = TRUE, fixed = TRUE)[[1]] %>%
        gsub(pattern = "**Hit Points** ", replacement = "", fixed = TRUE) %>%
        strsplit(split = ' ', fixed = TRUE) %>%
        sapply(`[`, 1) %>%
        as.numeric()
}

# Hit Point num dice
get.n.hp.dice <- function(d)
{
    retval <- grep("**Hit Points**", d, value = TRUE, fixed = TRUE)[[1]] %>%
        gsub(pattern = "**Hit Points** ", replacement = "", fixed = TRUE)

    if(retval == '0')
        return(0)

    retval <- strsplit(retval, split = ' ', fixed = TRUE) %>%
        sapply(`[`, 2) %>%
        strsplit(split = 'd') %>%
        sapply(`[`, 1) %>%
        gsub(pattern = '(', replacement = '', fixed = TRUE) %>%
        as.numeric()

    return(retval)
}

# Hit Point die (4, 6, 8, 10, 12, 20, 100)
get.hp.die <- function(d)
{
    retval <- grep("**Hit Points**", d, value = TRUE, fixed = TRUE)[[1]] %>%
        gsub(pattern = "**Hit Points** ", replacement = "", fixed = TRUE)

    if(retval == '0')
        return(0)

    retval <- strsplit(retval, split = ' ', fixed = TRUE) %>%
        sapply(`[`, 2) %>%
        strsplit(split = 'd') %>%
        sapply(`[`, 2) %>%
        strsplit(split = '+', fixed = TRUE) %>%
        sapply(`[`, 1) %>%
        strsplit(split = '-', fixed = TRUE) %>%
        sapply(`[`, 1) %>%
        gsub(pattern = ')', replacement = '', fixed = TRUE) %>%
        as.numeric()

    return(retval)
}

# Hit Point modifier
get.hp.mod <- function(d)
{
    retval <- grep("**Hit Points**", d, value = TRUE, fixed = TRUE)[[1]] %>%
        gsub(pattern = "**Hit Points** ", replacement = "", fixed = TRUE) %>%
        strsplit(split = '+', fixed = TRUE) %>%
        sapply(`[`, 2) %>%
        gsub(pattern = ')', replacement = '', fixed = TRUE)

    if(is.na(retval))
        return(0)

    return(as.numeric(retval))
}

# speed
get.speed <- function(d)
{
    retval <- grep("**Speed**", d, value = TRUE, fixed = TRUE)[[1]] %>%
        strsplit(split = ' ', fixed = TRUE) %>%
        sapply(`[`, 2)

    if(retval %in% c('swim', 'fly'))
        return({grep("**Speed**", d, value = TRUE, fixed = TRUE)[[1]] %>%
                   strsplit(split = ' ', fixed = TRUE) %>%
                   sapply(`[`, 3) %>%
                   as.numeric()})

    # inconsistent formatting of Elder Brain
    if(retval == '5ft.,')
        return(5)

    return(as.numeric(retval))
}

# Get modifier for the mod attribute
get.mods <- function(d, mod)
{
    column <- grep("|", d, value = TRUE, fixed = TRUE)[[1]] %>%
              gsub(pattern = ' ', replacement = '', fixed = TRUE) %>%
              strsplit(split = '|', fixed = TRUE) %>%
              unlist()
    column <- which(column == mod)

    grep("|", d, value = TRUE, fixed = TRUE)[[3]] %>%
        strsplit(split = '(', fixed = TRUE) %>%
        sapply(`[`, column) %>%
        strsplit(split = ')', fixed = TRUE) %>%
        sapply(`[`, 1) %>%
        gsub(pattern = '−', replacement = '-') %>%
        as.numeric()
}

# Challenge Rating
get.cr <- function(d)
{
    retval <- grep('**Challenge**', d, value = TRUE, fixed = TRUE)[[1]] %>%
              gsub(pattern = '**Challenge** ', replacement = '', fixed = TRUE) %>%
              strsplit(split = ' ', fixed = TRUE) %>%
              sapply(`[`, 1)

    if(retval == '1/8')
        return(0.125)

    if(retval == '1/4')
        return(0.25)

    if(retval == '1/2')
        return(0.5)

    if(retval == 'l')
        return(-1)

    return(as.numeric(retval))
}

# XP
get.xp <- function(d)
{
    grep('**Challenge**', d, value = TRUE, fixed = TRUE)[[1]] %>%
        strsplit(split = '(', fixed = TRUE) %>%
        sapply(`[`, 2) %>%
        gsub(pattern = ' XP)', replacement = '', fixed = TRUE) %>%
        gsub(pattern = ',', replacement = '', fixed = TRUE) %>%
        as.numeric()
}

# actions
get.actions <- function(d)
{
    startHere <- grep('**Actions**', d, fixed = TRUE)

    # if no attacks, return NULL
    if(length(startHere) == 0)
        return(NULL)

    if(d[3] == 'title: "Draft Horse"') # this one is not formatted consistently. :P
    {
        d[startHere] <- gsub('**Actions** Hooves.', '***Hooves***', d[startHere], fixed = TRUE)
    }

    action <- 1 # increments with each new action

    retval <- list()

    for(i in startHere:length(d))
    {
        # skip lines if they are blank or start with **Actions**, ...
        if(d[i] == '' |
           substr(d[i], 1, 11) == '**Actions**' |
           substr(d[i], 1, 21) == '**Legendary Actions**')
        {
            next
        }

        # exit if we get to a new section
        if(substr(d[i], 1, 3) != '***')
            break

        # grab next action
        retval[[action]] <- strsplit(d[i], '***', fixed = TRUE)[[1]][3] %>%
                            gsub(pattern = '^ ', replacement = '') %>% # remove leading spaces
                            gsub(pattern = ' $', replacement = '') # remove trailing spaces

        # get name
        tmp <- strsplit(d[i], '***', fixed = TRUE)[[1]][2]

        # # some have caveats/limitations in title
        # if(length(grep("(", tmp, fixed = TRUE)) > 0)
        # {
        #     tmp <- strsplit(tmp, "")[[1]]
        #
        #     # move caveats/limitations to description
        #     retval[[action]] <- paste(paste(tmp[grep("(", tmp, fixed = TRUE):
        #                                         grep(")", tmp, fixed = TRUE)], collapse = ''),
        #                          retval[[action]])
        #
        #     # keep title, minus the caveats/limitations, and traling space
        #     tmp <- paste(tmp[1:(grep("(", tmp, fixed = TRUE) - 2)], collapse = '')
        # }

        names(retval)[action] <- gsub('.', '', tmp, fixed = TRUE) # remove trailing '.'

        # increment action number
        action <- action + 1
    }

    return(retval)
}

# description
get.description <- function(d)
{
    # these rows are already captured
    captured <- c(1:18,
                  grep('**Challenge**', d, fixed = TRUE))

    d[-captured] %>%
        paste(collapse = '\n')
}

creatures <- data_frame(name = sapply(dat, get.name),
                        attitude = sapply(dat, get.attitude),
                        morality = sapply(dat, get.morality),
                        size = sapply(dat, get.size),
                        type = sapply(dat, get.type),
                        tags = lapply(dat, get.tags),
                        armor.class = sapply(dat, get.ac),
                        HPsummary = sapply(dat, get.hp.summary),
                        HPmean = sapply(dat, get.mean.hp),
                        HPnDice = sapply(dat, get.n.hp.dice),
                        HPdie = sapply(dat, get.hp.die),
                        HPmod = sapply(dat, get.hp.mod),
                        speed = sapply(dat, get.speed),
                        STRmod = sapply(dat, get.mods, mod = 'STR'),
                        DEXmod = sapply(dat, get.mods, mod = 'DEX'),
                        CONmod = sapply(dat, get.mods, mod = 'CON'),
                        INTmod = sapply(dat, get.mods, mod = 'INT'),
                        WISmod = sapply(dat, get.mods, mod = 'WIS'),
                        CHAmod = sapply(dat, get.mods, mod = 'CHA'),
                        challenge = sapply(dat, get.cr),
                        xp = sapply(dat, get.xp),
                        actions = lapply(dat, get.actions))
# going to be missing some details here...
#                        description = sapply(dat, get.description))

save(creatures, file = '../bestiary/data/creatures.RData')
