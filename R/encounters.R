# rencounter.R
# create a random encounter

# n = number of creatures to generate
# cname = name of creature type
rcreatures <- function(n, cname)
{
    data(creatures)
    csub <- filter(creatures, name == cname)

    retval <- list()
    for(i in 1:n)
    {
        retval[[i]] <- list(name = paste(cname, i),
                            HP = with(csub, sum(sample(1:HPdie, size = HPnDice, replace = TRUE)) +
                                            HPmod),
                            damage = 0,
                            AC = csub$armor.class,
                            minACthresh = 5,
                            maxACthresh = 25,
                            speed = csub$speed,
                            initiative = sample(1:20, size = 1) + csub$DEXmod,
                            xp = csub$xp,
                            STRmod = csub$STRmod,
                            DEXmod = csub$DEXmod,
                            CONmod = csub$CONmod,
                            INTmod = csub$INTmod,
                            WISmod = csub$WISmod,
                            CHAmod = csub$CHAmod,
                            details = csub$description)
        class(retval[[i]]) <- 'creature'
    }

    names(retval) <- paste(cname, 1:n)
    class(retval) <- 'creatureList'

    return(retval)
}

rencounter <- function(n, ...)
{
    args <- list(...)

    if(length(args) == 0)
    {
        print("Optional filters: challenge, attitude, morality, size")
        return(NULL)
    }

    data(creatures)
    csub <- creatures

    if('challenge' %in% names(args))
    {
        if(args$challenge %in% csub$challenge)
        {
            csub <- filter(csub, challenge %in% args$challenge)
        }else{
            warning("Optional challenge ratings:", unique(csub$challenge))
        }
    }

    if('attitude' %in% names(args))
    {
        if(args$attitude == 'chaotic')
            csub <- filter(csub, attitude %in% c('chaotic', 'any'))

        if(args$attitude == 'neutral')
            csub <- filter(csub, attitude %in% c('neutral', 'any'))

        if(args$attitude == 'lawful')
            csub <- filter(csub, attitude %in% c('lawful', 'any'))

        if(args$attitude == 'non-lawful')
            csub <- filter(csub, attitude %in% c('chaotic', 'neutral', 'any'))

        if(args$attitude == 'non-chaotic')
            csub <- filter(csub, attitude %in% c('lawful', 'neutral', 'any'))
    }

    if('morality' %in% names(args))
    {
        if(args$morality == 'non-good')
            csub <- filter(csub, morality %in% c('evil', 'neutral', 'non-good', 'any'))

        if(args$morality == 'non-evil')
            csub <- filter(csub, morality %in% c('good', 'neutral', 'non-evil', 'any'))

        if(args$morality == 'good')
            csub <- filter(csub, morality %in% c('good', 'non-evil', 'any'))

        if(args$morality == 'neutral')
            csub <- filter(csub, morality %in% c('neutral', 'non-good', 'non-evil', 'any'))

        if(args$morality == 'evil')
            csub <- filter(csub, morality %in% c('evil', 'non-good', 'any'))
    }

    if('size' %in% names(args))
    {
        if(args$size %in% csub$size)
        {
            csub <- fulter(csub, size %in% args$size)
        }else{
            warning("Optional sizes:", unique(csub$size))
        }
    }

    species <- sample(csub$name, size = 1)

    return(rcreatures(n, species))
}
