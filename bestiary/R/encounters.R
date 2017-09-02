# rencounter.R
# create a random encounter

# n = number of creatures to generate
# cname = name of creature type
rencounter <- function(n, cname)
{
    data(creatures)
    csub <- filter(creatures, name == cname)

    retval <- list()
    for(i in 1:n)
    {
        retval[[i]] <- list(name = cname,
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
                            details = csub$description,
                            class = 'creature')
    }

    return(retval)
}
