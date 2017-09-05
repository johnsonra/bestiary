##### print methods


print.creatureList <- function(x, ...)
{
    invisible(lapply(x, print))
}


print.creature <- function(x, ...)
{
    # give range of armor class based on interaction up to this point
    if(x$minACthresh == x$maxACthresh)
    {
        ac <- x$AC
    }else{
        ac <- paste0('(', x$minACthresh, ', ', x$maxACthresh, ')')
    }

    # graphic representation of health
    if(x$damage < x$HP)
    {
        health = paste0('|',
                 paste0(rep('*', (1 - x$damage / x$HP) * 10), collapse = ''),
                 paste0(rep('-', 10), collapse = '')) %>%
            substr(start = 1, stop = 11) %>%
            paste0('|')

        if(health == '----------')
            health <- 'Unconcious'
    }else{
        health = "0"
    }

    cat(' AC:', ac, '\n',
        'Initiative:', x$initiative, '\n',
        'Health:', health, '\n')

    invisible(1)
}


##### addition methods

`+.creatureList` <- function(e1, e2)
{
    append(e1, e2)
}
