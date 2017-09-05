# print.5e_creature()

print.creature <- function(x, ...)
{
    out <- list()

    for(i in 1:length(x))
    {
        # give range of armor class based on interaction up to this point
        if(x[[i]]$minACthresh == x[[i]]$maxACthresh)
        {
            ac <- x[[i]]$AC
        }else{
            ac <- paste0('(', x[[i]]$minACthresh, ', ', x[[i]]$maxACthresh, ')')
        }

        # graphic representation of health
        health = paste0('|',
                 paste0(rep('*', (1 - x[[i]]$damage / x[[i]]$HP) * 10), collapse = ''),
                 paste0(rep('-', 10), collapse = '')) %>%
            substr(start = 1, stop = 11) %>%
            paste0('|')

        out[[i]] <- paste('\n', x[[i]]$name, i, '\n',
                          'AC:', ac, '\n',
                          'Initiative:', x[[i]]$initiative, '\n',
                          'Health:', health, '\n')
    }

    cat(unlist(out), sep = '\n')
}
