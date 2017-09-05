# interactions.R
# Interactions with 5e_creatures

# x = list of creatures
# ar = attack roll
# dam = damage (negative number adds back HP)
# dc = difficulty class
# mod = saving throw modifier
# n = creature number

attack <- function(x, ar, n = 1)
{
    outcome <- ar > x[[n]]$AC

    if(outcome)
    {
        print("Attack succeeded")
    }else{
        print("Attack failed")
    }

    # if we succeeded, we know that AC < ar
    if(outcome & ar < x[[n]]$maxACthresh)
        x[[n]]$maxACthresh <- ar - 1

    # if we failed, we know that AC >= ar
    if(!outcome & ar > x[[n]]$minACthresh)
        x[[n]]$minACthresh <- ar

    invisible(x)
}

damage <- function(x, dam, n)
{
    x[[n]]$damage <- x[[n]]$damage + dam

    return(x)
}

savingThrow <- function(x, dc, dcMod, n = 1)
{
    # make the throw
    throw <- sample(1:20, 1)

    # add the modifier
    if(is.numeric(dcMod)) # if numeric, just add it
    {
        outcome <- throw + dcMod >= dc
    }else{ # if not numeric, assume it is character
        outcome <- throw + x[[n]][[dcMod]] >= dc
    }

    if(outcome)
    {
        print("Saving throw succeeded")
    }else{
        print("Saving throw failed")
    }
}
