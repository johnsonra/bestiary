# interactions.R
# Interactions with 5e_creatures

# x = list of creatures
# ar = attack roll
# dam = damage (negative number adds back HP)
# dc = difficulty class
# mod = saving throw modifier
# n = creature number(s)
# ac = armor class

# check to see if an atack on a creature in x is effective
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

    # update all armor classes for monsters of like species
    for(i in 1:length(x))
    {
        if(i == n) # already did this one
            next

        if(x[[i]]$species == x[[n]]$species)
        {
            x[[i]]$maxACthresh <- x[[n]]$maxACthresh
            x[[i]]$minACthresh <- x[[n]]$minACthresh
        }
    }

    invisible(x)
}

# deal damage to a creature in x
damage <- function(x, dam, n)
{
    x[[n]]$damage <- x[[n]]$damage + dam

    return(x)
}

# perform a saving throw for a creature in x
savingThrow <- function(x, dc, dcMod, n = 1)
{
    # make the throw
    throw <- sample(1:20, 1)

    # add the modifier
    if(is.numeric(dcMod)) # if numeric, just add it
    {
        outcome <- throw + dcMod
    }else{ # if not numeric, assume it is character
        outcome <- throw + x[[n]][[dcMod]]
    }

    if(outcome >= dc)
    {
        print(paste0(outcome, ": Saving throw succeeded"))
    }else{
        print(paste0(outcome, ": Saving throw failed"))
    }
}

# simulate an attack on a character from a mob of creatures
# n = number of creatures who are attacking
# ac = armor class of character being attacked
# dcMod = modifier to attach die
# dam = function to calculate damage by one creature if successful
#       (or simply the amount of damage dealt per monster)
mobAttack <- function(n, ac, dcMod, dam)
{
    # roll for attack
    attackDice <- sample(1:20, size = n, replace = TRUE) + dcMod

    # if the attack was successful, calculate damage
    successes <- sum(attackDice > ac)

    if(successes == 0)
        return(0)

    return(sum(unlist(replicate(successes, dam))))
}
