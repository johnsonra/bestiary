
library(bestiary)

# check difficulty of encounters (should be in the range of 0.25 - 1)
difficultyCheck('deadly', rep(4,3), c(rep(700, 3), rep(10, 30)))

# create different encounters
(dat <- rcreatures(30, 'Raven'))
(dop <- rcreatures(2, 'Doppelganger'))
(dat <- rcreatures(1, 'Bone Naga (Guardian)'))

difficultyCheck(difficulty = 'medium',
                playerLevels = rep(4,3),
                monsterXPlevels = unlist(lapply(dat, `[`, 'xp')))

(dat <- rencounter(2, challenge = 2))

(dat <- attack(dat, ar = 15, n = 1))


# simulate getting attacked by a mob
mobAttack(n = 10, ac = 17, dcMod = 4, dam = 1)


# deal damage to a creature
(dat <- damage(dat, dam = 19, n = 1))


# creature saving throw
savingThrow(dat, dc = 13, dcMod = 'CONmod', n = 1)


# treasure -- see http://donjon.bin.sh/5e/random/#type=treasure;cr=0;loot_type=Individual%20Treasure

# who to randomly attack?
ndspm(n = 1, sides = 3)

# 3d6 roll
ndspm(n = 3, sides = 6, mod = 0)
