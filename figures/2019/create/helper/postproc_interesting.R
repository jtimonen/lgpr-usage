require(lgpr)

# takes quite long to run

# 513
load('data/main2/prot/basic/res_513') # loads 'results'
basic_513 <- postproc(results$fit)

# 662
load('data/main2/prot/basic/res_662') # loads 'results'
basic_662 <- postproc(results$fit)

# 909
load('data/main2/prot/basic/res_909') # loads 'results'
basic_909 <- postproc(results$fit)

# 450
load('data/main2/prot/heter/res_450') # loads 'results'
heter_450 <- postproc(results$fit)
load('data/main2/prot/basic/res_450') # loads 'results'
basic_450 <- postproc(results$fit)

# 973
load('data/main2/prot/heter/res_973') # loads 'results'
heter_973 <- postproc(results$fit)
load('data/main2/prot/basic/res_973') # loads 'results'
basic_973 <- postproc(results$fit)

# 1343
load('data/main2/prot/heter/res_1343') # loads 'results'
heter_1343 <- postproc(results$fit)
load('data/main2/prot/basic/res_1343') # loads 'results'
basic_1343 <- postproc(results$fit)

# 1431
load('data/main2/prot/heter/res_1431') # loads 'results'
heter_1431 <- postproc(results$fit)
load('data/main2/prot/basic/res_1431') # loads 'results'
basic_1431 <- postproc(results$fit)

all <- list(basic_513 = basic_513,
            basic_662 = basic_662,
            basic_909 = basic_909,
            basic_450 = basic_450,
            heter_450 = heter_450,
            basic_973 = basic_973,
            heter_973 = heter_973,
            basic_1343 = basic_1343,
            heter_1343 = heter_1343,
            basic_1431 = basic_1431,
            heter_1431 = heter_1431)

saveRDS(all, file = 'data/main2/prot/interesting.rds')
