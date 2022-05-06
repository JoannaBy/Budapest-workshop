


# Using `stylo` in batch mode: Three simple pieces



## What is a distance between two given texts, really?

In this script, we'll use the dataset `lee` that is provided by the package `stylo`. It is a matrix with word frequencies of 3,000 MFWs for 28 American novels. Type `help(lee)` to see what's inside.

The aim is to perform 100 iterations, and in each iteration to estimate the distance between two texts from the `lee` dataset. As an example, we'll use the 1st and the 2nd text, e.g. _In Cold Blood_ by Truman Capote, and _Breakfast at Tiffany's_ by the same author.


``` R
library(stylo)
data(lee)
final.results = c()

for(iteration in 1:100) {

    # randomly pick 100 numbers from the range 1:3000
    pick.features = sample(1:3000, 100, replace = FALSE)
    # select the features matching the 100 random numbers
    dataset = lee[ , pick.features]
    # perform the unsupervised classification, using Classic Delta
    results = stylo(frequencies = dataset, gui = FALSE, display.on.screen = FALSE)
    # pick the distance stored in the 1st row and 2nd column,
    # e.g. the one between two different texts by Capote
    final.distance = results$distance.table[1,2]
    # add the new results to the already existing vector
    final.results = c(final.results, final.distance)

}

# basic statistics
mean(final.results)
sd(final.results)

# plottting the distribution
plot(density(final.results))
```



## The distribution of distances in a corpus

Now, what if we generalize the above code to cover all the texts in the corpus? 


``` R
library(stylo)
data(lee)
final.results = c()

for(iteration in 1:100) {

    pick.features = sample(1:3000, 100)
    dataset = lee[ , pick.features]
    results = stylo(frequencies = dataset, gui = FALSE, display.on.screen = FALSE)
    final.distance = as.vector(results$distance.table)
    final.results = c(final.results, final.distance)

}

# since the values contain self-similarity tests as well, here's a dirty trick to get rid of 0 values from the results:
final.results = final.results[final.results > 0]

# plottting the distribution
plot(density(final.results))
```




``` R
library(stylo)
data(lee)

####### adjust the parameters #############

# pick a distance: "delta", "wurzburg", ...
preferred.distance = "delta" 

# choose a number of randomly picked features
random.features = 100

###########################################


final.results.same = c()
final.results.different = c()

for(iteration in 1:100) {

    pick.features = sample(1:3000, random.features)
    dataset = lee[ , pick.features]
    results = stylo(frequencies = dataset, gui = FALSE, display.on.screen = FALSE, mfw.min = 3000, mfw.max = 3000, distance.measure = preferred.distance)

    final.distance = results$distance.table

    classes = gsub("_.*", "", rownames(final.distance))

    for(n in 1:length(classes) ) {

        same = classes[n] == classes
        different = classes[n] != classes
        same.distance = final.distance[n, same]
        different.distance = final.distance[n, different]

    }

    final.results.same = c(final.results.same, same.distance)
    final.results.same = final.results.same[final.results.same > 0]
    final.results.different = c(final.results.different, different.distance)

}

# estimating the density funcion
density.same = density(final.results.same)
density.different = density(final.results.different)

# plotting the outgroup and the ingroup distances
fancy.green = rgb(0, 1, 0, 0.3)
fancy.red = rgb(1, 0, 0, 0.3)
max.y.value = max(c(density.same$y, density.different$y))
max.x.value = max(c(density.same$x, density.different$x))
plot(NULL, ylim = c(0, max.y.value), xlim = c(0, max.x.value), ylab = "density", xlab = "distance")
polygon(density.same, col = fancy.green)
polygon(density.different, col = fancy.red)
legend("topleft", legend = c("ingroup distances", "outgroup distances"), lty = 1, lwd = 10, col = c(fancy.green, fancy.red), bty = "n")
```






