library("parallel"); options(mc.cores = detectCores())
library("tidyverse"); theme_set(theme_minimal())
library("gganimate"); options(gganimate.dev_args = list(width = 1500, height = 800))
library("algstat")

## 
################################################################################

p <- mp("(x^2 + y^2)^2 - 2 (x^2 - y^2)")
(samps <- rvnorm(200, p, .01, "tibble", chains = 8, refresh = 100))

ggplot(samps, aes(x, y)) + geom_point(size = .5) + coord_equal()
samps %>% 
  select(x, y) %>% 
  write_csv("lemniscate.csv")




## lemniscate
################################################################################

p <- mp("(x^2 + y^2)^2 - 2 (x^2 - y^2)")
(samps <- rvnorm(1000, p, .01, "tibble", keep_warmup = TRUE, chains = 16, refresh = 100))

ggplot(samps, aes(x, y)) + geom_point(size = .5) + coord_equal()

ggplot(samps, aes(x, y, color = iter)) + 
  geom_point(size = .5) + geom_path(alpha = .2) +  
  coord_equal() + facet_wrap(~ factor(chain)) +
  theme_void()

plots <- ggplot(samps, aes(x, y)) + 
  geom_point(size = 1) + 
  geom_path(alpha = .25) +
  coord_equal() + facet_wrap(~ factor(chain), nrow = 4) +
  transition_reveal(iter) + theme_void(20)

animate(plots, renderer = gifski_renderer(), nframes = 100, fps = 20)
anim_save(here::here("infinity-animation.gif"))




## heart
################################################################################

p <- mp("(x^2 + y^2 - 1)^3 - x^2 y^3")
(samps <- rvnorm(1000, p, .01, "tibble", keep_warmup = TRUE, chains = 32, refresh = 100))

plots <- ggplot(samps, aes(x, y)) + 
  geom_point(size = 1) + 
  geom_path(alpha = .25) +
  coord_equal() + facet_wrap(~ factor(chain), nrow = 4) +
  transition_reveal(iter) + theme_void(20)

animate(plots, renderer = gifski_renderer(), nframes = 100, fps = 20)
anim_save(here::here("heart-animation.gif"))






## surface examples
########################################

library("plotly")


## 1d in 3d
#########################

# twisted cubic
p <- mp("(y - x^2)^2 + (z - x^3)^2")
(samps <- rvnorm(2000, p, .01, "tibble", keep_warmup = TRUE, w = 5))

plot_ly(
  samps, x = ~x, y = ~y, z = ~z, 
  type = "scatter3d", mode = "markers",
  marker = list(size = 2, color = "black")
)

plot_ly(
  samps, x = ~x, y = ~y, z = ~z, 
  type = "scatter3d", mode = "lines+markers",
  line = list(width = 1.5), marker = list(size = 3),
  split = ~factor(chain), opacity = .2
)




## 2d in 3d
#########################


# torus
p <- mp("(x^2 + y^2 + z^2 + 2^2 - 1^2)^2 - 4 2^2 (x^2 + y^2)")
(samps <- rvnorm(2000, p, .01, "tibble"))

plot_ly(
  samps, x = ~x, y = ~y, z = ~z, 
  type = "scatter3d", mode = "markers",
  marker = list(size = 1, color = "black")
)

# plot_ly(
#   samps, x = ~x, y = ~y, z = ~z, 
#   type = "scatter3d", mode = "lines+markers",
#   line = list(width = 1.5), marker = list(size = 3),
#   split = ~factor(chain), opacity = .2
# )




# torus semi-algebraic (filled in donut)
p <- mp("(x^2 + y^2 + z^2 + 2^2 - 1^2)^2 - 4 2^2 (x^2 + y^2) + s^2")
(samps <- rvnorm(2000, p, .01, "tibble", chains = 8, refresh = 100))

plot_ly(
  samps, x = ~x, y = ~y, z = ~z, 
  type = "scatter3d", mode = "markers",
  marker = list(size = 1, color = "black")
)

# plot_ly(
#   samps, x = ~x, y = ~y, z = ~z, 
#   type = "scatter3d", mode = "lines+markers",
#   line = list(width = 1.5), marker = list(size = 3),
#   split = ~factor(chain), opacity = .2
# )




# heart
p <- mp("(x^2 + 2.25 y^2 + z^2 - 1)^3 - x^2 z^3 - .1125 x^2 z^3")
samps <- rvnorm(2000, p, .01, "tibble")
plot_ly(
  samps, x = ~x, y = ~y, z = ~z, 
  type = "scatter3d", mode = "markers",
  marker = list(size = 1, color = "black")
)


# whitney
p <- mp("x^2 - y^2 z")
(samps <- rvnorm(2000, p, .05, "tibble", w = 5))

plot_ly(
  samps, x = ~x, y = ~y, z = ~z, 
  type = "scatter3d", mode = "markers",
  marker = list(size = 1, color = "black")
)



# geisha
p <- mp("x^2 y z + x^2 z^2 - y^3 z - y^3")
(samps <- rvnorm(5000, p, .01, "tibble", w = 5))

plot_ly(
  samps, x = ~x, y = ~y, z = ~z, 
  type = "scatter3d", mode = "markers",
  marker = list(size = 1, color = "black")
)



## TDA
################################################################################


library("TDA")
library("algstat")
library("tidyverse"); theme_set(theme_minimal())


## 
############################################################


# from vignette https://cran.r-project.org/web/packages/TDA/vignettes/article.pdf
Circle1 <- circleUnif(60)
Circle2 <- circleUnif(60, r = 2) + 3
Circles <- rbind(Circle1, Circle2)

maxscale <- 5        # limit of the filtration
maxdimension <- 1    # components and loops

DiagRips <- ripsDiag(X = Circles, maxdimension, maxscale,
  library = c("GUDHI", "Dionysus"), location = TRUE, printProgress = TRUE)

str(DiagRips)
plot(DiagRips[["diagram"]])

one <- which(
  DiagRips[["diagram"]][, 1] == 1 &
    DiagRips[["diagram"]][, 3] - DiagRips[["diagram"]][, 2] > 0.5)

DiagRips %>% 
  pluck("cycleLocation") %>% 
  .[one] %>% 
  imap(~ {
    from <- .x[,,1]
    colnames(from) <- c("x","xend")
    from <- as_tibble(from)
    
    to <- .x[,,2]
    colnames(to) <- c("y","yend")
    to <- as_tibble(to)
    
    mutate(
      bind_cols(from, to),
      ndx = .y
    )
  }) %>% 
  bind_rows() %>% 
  ggplot(aes(x, y)) +
  geom_point(aes(x1, x2), alpha = .5, data = as_tibble(Circles)) +
  geom_segment(aes(xend = xend, yend = yend, color = factor(ndx)), size = 1) +
  scale_color_discrete(guide = FALSE)





## 
############################################################

# from help ?ripsDiag
# X <- circleUnif(30)

set.seed(1)
X <- rvnorm(50L, mp("x^2 + y^2 - 1"), sd = .01)
plot(X)

maxscale <- 5
maxdimension <- 1
## note that the input X is a point cloud
DiagRips <- ripsDiag(
  X = X, maxdimension = maxdimension, maxscale = maxscale,
  library = "Dionysus", location = TRUE, printProgress = TRUE)

# plot
layout(matrix(c(1, 3, 2, 2), 2, 2))
plot(X, cex = 0.5, pch = 19)
title(main = "Data")
plot(DiagRips[["diagram"]])
title(main = "rips Diagram")
one <- which(
  DiagRips[["diagram"]][, 1] == 1 &
    DiagRips[["diagram"]][, 3] - DiagRips[["diagram"]][, 2] > 0.5)


DiagRips %>% 
  pluck("cycleLocation") %>% 
  .[one] %>% 
  imap(~ {
    from <- .x[,,1]
    colnames(from) <- c("x","xend")
    from <- as_tibble(from)
    
    to <- .x[,,2]
    colnames(to) <- c("y","yend")
    to <- as_tibble(to)
    
    mutate(
      bind_cols(from, to),
      ndx = .y
    )
  }) %>% 
  bind_rows() %>% 
  ggplot(aes(x, y)) +
  geom_point(aes(x, y), alpha = .75, data = as_tibble(X)) +
  geom_segment(aes(xend = xend, yend = yend, color = factor(ndx)), size = 1) +
  scale_color_discrete(guide = FALSE) +
  theme_void()



