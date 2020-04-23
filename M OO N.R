
# Moon

library(tidyverse); library(patchwork)

Moons = c(0, 0, 1, 2, 67, 62, 27, 14, 5)

Lambda = mean(Moons); print(Lambda)

dpois(1, Lambda)

Size = (Lambda^2)/(var(Moons) - Lambda)

dnbinom(1, size = Size, mu = Lambda)

qplot(Moons)

# From http://umich.edu/~lowbrows/reflections/2008/jmaguran.1.html

MoonSizes <- read.csv("C:/Users/gfalb/Documents/MoonSizes.csv")

MoonSizes %>%
  mutate_at(3:ncol(MoonSizes), ~.x %>% str_remove_all(",|[%]|<") %>% as.numeric) %>%
  mutate(Luna = factor(as.numeric(Moon == "Moon"), levels = c(0, 1))) ->
  MoonSizes

qplot(MoonSizes$Distance_km, geom = "density")

qplot(MoonSizes$Diameter_km, geom = "density")

MoonSizes %>% ggplot(aes(Distance_km, ApparentSize)) + geom_point() + scale_x_log10() + geom_smooth()
MoonSizes %>% ggplot(aes(Diameter_km, ApparentSize)) + geom_point() + scale_x_log10() + geom_smooth()

MoonSizes %>% ggplot(aes(Distance_km, ApparentSize)) +
  geom_point(aes(colour = Luna, size = Diameter_km)) +
  scale_x_log10() + geom_smooth(fill = NA, colour = "black") +
  lims(y = c(0, 32)) +
  scale_colour_manual(values = c("grey", "dark blue")) +
  ggsave("ApparentMoonSizes.jpeg")


qplot(MoonSizes$ApparentSize)

MoonSizes %>% mutate(ApparentSize2 = Diameter_km/Distance_km) %>%
  ggplot(aes(ApparentSize2, ApparentSize))

# Draws ####

1/length(MoonSizes$ApparentSize) # Uniform
(1/length(MoonSizes$ApparentSize))*(0.072) # Uniform * 1 moon probability

qplot(MoonSizes$Distance_km, geom = "density")

DiameterLambda = mean(MoonSizes$Diameter_km); print(DiameterLambda)

DiameterSize = (DiameterLambda^2)/(var(MoonSizes$Diameter_km) - DiameterLambda)

rnbinom(100000, size = DiameterSize, mu = DiameterLambda) %>% qplot

DistanceLambda = mean(MoonSizes$Distance_km); print(DistanceLambda)

DistanceSize = (DistanceLambda^2)/(var(MoonSizes$Distance_km) - DistanceLambda)

rnbinom(100000, size = DistanceSize, mu = DistanceLambda) %>% qplot

NewMoons <- data.frame(

  NewDiameters <- rnbinom(100000, size = DiameterSize, mu = DiameterLambda),
  NewDistances <- rnbinom(100000, size = DistanceSize, mu = DistanceLambda)

) %>%
  mutate(ApparentSize = NewDiameters/NewDistances) -> NewMoons

NewMoons %>%
  ggplot(aes(NewDiameters, NewDistances)) +
  labs(x = "New diameters (km)", y = "New distances (km)") +
  geom_point() -> Plot1

NewMoons %>%
  ggplot(aes(ApparentSize)) + geom_histogram() +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 27, xmax = 33),
            fill = "red") + scale_x_log10() +
  geom_label(data = data.frame(x = 1, y = 1),
             inherit.aes = F, aes(x = 30, y = 9000, label = "'Moon Zone'"), colour = "red") +
  labs(x = "Apparent Size") -> Plot2

NewMoons$ApparentSize %>% between(27, 33) %>% table()
NewMoons$ApparentSize %>% between(27, 33) %>% ggregplot::Prev()

(Plot1|Plot2) + ggsave("PredictedSizes.jpeg", width = 250, height = 100, units = "mm")
