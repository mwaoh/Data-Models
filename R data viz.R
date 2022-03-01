ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +  geom_point()
install.packages("ggplot2")

install.packages("palmerpenguins")
View(penguins)
data(penguins)

ggplot(data = penguins) + geom_col(mapping = aes(x = flipper_length_mm, y = body_mass_g))
gglot(data=penguins)

 library(ggplot2)
library(palmerpenguins)

ggplot(data=penguins)+
  geom_smooth(mapping = aes(x=flipper_length_mm,y=body_mass_g))+
  geom_jitter(mapping = aes(x=flipper_length_mm,y=body_mass_g))

ggplot(data=diamonds)+
  geom_bar(mapping = aes(x=cut,fill=clarity))

library(ggplot2)
library(palmerpenguins)

ggplot(data=penguins)+
  geom_point(mapping = aes( x=flipper_length_mm, y=body_mass_g ,shape=species))

