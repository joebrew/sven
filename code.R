
# Define function for mapping out the segments of the diamond
diamondCI <- function(x, y, ul, ll, w = ul-ll, ...){
  hw <- w/2
  segments(x-hw, y, x+hw, y, ...)  # horizontal bar
  segments(x-hw, y, x, ul, ...) # left upper diag
  segments(x, ul, x+hw, y, ...) # right upper diag
  segments(x-hw, y, x, ll, ...) # left lower diag
  segments(x, ll, x+hw, y, ...) # right lwoer diag
}

# Define data to be plotted
# just guessing values based on plot sent in email
# REPLACE WITH CORRECT DATA
df <- data.frame(
  name = c('Family planning', 'Antenatal care', 'Delivery care'),
  y = c(2, 7.5, 2),
  q75 = c(5, 13, 3),
  q25 = c(-1, 1, 1),
  upr = c(11, 17, 5),
  lwr = c(-7, -5, -1)
)

# Make polygons
# (this is a bit hacky, but gets the job done and will adjust automatically to changes in the values above)
poly_list <- liney_list <- list()
for(i in 1:nrow(df)){
  coords <- tibble(x = c(i, i-0.2, i, i+0.2),
                   y = c(df$q25[i],
                         df$y[i],
                         df$q75[i],
                         df$y[i]))
  liney <- tibble(x = c(i, i),
                  y = c(df$lwr[i], df$upr[i]))
  coords$name <- df$name[i]
  liney$name <- df$name[i]
  poly_list[[i]] <- coords
  liney_list[[i]] <- liney
}
polys <- bind_rows(poly_list)
liney <- bind_rows(liney_list)

# Make the plot
library(ggplot2)
ggplot() +
  geom_point(data = polys,
             aes(x = name,
                 y = y),
             color = NA) +
  geom_polygon(data = polys,
               aes(x = x,
                   y = y,
                   group = name),
               fill = 'blue') +
  geom_line(data = liney,
            aes(x = x,
                y = y,
                group = name),
            color = 'blue') +
  theme_bw() +
  labs(x = '',
       y = 'Percentage points') +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(-5, 0, 5)) +
  theme(axis.text.x = element_text(size = 15))
