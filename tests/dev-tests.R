# ============================================
# ggram() Test Cases
# ============================================

# Test Case 1: Basic usage with default style
# ============================================
clearhistory()

trees |>
  ggplot(aes(Height, Girth)) +
  geom_point() +
  geom_smooth() +
  labs(title = 'Girth ~ Smooth') + #
  theme_minimal()

ggram()


# Test Case 2: Using stamp_notebook() style
# ============================================
clearhistory()

trees |>
  ggplot(aes(Height, Girth)) +
  geom_point() +
  geom_smooth() +
  labs(title = 'Girth ~ Smooth') + #
  theme_minimal()

ggram(style = stamp_notebook())


# Test Case 3: Using stamp_code() default
# ============================================
clearhistory()

trees |>
  ggplot(aes(Height, Girth)) +
  geom_point() +
  geom_smooth() +
  labs(title = 'Girth ~ Smooth') + #<<
  theme_minimal()

ggram(style = stamp_code())


# Test Case 4: stamp_code() with theme argument
# ============================================
clearhistory()

trees |>
  ggplot(aes(Height, Girth)) +
  geom_point() +
  geom_smooth() +
  labs(title = 'Girth ~ Smooth') + #
  theme_minimal()

ggram(style = stamp_code(theme = "warm"))


# Test Case 5: Custom highlight colors (single line)
# ============================================
clearhistory()

trees |>
  ggplot(aes(Height, Girth)) +
  geom_point() + #<<
  geom_smooth() +
  labs(title = 'Girth ~ Smooth') + #<<
  theme_minimal()

ggram(highlight_colors = c("blue", "red"))


# Test Case 6: Multiple arguments (multi-line call)
# ============================================
clearhistory()

trees |>
  ggplot(aes(Height, Girth)) +
  geom_point() +
  geom_smooth() +
  labs(title = 'Girth ~ Smooth') + #<<
  theme_minimal()

ggram(
  title = "Custom Title",
  style = stamp_code(theme = 'dark'),
  highlight_colors = c("blue", "red")
)


# Test Case 7: Different plot - bar chart
# ============================================
clearhistory()

mtcars |>
  ggplot(aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  labs(title = "Cylinders Distribution") +
  theme_classic()

ggram(title = "one title",
      style = stamp_code(theme = "dark",title = "other title"))


# Test Case 8: Faceted plot with custom colors
# ============================================
clearhistory()

iris |>
  ggplot(aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(size = 3) +
  facet_wrap(~Species) + #<<
  theme_bw()

ggram(
  title = "Iris Sepal Measurements",
  highlight_colors = c(alpha("lightblue", 0.3), alpha("orange", 0.5))
)


# Test Case 9: With custom width ratios
# ============================================
clearhistory()

trees |>
  ggplot(aes(Height, Girth)) +
  geom_point() +
  geom_smooth() +
  labs(title = 'Girth ~ Smooth') + #
  theme_minimal()

ggram(widths = c(2, 1), style = stamp_code())


# Test Case 10: Minimal plot, maximum customization
# ============================================
clearhistory()

data.frame(x = 1:10, y = 1:10) |>
  ggplot(aes(x, y)) +
  geom_line(color = "steelblue", linewidth = 2)

ggram(
  title = "Simple Line Plot",
  widths = c(1, 2),
  style = stamp_code(theme = "dark"),
  highlight_colors = c(alpha("grey10", 0.2), alpha("yellow", 0.6)),
  family = "sans"
)

# Test Case 11: setting a single highlight color w alpha
# ============================================
clearhistory()

data.frame(x = 1:10, y = 1:10) |>
  ggplot(aes(x, y)) +
  geom_line(color = "steelblue", linewidth = 2)+theme_minimal()

ggram(
  style = stamp_code(theme = "warm"),
  code_color = 'deeppink',
  highlight_color = alpha('steelblue',0.2)
)

# Test Case 12: I think thats enough
# ============================================

{
set.seed(42)

df <- data.frame(
  date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = 120),
  value = cumsum(rnorm(120, mean = 0.15, sd = 1)) + 100
)
themes <- list(
  theme_minimal(base_size = 12),
    theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#1e1e1e", color = NA),
    panel.background = element_rect(fill = "#1e1e1e", color = NA),
    panel.grid.major = element_line(color = "#2a2a2a"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#bfbfbf"),
    axis.title = element_text(color = "#bfbfbf"),
    plot.title = element_text(colour = '#bfbfbf')
  )
)
}
clearhistory()
df |> ggplot(aes(x = date, y = value)) +
  geom_line(color = "#00FF33", linewidth = 1) +
  labs(title = "Stock time series", x = NULL, y = NULL) +
  themes
ggram(
  widths = c(3,1),
  style = stamp_code(theme = "dark", "ggplot2 is cool"),
  family = "JetBrains Mono",
  code_color = '#00FF33',
  highlight_color = '#1e1e1e'
)



