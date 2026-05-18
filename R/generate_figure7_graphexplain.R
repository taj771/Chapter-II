# Figure 7: Economic benefit of flexible water allocation
#
# MV curves chosen so equimarginal condition holds exactly at depicted allocations:
#   MV1(Q) = 3.0 - 0.010*Q  (high-value crop, e.g. potato/canola)
#   MV2(Q) = 1.5 - 0.005*Q  (lower-value crop, e.g. wheat)
#
# Q0 = 150 mm (rigid quota per crop)
# Q* = 50 mm  (optimal transfer)
# At new allocations: MV1(200) = MV2(100) = 1.0 = lambda  [equimarginal verified]
#
# Shading:
#   Green: gain for crop 1 = area under MV1 from Q0 to Q0+Q*
#   Red:   loss for crop 2 = area under MV2 from Q0-Q* to Q0
#   Net    = green - red = welfare gain from flexible allocation

library(ggplot2)
library(dplyr)

Q0    <- 150
Qstar <- 50
lam   <- 1.0

MV1 <- function(q) 3.0 - 0.010 * q
MV2 <- function(q) 1.5 - 0.005 * q

q_range <- seq(0, 280, by = 1)

curves <- bind_rows(
  data.frame(q = q_range, mv = MV1(q_range), crop = "MV[1](Q)"),
  data.frame(q = q_range, mv = MV2(q_range), crop = "MV[2](Q)")
)

# Shading polygons
gain_poly <- data.frame(
  q  = c(Q0,       seq(Q0, Q0+Qstar, 1), Q0+Qstar, Q0),
  mv = c(lam, MV1(seq(Q0, Q0+Qstar, 1)),       lam, lam)
)

loss_poly <- data.frame(
  q  = c(Q0-Qstar, seq(Q0-Qstar, Q0, 1), Q0, Q0-Qstar),
  mv = c(lam,  MV2(seq(Q0-Qstar, Q0, 1)), lam,       lam)
)

ggplot() +

  # shaded gain (green)
  geom_polygon(data = gain_poly, aes(x = q, y = mv),
               fill = "#2ca25f", alpha = 0.35, colour = NA) +

  # shaded loss (red)
  geom_polygon(data = loss_poly, aes(x = q, y = mv),
               fill = "#de2d26", alpha = 0.35, colour = NA) +

  # MV curves
  geom_line(data = filter(curves, crop == "MV[1](Q)"),
            aes(x = q, y = mv), linewidth = 0.9, colour = "black") +
  geom_line(data = filter(curves, crop == "MV[2](Q)"),
            aes(x = q, y = mv), linewidth = 0.9, colour = "black",
            linetype = "dashed") +

  # lambda line (equimarginal shadow price)
  geom_segment(aes(x = Q0-Qstar, xend = Q0+Qstar+10, y = lam, yend = lam),
               linewidth = 0.55, colour = "grey30", linetype = "dotted") +

  # vertical guide lines at Q0-Q*, Q0, Q0+Q*
  geom_vline(xintercept = c(Q0-Qstar, Q0, Q0+Qstar),
             linewidth = 0.35, colour = "grey50", linetype = "solid") +

  annotate("text", x = Q0 - Qstar/2, y = 0.58,
           label = expression(Q^"*"~"(Crop 2 gives up)"),
           size = 2.3, colour = "#a81c12", vjust = 0.5, hjust = 0.5) +
  annotate("text", x = Q0 + 4, y = 1.63,
           label = expression(Q^"*"~"(Crop 1 receives)"),
           size = 2.3, colour = "#1a7f4b", vjust = 0.5, hjust = 0) +

  # crop curve labels
  annotate("text", x = 240, y = MV1(240) + 0.10, label = expression(MV[1](Q)),
           size = 3.5, hjust = 0) +
  annotate("text", x = 210, y = MV2(210) - 0.18, label = expression(MV[2](Q)),
           size = 3.5, hjust = 0) +

  # lambda label
  annotate("text", x = Q0+Qstar+14, y = lam, label = expression(lambda),
           size = 3.8, vjust = 0.4, hjust = 0) +

  # x-axis tick labels
  scale_x_continuous(
    breaks = c(Q0-Qstar, Q0, Q0+Qstar),
    labels = c(expression(Q[0]-Q["*"]), expression(Q[0]), expression(Q[0]+Q["*"])),
    limits = c(20, 285)
  ) +
  scale_y_continuous(limits = c(0, 3.1), expand = c(0, 0), labels = NULL) +

  labs(
    x = "Irrigation (mm)",
    y = expression("Marginal Value ($/m"^3*")")
  ) +

  # gain/loss annotations — centred in each triangle
  annotate("text",
           x = Q0 + Qstar * 0.55,
           y = lam + (MV1(Q0 + Qstar * 0.55) - lam) * 0.45,
           label = "Gain", size = 2.4, colour = "#1a7f4b", fontface = "bold") +
  annotate("text",
           x = Q0 - Qstar * 0.45,
           y = lam - (lam - MV2(Q0 - Qstar * 0.45)) * 0.5,
           label = "Loss", size = 2.4, colour = "#a81c12", fontface = "bold") +

  theme_classic(base_size = 12) +
  theme(
    axis.text.x  = element_text(size = 10),
    axis.title   = element_text(size = 11),
    plot.margin  = margin(10, 20, 5, 5)
  )

out_dir <- here::here("Dissertation_Latex_Project/Figures2")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(out_dir, "graphexplain.pdf"),
       width = 6, height = 4.5, dpi = 300)
ggsave(file.path(out_dir, "graphexplain.png"),
       width = 6, height = 4.5, dpi = 300)

cat("Saved: graphexplain.pdf + graphexplain.png\n")
cat("Verify: MV1(200) =", MV1(200), " MV2(100) =", MV2(100),
    " lambda =", lam, "\n")
cat("Gain area:", integrate(function(q) MV1(q) - lam, Q0, Q0+Qstar)$value, "\n")
cat("Loss area:", integrate(function(q) MV2(q) - lam, Q0-Qstar, Q0)$value, "\n")
cat("Net gain:", integrate(function(q) MV1(q)-lam, Q0, Q0+Qstar)$value -
               integrate(function(q) MV2(q)-lam, Q0-Qstar, Q0)$value, "\n")
