# Init --------------------------------------------------------------------

library(grid)
library(scales)

# Colours -----------------------------------------------------------------

AssignColour <- function (x) {
  if (x == "A") result <- "#D23737"
  if (x == "P") result <- "#3191C9"
  if (x == "C") result <- "#D2BC2D"
  if (x == "T") result <- "#4EC93B"
  if (x == "D") result <- "#881F93"
  if (x == "L") result <- "#C5752B"

  return(result)
}

# Diagram Draw Function ---------------------------------------------------

DrawDiatime <- function(x_label = "P", y_label = "A",
                        isoline = TRUE,
                        isoline_label = "C",
                        isoline_orientation = "diagup",
                        isoline_flow_direction = "E") {

  # colours
  xcolour    <- AssignColour(x_label)
  xcolour2    <- muted(xcolour, l = 70, c = 50)
  ycolour    <- AssignColour(y_label)
  ycolour2    <- muted(ycolour, l = 70, c = 50)
  diagcolour <- AssignColour(isoline_label)
  diagcolour2    <- muted(diagcolour, l = 70, c = 50)

  # label positions
  ylabelx <- 0.04
  ylabely <- 0.9
  xlabelx <- 0.9
  xlabely <- 0.04

  # arrowhead style
  arrowhead1 <- arrow(angle = 20, length = unit(0.18, "inches"), type = "closed")
  arrowhead2 <- arrow(angle = 20, length = unit(0.1, "inches"),  type = "closed")

  # translate specifications into graphical parameters
  if (isoline_orientation == "diagup")   {slope <-  1; intercept <- 0}
  if (isoline_orientation == "diagdown") {slope <- -1; intercept <- 1.1}
  if (isoline_flow_direction == "E") arrowx <- c(0.25, 0.75); arrowy <- c(0.5, 0.5)
  if (isoline_flow_direction == "W") arrowx <- c(0.75, 0.25); arrowy <- c(0.5, 0.5)

  # prepare viewport
  grid.newpage()
  vp <- viewport(x = 0.5, y = 0.5,
                 width = unit(5, "cm"),
                 height = unit(5, "cm"),
                 clip = "on")
  pushViewport(vp)

  # plot...

  # ...isoline
  if (isoline == TRUE) {
    # ...isolines
    grid.abline(intercept = intercept,       slope = slope, gp = gpar(lty = "dashed", col = diagcolour2))
    grid.abline(intercept = intercept + 0.3, slope = slope, gp = gpar(lty = "dashed", col = diagcolour2))
    grid.abline(intercept = intercept - 0.3, slope = slope, gp = gpar(lty = "dashed", col = diagcolour2))
    # ...isoline arrow
    grid.lines(x = arrowx, y = arrowy,
               arrow = arrowhead1,
               gp = gpar(fill = diagcolour, col = diagcolour))
    # ...embedded scale label
    grid.text(paste0("(", isoline_label,")"), x = 0.5, y = 0.6, gp = gpar(col = diagcolour))
  }

  # ...x and y guides
  grid.lines(x = c(0.4, 0.4), y = c(0, 1), gp = gpar(lty = "dashed", col = xcolour2))
  grid.lines(x = c(0.7, 0.7), y = c(0, 1), gp = gpar(lty = "dashed", col = xcolour2))
  grid.lines(x = c(0, 1), y = c(0.4, 0.4), gp = gpar(lty = "dashed", col = ycolour2))
  grid.lines(x = c(0, 1), y = c(0.7, 0.7), gp = gpar(lty = "dashed", col = ycolour2))

  # ...x and y axis
  grid.lines(x = c(0, 1), y = c(0.1, 0.1),
             arrow = arrowhead2,
             gp = gpar(fill = xcolour, col = xcolour))
  grid.lines(x = c(0.1, 0.1), y = c(0, 1),
             arrow = arrowhead2,
             gp = gpar(fill = ycolour, col = ycolour))

  # ...x and y labels
  grid.text(x_label, x = xlabelx, y = xlabely, gp = gpar(col = xcolour))
  grid.text(y_label, x = ylabelx, y = ylabely, gp = gpar(col = ycolour))

}

DrawDiatimeIsotropic <- function(x_label = "P", y_label = "A",
                                 isoline = TRUE,
                                 isoline_label = "C",
                                 isoline_flow_direction = "E") {

  # colours
  xcolour    <- AssignColour(x_label)
  xcolour2    <- muted(xcolour, l = 70, c = 50)
  ycolour    <- AssignColour(y_label)
  ycolour2    <- muted(ycolour, l = 70, c = 50)
  diagcolour <- AssignColour(isoline_label)
  diagcolour2    <- muted(diagcolour, l = 70, c = 50)

  # label positions
  ylabelx <- 1.25
  ylabely <- 0.65
  xlabelx <- 0.9
  xlabely <- 0.05

  # arrowhead style
  arrowhead1 <- arrow(angle = 20, length = unit(0.18, "inches"), type = "closed")
  arrowhead2 <- arrow(angle = 20, length = unit(0.1, "inches"),  type = "closed")

  # translate specifications into graphical parameters
  if (isoline_flow_direction == "E") arrowx <- c(0.7, 1.1); arrowy <- c(0.5, 0.5)
  if (isoline_flow_direction == "W") arrowx <- c(1.1, 0.7); arrowy <- c(0.5, 0.5)

  # prepare viewport
  grid.newpage()
  vp <- viewport(x = 0.5, y = 0.5,
                 width = unit(5, "cm"),
                 height = unit(5, "cm"),
                 clip = "off")
  pushViewport(vp)

  # plot...

  # ...isoline
  if (isoline == TRUE) {
    # ...isolines
    grid.lines(x = c(0.35, 0.95), y = c(0.06, 1.1), gp = gpar(lty = "dashed",  col = diagcolour2))
    grid.lines(x = c(0.68, 1.12), y = c(0.06, 0.82), gp = gpar(lty = "dashed", col = diagcolour2))
    grid.lines(x = c(1.02, 1.29), y = c(0.06, 0.55), gp = gpar(lty = "dashed", col = diagcolour2))
    # ...isoline arrow
    grid.lines(x = arrowx, y = arrowy,
               arrow = arrowhead1,
               gp = gpar(fill = diagcolour, col = diagcolour))
    # ...embedded scale label
    grid.text(paste0("(", isoline_label,")"), x = 0.9, y = 0.6, gp = gpar(col = diagcolour))
  }

  # ...x and y guides
  grid.lines(x = c(1.12, 0.68), y = c(0.06, 0.81), gp = gpar(lty = "dashed", col = xcolour2))
  grid.lines(x = c(0.79, 0.52), y = c(0.06, 0.52), gp = gpar(lty = "dashed", col = xcolour2))
  grid.lines(x = c(0.47, 1.34), y = c(0.44, 0.44), gp = gpar(lty = "dashed", col = ycolour2))
  grid.lines(x = c(0.63, 1.17), y = c(0.72, 0.72), gp = gpar(lty = "dashed", col = ycolour2))

    # ...x and y axis
  grid.lines(x = c(0.3, 1.5), y = c(0.15, 0.15),
             arrow = arrowhead2,
             gp = gpar(fill = xcolour, col = xcolour))
  grid.lines(x = c(1.45, 0.85), y = c(0.06, 1.1),
             arrow = arrowhead2,
             gp = gpar(fill = ycolour, col = ycolour))

  # ...x and y labels
  grid.text(x_label, x = xlabelx, y = xlabely, gp = gpar(col = xcolour))
  grid.text(y_label, x = ylabelx, y = ylabely, gp = gpar(col = ycolour))

}

# Examples ----------------------------------------------------------------

# dimensions of exported pdf
width  <- unit(5, "cm"); height <- width

# APc
pdf(file = "./fig/APc.pdf", width = width, height = width)
  DrawDiatime(y_label = "A", x_label = "P", isoline_label = "C",
              isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
pdf(file = "./fig/APc_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "A", x_label = "P", isoline_label = "C",
                     isoline_flow_direction = "E")
dev.off()

# ACp
pdf(file = "./fig/ACp.pdf", width = width, height = width)
DrawDiatime(y_label = "A", x_label = "C", isoline_label = "P",
            isoline_orientation = "diagdown", isoline_flow_direction = "E")
dev.off()
pdf(file = "./fig/ACp_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "A", x_label = "C", isoline_label = "P",
                     isoline_flow_direction = "W")
dev.off()

# CPa
pdf(file = "./fig/CPa.pdf", width = width, height = width)
DrawDiatime(y_label = "C", x_label = "P", isoline_label = "A",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
pdf(file = "./fig/CPa_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "C", x_label = "P", isoline_label = "A",
                     isoline_flow_direction = "E")
dev.off()

# LDc
pdf(file = "./fig/LDc.pdf", width = width, height = width)
DrawDiatime(y_label = "L", x_label = "D", isoline_label = "C",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
pdf(file = "./fig/LDc_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "L", x_label = "D", isoline_label = "C",
                     isoline_flow_direction = "E")
dev.off()

# TLa
pdf(file = "./fig/TLa.pdf", width = width, height = width)
DrawDiatime(y_label = "T", x_label = "L", isoline_label = "A",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
pdf(file = "./fig/TLa_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "T", x_label = "L", isoline_label = "A",
                     isoline_flow_direction = "E")
dev.off()

# TDp
pdf(file = "./fig/TDp.pdf", width = width, height = width)
DrawDiatime(y_label = "T", x_label = "D", isoline_label = "P",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
pdf(file = "./fig/TDp_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "T", x_label = "D", isoline_label = "P",
                     isoline_flow_direction = "E")
dev.off()

# ALt
pdf(file = "./fig/ALt.pdf", width = width, height = width)
DrawDiatime(y_label = "A", x_label = "L", isoline_label = "T",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
pdf(file = "./fig/ALt_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "A", x_label = "L", isoline_label = "T",
                     isoline_flow_direction = "E")
dev.off()

# LP
pdf(file = "./fig/LP.pdf", width = width, height = width)
DrawDiatime(y_label = "L", x_label = "P", isoline = FALSE)
dev.off()
pdf(file = "./fig/LP_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "L", x_label = "P", isoline = FALSE)
dev.off()

# PDt
pdf(file = "./fig/PDt.pdf", width = width, height = width)
DrawDiatime(y_label = "P", x_label = "D", isoline_label = "T",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
pdf(file = "./fig/PDt_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "P", x_label = "D", isoline_label = "T",
                     isoline_flow_direction = "E")
dev.off()

# CDl
pdf(file = "./fig/CDl.pdf", width = width, height = width)
DrawDiatime(y_label = "C", x_label = "D", isoline_label = "L",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
pdf(file = "./fig/CDl_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "C", x_label = "D", isoline_label = "L",
                     isoline_flow_direction = "E")
dev.off()

# CT
pdf(file = "./fig/CT.pdf", width = width, height = width)
DrawDiatime(y_label = "C", x_label = "T", isoline = FALSE)
dev.off()
pdf(file = "./fig/CT_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "C", x_label = "T", isoline = FALSE)
dev.off()

# TAl
pdf(file = "./fig/TAl.pdf", width = width, height = width)
DrawDiatime(y_label = "T", x_label = "A", isoline_label = "L",
            isoline_orientation = "diagdown", isoline_flow_direction = "E")
dev.off()
pdf(file = "./fig/TAl_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "T", x_label = "A", isoline_label = "L",
                     isoline_flow_direction = "W")
dev.off()

# AD
pdf(file = "./fig/AD.pdf", width = width, height = width)
DrawDiatime(y_label = "A", x_label = "D", isoline = FALSE)
dev.off()
pdf(file = "./fig/AD_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "A", x_label = "D", isoline = FALSE)
dev.off()

# LCd
pdf(file = "./fig/LCd.pdf", width = width, height = width)
DrawDiatime(y_label = "L", x_label = "C", isoline_label = "D",
            isoline_orientation = "diagdown", isoline_flow_direction = "E")
dev.off()
pdf(file = "./fig/LCd_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "L", x_label = "C", isoline_label = "D",
                     isoline_flow_direction = "W")
dev.off()

# TPd
pdf(file = "./fig/TPd.pdf", width = width, height = width)
DrawDiatime(y_label = "T", x_label = "P", isoline_label = "D",
            isoline_orientation = "diagdown", isoline_flow_direction = "E")
dev.off()
pdf(file = "./fig/TPd_iso.pdf", width = width, height = width)
DrawDiatimeIsotropic(y_label = "T", x_label = "P", isoline_label = "D",
                     isoline_flow_direction = "E")
dev.off()
