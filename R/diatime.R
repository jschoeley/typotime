# Init --------------------------------------------------------------------

library(grid)

# Diagram Draw Function ---------------------------------------------------

DrawDiatime <- function(x_label = "P", y_label = "A",
                        isoline = TRUE,
                        isoline_label = "C",
                        isoline_orientation = "diagup",
                        isoline_flow_direction = "E") {

  # label positions
  ylabelx <- 0.04
  ylabely <- 0.9
  xlabelx <- 0.9
  xlabely <- 0.04

  # translate specifications into graphical parameters
  if (isoline_orientation == "diagup")   {slope <-  1; intercept <- 0}
  if (isoline_orientation == "diagdown") {slope <- -1; intercept <- 1}
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
  # ...x and y axis
  grid.lines(x = c(0, 1), y = c(0.1, 0.1))
  grid.lines(x = c(0.1, 0.1), y = c(0, 1))
  # ...x and y labels
  grid.text(x_label, x = xlabelx, y = xlabely)
  grid.text(y_label, x = ylabelx, y = ylabely)
  if (isoline == TRUE) {
    # ...isolines
    grid.abline(intercept = intercept,       slope = slope, gp = gpar(lty = "dashed"))
    grid.abline(intercept = intercept + 0.5, slope = slope, gp = gpar(lty = "dashed"))
    grid.abline(intercept = intercept - 0.5, slope = slope, gp = gpar(lty = "dashed"))
    # ...isoline arrow
    grid.lines(x = arrowx, y = arrowy, arrow = arrow(angle = 20))
    # ...embedded scale label
    grid.text(paste0("(", isoline_label,")"), x = 0.5, y = 0.6)
  }
}

# Examples ----------------------------------------------------------------

# dimensions of exported pdf
width  <- unit(2.6, "cm"); height <- width

# APc
pdf(file = "./fig/APc.pdf", width = width, height = width)
  DrawDiatime(y_label = "A", x_label = "P", isoline_label = "C",
              isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
# ACp
pdf(file = "./fig/ACp.pdf", width = width, height = width)
DrawDiatime(y_label = "A", x_label = "C", isoline_label = "P",
            isoline_orientation = "diagdown", isoline_flow_direction = "E")
dev.off()
# CPa
pdf(file = "./fig/CPa.pdf", width = width, height = width)
DrawDiatime(y_label = "C", x_label = "P", isoline_label = "A",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
# LDc
pdf(file = "./fig/LDc.pdf", width = width, height = width)
DrawDiatime(y_label = "L", x_label = "D", isoline_label = "C",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
# TLa
pdf(file = "./fig/TLa.pdf", width = width, height = width)
DrawDiatime(y_label = "T", x_label = "L", isoline_label = "A",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
# TDp
pdf(file = "./fig/TDp.pdf", width = width, height = width)
DrawDiatime(y_label = "T", x_label = "D", isoline_label = "P",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
# ALt
pdf(file = "./fig/ALt.pdf", width = width, height = width)
DrawDiatime(y_label = "A", x_label = "L", isoline_label = "T",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
# LP
pdf(file = "./fig/LP.pdf", width = width, height = width)
DrawDiatime(y_label = "L", x_label = "P", isoline = FALSE)
dev.off()
# PDt
pdf(file = "./fig/PDt.pdf", width = width, height = width)
DrawDiatime(y_label = "P", x_label = "D", isoline_label = "T",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
# CDl
pdf(file = "./fig/CDl.pdf", width = width, height = width)
DrawDiatime(y_label = "C", x_label = "D", isoline_label = "L",
            isoline_orientation = "diagup", isoline_flow_direction = "E")
dev.off()
# CT
pdf(file = "./fig/CT.pdf", width = width, height = width)
DrawDiatime(y_label = "C", x_label = "T", isoline = FALSE)
dev.off()
# TAl
pdf(file = "./fig/TAl.pdf", width = width, height = width)
DrawDiatime(y_label = "T", x_label = "A", isoline_label = "L",
            isoline_orientation = "diagdown", isoline_flow_direction = "E")
dev.off()
# LCd
pdf(file = "./fig/LCd.pdf", width = width, height = width)
DrawDiatime(y_label = "L", x_label = "C", isoline_label = "D",
            isoline_orientation = "diagdown", isoline_flow_direction = "E")
dev.off()
# TP
pdf(file = "./fig/TP.pdf", width = width, height = width)
DrawDiatime(y_label = "T", x_label = "P", isoline = FALSE)
dev.off()
