# Fixed version of key functions from utils.R with main issues addressed

########################################################################
####                                                        ############
#### Helper functions marineSABRES T5.3 network analyses   ############
####                                                        ############
#### Expected to be used with relative paths like "../data/"        ####
########################################################################

# Safe package loading function
safe_require <- function(package) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    stop(paste("Package", package, "is required but not installed."))
  }
}

# Helper function to ensure proper path handling
ensure_path <- function(folder) {
  # Normalize path and ensure it exists
  folder <- normalizePath(folder, mustWork = FALSE)
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
    cat("Created directory:", folder, "\n")
  }
  return(folder)
}

# Function to create a matrix from 'from', 'to', and 'weight' vectors
make_matrix <- function(from, to, weight) {
  if (length(from) != length(to) || length(from) != length(weight)) {
    stop("from, to, and weight vectors must have the same length")
  }
  
  elements <- unique(c(unique(from), unique(to)))
  SES.mat <- matrix(0, length(elements), length(elements))
  colnames(SES.mat) <- elements
  rownames(SES.mat) <- elements
  
  for (i in 1:length(from)) {
    SES.mat[rownames(SES.mat) == from[i], colnames(SES.mat) == to[i]] <- weight[i]
  }
  return(SES.mat)
}

# Fixed data.load function
data.load <- function(df, folder, graph = FALSE, graph.name = NULL, graph.title = NULL) {
  # Validate inputs
  if (missing(df) || missing(folder)) {
    stop("df and folder are required parameters")
  }
  
  # Check required columns
  required_cols <- c("from", "to", "strength")
  if (!all(required_cols %in% names(df))) {
    stop(paste("Data frame must contain columns:", paste(required_cols, collapse = ", ")))
  }
  
  # Process strength values
  df$weight <- 1  # Default weight for "Strong Positive"
  df$weight[df$strength == "Strong Positive"] <- 1
  df$weight[df$strength == "Medium Positive"] <- 0.5
  df$weight[df$strength == "Weak Positive" | df$strength == "Weak positive"] <- 0.25
  df$weight[df$strength == "Medium Negative"] <- -0.5
  df$weight[df$strength == "Strong Negative"] <- -1
  df$weight[df$strength == "Weak Negative" | df$strength == "Weak negative"] <- -0.25
  
  # Handle any unrecognized strength values
  recognized_strengths <- c("Strong Positive", "Strong positive", "Medium Positive", "Medium positive", 
                           "Weak Positive", "Weak positive", "Medium Negative", "Medium negative",
                           "Strong Negative", "Strong negative", "Weak Negative", "Weak negative")
  unknown_strengths <- unique(df$strength[!df$strength %in% recognized_strengths])
  if (length(unknown_strengths) > 0) {
    warning(paste("Unknown strength values found:", paste(unknown_strengths, collapse = ", "), "- assigned weight 1"))
  }
  
  df$sign <- sign(df$weight)
  
  SES.mat <- make_matrix(from = df$from, to = df$to, weight = df$weight)
  
  if (graph && !is.null(graph.name) && !is.null(graph.title)) {
    SES.graph <- plot.SES(SES.mat,
                          folder = folder,
                          filename = graph.name,
                          title = graph.title,
                          label.cex = 1.1,
                          vsize = 30)
  }
  
  return(SES.mat)
}

# Fixed plot.SES function with better error handling
plot.SES <- function(SES.mat, folder, filename, title, w = 80, h = 40, 
                     layout = layout_with_fr, label.cex = 1.5, vsize = 20, eweight = 10) {
  
  safe_require("igraph")
  
  folder <- ensure_path(folder)
  
  SES.graph <- graph_from_adjacency_matrix(SES.mat, mode = "directed", weighted = TRUE)
  
  # Set vertex properties
  V(SES.graph)$color <- "white"
  V(SES.graph)$label.cex <- label.cex
  V(SES.graph)$size <- vsize
  
  # Set edge properties
  E(SES.graph)$sign <- sign(E(SES.graph)$weight)
  E(SES.graph)$color <- "blue"
  E(SES.graph)$color[E(SES.graph)$sign < 0] <- "red"
  E(SES.graph)$weight <- abs(E(SES.graph)$weight)
  E(SES.graph)$curved <- 0.2
  E(SES.graph)$width <- E(SES.graph)$weight * eweight
  E(SES.graph)$arrow.size <- E(SES.graph)$weight * (eweight / 2)
  
  l <- layout(SES.graph)
  
  png(paste0(folder, "/", filename, ".png"), width = w, height = h, units = "cm", res = 200)
  plot(SES.graph, layout = l, ylab = title)
  dev.off()
  
  return(SES.graph)
}

# Fixed state.shift function
state.shift <- function(mat, greed, iter, type = c('uniform','ordinal'), folder, file) {
  safe_require("reshape2")
  
  # Validate inputs
  if (missing(mat) || missing(greed) || missing(iter) || missing(folder) || missing(file)) {
    stop("All parameters (mat, greed, iter, folder, file) are required")
  }
  
  folder <- ensure_path(folder)
  
  # Create starting values
  starting.value <- runif(nrow(mat), 0, 1)
  
  # Initialize matrices
  state.sim <- array(NA, dim = c(nrow(mat), greed))
  rownames(state.sim) <- rownames(mat)
  
  # Create focus matrix for tracking changes
  mat.m <- melt(mat)
  mat.sim.df <- array(NA, dim = c(nrow(mat.m), greed + 2))
  mat.sim.df[, 1] <- mat.m$Var1
  mat.sim.df[, 2] <- mat.m$Var2
  
  tic <- Sys.time()
  
  for (i in 1:greed) {
    mat.sim <- simulate.mat(mat, type[1]) # Use first element if vector provided
    
    # Use time.simulate function (assuming it exists)
    SES <- SES.simulate(SES.mat = mat.sim, iter = iter, save.fig = FALSE, 
                       folder = NULL, fig.filename = NULL, fig.title = NULL)
    
    mat.sim.df[, i + 2] <- melt(mat.sim)$value
    state.sim[, i] <- apply(sign(SES[, max(1, (iter - 100)):iter]), 1, prod) # Fixed boundary check
  }
  
  toc <- Sys.time() - tic
  print(paste("Simulation completed in", round(as.numeric(toc), 2), "seconds"))
  
  # Save results with proper path construction
  save(state.sim, mat.sim.df, mat.m, mat, file = file.path(folder, file))
  
  return(list(state.sim = state.sim, mat.sim.df = mat.sim.df, mat.m = mat.m, mat = mat))
}

# Fixed simulate.mat function with type parameter
simulate.mat <- function(mat, type = 'uniform') {
  if (type == 'uniform') {
    newmat <- matrix(runif(prod(dim(mat)), 0, 1), nrow(mat), ncol(mat))
  } else if (type == 'ordinal') {
    vals <- c(0, 0.25, 0.5, 0.75, 1)
    newmat <- matrix(sample(vals, prod(dim(mat)), replace = TRUE), nrow(mat), ncol(mat))
  } else {
    stop("type must be either 'uniform' or 'ordinal'")
  }
  
  mat.sim <- 1 * (mat != 0) * sign(mat) * newmat
  return(mat.sim)
}

# Fixed SES.simulate function with better parameter handling
SES.simulate <- function(SES.mat, iter, save.fig = FALSE, folder = NULL, fig.filename = NULL, fig.title = NULL) {
  
  if (save.fig) {
    safe_require("Polychrome")
    safe_require("ggplot2")
    safe_require("ggfortify")
    safe_require("ggpubr")
    safe_require("grid")
    
    if (is.null(folder) || is.null(fig.filename) || is.null(fig.title)) {
      warning("save.fig = TRUE but folder, fig.filename, or fig.title is NULL. Skipping plot save.")
      save.fig <- FALSE
    } else {
      folder <- ensure_path(folder)
    }
  }
  
  SES.sim <- matrix(NA, nrow(SES.mat), iter)
  SES.sim[, 1] <- runif(nrow(SES.mat), 0, 1)
  
  for (i in 2:iter) {
    SES.sim[, i] <- t(SES.mat) %*% matrix((SES.sim[, i - 1]), ncol = 1)
  }
  
  rownames(SES.sim) <- rownames(SES.mat)
  
  if (save.fig) {
    # Plot generation code here (simplified for brevity)
    sim.melt <- reshape2::melt(SES.sim)
    
    # Create basic plot
    p1a <- ggplot(sim.melt, aes(x = log10(Var2), y = value, colour = Var1)) +
      geom_path(linewidth = 2) +
      labs(colour = "Element", y = "Progress", x = "log(time)") +
      theme_minimal(base_size = 18)
    
    # Save plot with proper path construction
    ggsave(file.path(folder, paste0(fig.filename, ".png")), plot = p1a, 
           width = 60, height = 35, units = "cm", dpi = 200)
  }
  
  return(SES.sim)
}

############ Qualitative Analysis Functions ############

# Function to compute the Laplacian of the SES matrix
SES.laplacian <- function(SES.mat, from = c("rows", "cols")) {
  if (from[1] == "rows") {
    SES.Lap <- diag(rowSums(t(SES.mat))) - t(SES.mat)
  } else {
    SES.Lap <- diag(rowSums((SES.mat))) - (SES.mat)
  }
  
  lap.e <- eigen(SES.Lap)$value
  names(lap.e) <- rownames(SES.mat)
  return(lap.e)
}

# Function to create a boolean file from the SES matrix
boolean.file.creation <- function(SES.mat, folder, filename) {
  SES.bin <- sign(SES.mat)
  
  # Clean column and row names
  colnames(SES.bin) <- gsub("\\s*\\([^\\)]+\\)", "", colnames(SES.bin))
  rownames(SES.bin) <- gsub("\\s*\\([^\\)]+\\)", "", rownames(SES.bin))
  colnames(SES.bin) <- gsub("[^[:alnum:]]", "", colnames(SES.bin))
  rownames(SES.bin) <- gsub("[^[:alnum:]]", "", rownames(SES.bin))
  colnames(SES.bin) <- gsub(" ", "_", colnames(SES.bin))
  rownames(SES.bin) <- gsub(" ", "_", rownames(SES.bin))
  
  boolean.df <- data.frame(targets = factor(colnames(SES.bin)), factors = NA)
  
  for (i in 1:ncol(SES.bin)) {
    poss <- names(which(SES.bin[, i] == 1))
    negs <- names(which(SES.bin[, i] == -1))
    if (length(negs) > 0) {
      negs <- paste0("!", negs)
    }
    all <- c(poss, negs)
    boolean.df$factors[i] <- paste(all, collapse = "|")
  }
  
  folder <- ensure_path(folder)
  write.csv(boolean.df, file = file.path(folder, paste0(filename, ".csv")), row.names = FALSE, quote = FALSE)
  return(boolean.df)
}

# Function to analyze boolean networks
boolean.analyses <- function(boolean.net, folder, filename) {
  safe_require("BoolNet")
  safe_require("igraph")
  
  # Safely get attractors
  tryCatch({
    states <- getAttractors(boolean.net, returnTable = TRUE)
  }, error = function(e) {
    stop(paste("Error in getAttractors:", e$message))
  })
  
  # Safely create state graph
  tryCatch({
    state.map <- plotStateGraph(states, layout = layout.fruchterman.reingold, plotIt = FALSE)
  }, error = function(e) {
    warning(paste("Could not create state graph:", e$message))
    return(list(states = 0, N_attractors = 0, basins = c(), attractors = list()))
  })
  
  V(state.map)$degree <- degree(state.map)
  V(state.map)$attractor <- "no"
  V(state.map)$attractor[V(state.map)$frame.color == "#000000FF"] <- "yes"
  
  core.state <- subgraph(state.map, which(V(state.map)$degree > 1))
  
  folder <- ensure_path(folder)
  write_graph(core.state, file = file.path(folder, paste0(filename, ".graphml")), format = "graphml")
  
  n_states <- length(states$stateInfo$table)
  n_attractors <- length(states$attractors)
  
  attractors <- vector("list", n_attractors)
  basin_attraction <- array(0, n_attractors)
  
  if (n_attractors > 0) {
    for (i in 1:n_attractors) {
      attractors[[i]] <- as.data.frame(getAttractorSequence(states, i))
      basin_attraction[i] <- states$attractors[[i]]$basinSize
    }
  }
  
  return(list(states = n_states, N_attractors = n_attractors, basins = basin_attraction, attractors = attractors))
}

# Combined qualitative analysis function
qualitative.analyses <- function(SES.mat, folder, filename.boolean.csv, filename.boolean.graph) {
  laplacian <- SES.laplacian(SES.mat, from = "rows")
  boolean.df <- boolean.file.creation(SES.mat, folder, filename.boolean.csv)
  boolean.network <- loadNetwork(file.path(folder, paste0(filename.boolean.csv, ".csv")))
  boolean.results <- boolean.analyses(boolean.network, folder, filename.boolean.graph)
  
  return(list(laplacian = laplacian, boolean.network = boolean.network, boolean.results = boolean.results))
}

############ Quantitative Analysis Functions ############

# Function to calculate the participation ratio
participation_ratio <- function(SES, folder, filename, title) {
  safe_require("ggplot2")
  
  # Calculate Jacobian and eigenvectors
  SES.j <- t(SES)
  LJ <- eigen(t(SES.j))$vector
  RJ <- eigen(SES.j)$vector
  
  # Calculate Participation Ratio
  PR <- rowSums(RJ * LJ)^2 / rowSums((RJ * LJ)^2)
  
  PR.df <- data.frame(group = title, components = colnames(SES), PR = PR)
  
  # Create plot
  plot1 <- ggplot(PR.df, aes(y = Re(PR), x = components)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none") +
    xlab(title) +
    ylab("Participation ratio") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  folder <- ensure_path(folder)
  ggsave(file.path(folder, paste0(filename, ".png")), plot = plot1, 
         device = "png", width = 30, height = 20, units = "cm", dpi = 200, bg = "white")
  
  return(PR.df)
}

############ Random Forest Functions ############

# Function to perform random forest analysis
random.forest <- function(sim.outcomes, ntree = 500, folder, file) {
  safe_require("randomForest")
  
  sim.outcomes$outcomes <- factor(sim.outcomes$outcomes)
  
  # Clean column names
  colnames(sim.outcomes) <- gsub("\\s*\\([^\\)]+\\)", "", colnames(sim.outcomes))
  colnames(sim.outcomes) <- gsub(" ", "_", colnames(sim.outcomes))
  colnames(sim.outcomes) <- gsub("\\.", "", colnames(sim.outcomes))
  colnames(sim.outcomes) <- gsub("\\-", "", colnames(sim.outcomes))
  
  forest <- randomForest(outcomes ~ ., data = sim.outcomes, ntree = ntree, localImp = TRUE, proximity = FALSE)
  
  folder <- ensure_path(folder)
  save(forest, file = file.path(folder, file))
  
  return(forest)
}

# Function to analyze random forest results
random.forest.res <- function(forest, folder, filename1, filename2) {
  safe_require("randomForest")
  
  # Basic importance measures
  importance_frame <- importance(forest)
  
  folder <- ensure_path(folder)
  save(importance_frame, file = file.path(folder, filename1))
  
  # Create importance plot
  png(file.path(folder, paste0(filename2, ".png")), width = 40, height = 40, units = "cm", res = 200)
  varImpPlot(forest, main = "Variable Importance")
  dev.off()
  
  return(importance_frame)
}

############ Measure Functions ############

# Function to simulate effects on the matrix based on specified measures
simulate.measure <- function(mat, measure, affected, indicators, lower, upper) {
  # Generate random effects for the affected variables
  measure.ef <- runif(length(affected), lower, upper)
  
  # Initialize a new row for the matrix
  newrow <- matrix(rep(0, ncol(mat)), nrow = 1)
  newrow[match(affected, colnames(mat))] <- measure.ef
  
  # Add the new row to the original matrix
  mat <- rbind(mat, newrow)
  
  # Generate random effects for the indicators
  measure.af <- runif(length(indicators), lower, upper)
  
  # Initialize a new column for the matrix
  newcol <- matrix(rep(0, nrow(mat)), ncol = 1)
  newcol[match(indicators, rownames(mat))] <- measure.af
  
  # Add the new column to the matrix
  mat <- cbind(mat, newcol)
  
  # Name the new row and column appropriately
  rownames(mat)[nrow(mat)] <- colnames(mat)[ncol(mat)] <- measure
  
  return(mat)
}