# Advanced alignment functions for root system analysis

# Procrustes alignment for point sets
procrustes_align <- function(points1, points2, max_iter = 100) {
  if (nrow(points1) == 0 || nrow(points2) == 0) {
    return(list(
      translation = c(0, 0),
      rotation = 0,
      scale = 1,
      aligned_points = points2
    ))
  }
  
  # Find corresponding points using nearest neighbor
  n1 <- nrow(points1)
  n2 <- nrow(points2)
  
  # Sample points if too many
  max_points <- 500
  if (n1 > max_points) {
    idx1 <- sample(1:n1, max_points)
    points1 <- points1[idx1, ]
  }
  if (n2 > max_points) {
    idx2 <- sample(1:n2, max_points)
    points2 <- points2[idx2, ]
  }
  
  # Center points
  centroid1 <- colMeans(points1)
  centroid2 <- colMeans(points2)
  
  points1_centered <- sweep(points1, 2, centroid1)
  points2_centered <- sweep(points2, 2, centroid2)
  
  # Initial alignment using centroids
  translation <- centroid1 - centroid2
  
  # Estimate scale
  scale1 <- sqrt(sum(points1_centered^2))
  scale2 <- sqrt(sum(points2_centered^2))
  scale <- scale1 / scale2
  
  # Estimate rotation using SVD
  if (nrow(points1_centered) == nrow(points2_centered)) {
    H <- t(points1_centered) %*% points2_centered
    svd_result <- svd(H)
    R <- svd_result$v %*% t(svd_result$u)
    
    # Ensure proper rotation matrix
    if (det(R) < 0) {
      svd_result$v[, 2] <- -svd_result$v[, 2]
      R <- svd_result$v %*% t(svd_result$u)
    }
    
    # Extract rotation angle
    rotation <- atan2(R[2, 1], R[1, 1]) * 180 / pi
  } else {
    rotation <- 0
  }
  
  # Apply transformation
  theta <- rotation * pi / 180
  rotation_matrix <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), 2, 2)
  
  aligned_points <- points2
  aligned_points <- sweep(aligned_points, 2, centroid2)
  aligned_points <- aligned_points %*% t(rotation_matrix)
  aligned_points <- aligned_points * scale
  aligned_points <- sweep(aligned_points, 2, -centroid1)
  
  return(list(
    translation = translation,
    rotation = rotation,
    scale = scale,
    aligned_points = aligned_points
  ))
}

# Hierarchical multi-stage optimization for temporal root growth data with advanced parameters
# Optimizes 10 parameters: translation_x, translation_y, rotation, scale, 
# stretch_x, stretch_y, keystone_h, keystone_v, shear_x, shear_y
feature_based_align <- function(rsml1_data, rsml2_data, verbose = TRUE, 
                               use_advanced_params = TRUE, convergence_threshold = 0.5) {
  
  # Extract points from both time steps
  points1 <- extract_root_points(rsml1_data)  # Earlier time point
  points2 <- extract_root_points(rsml2_data)  # Later time point
  
  if (nrow(points1) == 0 || nrow(points2) == 0) {
    warning("One or both datasets have no points. Cannot perform alignment.")
    return(list(
      translation = c(0, 0),
      rotation = 0,
      scale = 1,
      stretch_x = 1.0, stretch_y = 1.0,
      keystone_h = 0, keystone_v = 0,
      shear_x = 0, shear_y = 0,
      overlap_percentage = 0,
      optimization_steps = 0
    ))
  }
  
  if (verbose) {
    cat("Starting hierarchical multi-stage alignment optimization...\n")
    cat(sprintf("Time 1 points: %d, Time 2 points: %d\n", nrow(points1), nrow(points2)))
    if (use_advanced_params) {
      cat("Using advanced transformation parameters (10D optimization)\n")
    } else {
      cat("Using basic transformation parameters only (4D optimization)\n")
    }
  }
  
  # Initialize best parameters
  best_params <- list(
    translation_x = 0, translation_y = 0, rotation = 0, scale = 1,
    stretch_x = 1.0, stretch_y = 1.0, keystone_h = 0, keystone_v = 0,
    shear_x = 0, shear_y = 0, overlap = 0
  )
  
  total_steps <- 0
  stage_start_time <- Sys.time()
  
  # STAGE 1: Coarse search for basic parameters only
  if (verbose) cat("\n=== STAGE 1: Coarse Basic Parameter Search ===\n")
  
  coarse_result <- grid_search_basic_params(
    points1, points2, 
    translation_range = seq(-200, 200, by = 25),
    rotation_range = seq(-20, 20, by = 5),
    scale_range = seq(0.8, 1.2, by = 0.1),
    verbose = verbose, convergence_threshold = convergence_threshold
  )
  
  best_params <- update_best_params(best_params, coarse_result)
  total_steps <- total_steps + coarse_result$steps
  
  if (verbose) {
    elapsed <- as.numeric(difftime(Sys.time(), stage_start_time, units = "secs"))
    cat(sprintf("Stage 1 complete: %.1f%% overlap in %.1f seconds\n", 
               coarse_result$overlap, elapsed))
  }
  
  # STAGE 2: Fine-tune basic parameters
  if (verbose) cat("\n=== STAGE 2: Fine-tune Basic Parameters ===\n")
  stage_start_time <- Sys.time()
  
  fine_result <- refine_basic_params(
    points1, points2, coarse_result,
    verbose = verbose, convergence_threshold = convergence_threshold
  )
  
  best_params <- update_best_params(best_params, fine_result)
  total_steps <- total_steps + fine_result$steps
  
  if (verbose) {
    elapsed <- as.numeric(difftime(Sys.time(), stage_start_time, units = "secs"))
    cat(sprintf("Stage 2 complete: %.1f%% overlap in %.1f seconds\n", 
               fine_result$overlap, elapsed))
  }
  
  # STAGE 3 & 4: Advanced parameter optimization (if enabled)
  if (use_advanced_params) {
    
    # STAGE 3: Individual advanced parameter optimization
    if (verbose) cat("\n=== STAGE 3: Individual Advanced Parameter Optimization ===\n")
    stage_start_time <- Sys.time()
    
    advanced_result <- optimize_advanced_params_individual(
      points1, points2, best_params,
      verbose = verbose, convergence_threshold = convergence_threshold
    )
    
    best_params <- update_best_params(best_params, advanced_result)
    total_steps <- total_steps + advanced_result$steps
    
    if (verbose) {
      elapsed <- as.numeric(difftime(Sys.time(), stage_start_time, units = "secs"))
      cat(sprintf("Stage 3 complete: %.1f%% overlap in %.1f seconds\n", 
                 advanced_result$overlap, elapsed))
    }
    
    # STAGE 4: Joint fine-tuning of all parameters
    if (verbose) cat("\n=== STAGE 4: Joint Fine-tuning ===\n")
    stage_start_time <- Sys.time()
    
    joint_result <- joint_parameter_refinement(
      points1, points2, best_params,
      verbose = verbose, convergence_threshold = convergence_threshold
    )
    
    best_params <- update_best_params(best_params, joint_result)
    total_steps <- total_steps + joint_result$steps
    
    if (verbose) {
      elapsed <- as.numeric(difftime(Sys.time(), stage_start_time, units = "secs"))
      cat(sprintf("Stage 4 complete: %.1f%% overlap in %.1f seconds\n", 
                 joint_result$overlap, elapsed))
    }
  }
  
  if (verbose) {
    cat(sprintf("\n=== HIERARCHICAL OPTIMIZATION COMPLETE ===\n"))
    cat(sprintf("Final overlap: %.2f%% (Total steps: %d)\n", best_params$overlap, total_steps))
    cat(sprintf("Basic params: tx=%.1f, ty=%.1f, rot=%.2f°, scale=%.3f\n",
               best_params$translation_x, best_params$translation_y, 
               best_params$rotation, best_params$scale))
    if (use_advanced_params) {
      cat(sprintf("Advanced params: stretch_x=%.3f, stretch_y=%.3f\n",
                 best_params$stretch_x, best_params$stretch_y))
      cat(sprintf("                 keystone_h=%.3f, keystone_v=%.3f\n",
                 best_params$keystone_h, best_params$keystone_v))
      cat(sprintf("                 shear_x=%.3f, shear_y=%.3f\n",
                 best_params$shear_x, best_params$shear_y))
    }
  }
  
  return(list(
    translation = c(best_params$translation_x, best_params$translation_y),
    rotation = best_params$rotation,
    scale = best_params$scale,
    stretch_x = best_params$stretch_x,
    stretch_y = best_params$stretch_y,
    keystone_h = best_params$keystone_h,
    keystone_v = best_params$keystone_v,
    shear_x = best_params$shear_x,
    shear_y = best_params$shear_y,
    overlap_percentage = best_params$overlap,
    optimization_steps = total_steps
  ))
}

# ===== HIERARCHICAL OPTIMIZATION HELPER FUNCTIONS =====

# Update best parameters with new result
update_best_params <- function(best_params, new_result) {
  if (new_result$overlap > best_params$overlap) {
    # Update only the parameters that were optimized
    for (param_name in names(new_result)) {
      if (param_name %in% names(best_params)) {
        best_params[[param_name]] <- new_result[[param_name]]
      }
    }
  }
  return(best_params)
}

# Stage 1: Coarse search for basic parameters only
grid_search_basic_params <- function(points1, points2, translation_range, rotation_range, 
                                    scale_range, verbose = FALSE, convergence_threshold = 0.5) {
  
  best_overlap <- 0
  best_params <- list(translation_x = 0, translation_y = 0, rotation = 0, scale = 1)
  
  total_combinations <- length(translation_range)^2 * length(rotation_range) * length(scale_range)
  
  if (verbose) {
    cat(sprintf("  Testing %d basic parameter combinations...\n", total_combinations))
  }
  
  step_count <- 0
  progress_interval <- max(1, floor(total_combinations / 20))  # Update every 5%
  last_improvement_step <- 0
  
  # Grid search over basic parameter space
  for (tx in translation_range) {
    for (ty in translation_range) {
      for (rotation in rotation_range) {
        for (scale in scale_range) {
          
          step_count <- step_count + 1
          
          # Transform points with basic parameters only
          points2_transformed <- transform_points_basic(points2, tx, ty, rotation, scale)
          
          # Calculate overlap percentage
          overlap_pct <- calculate_temporal_overlap(points1, points2_transformed)
          
          # Update best parameters if this is better
          if (overlap_pct > best_overlap + convergence_threshold) {
            improvement <- overlap_pct - best_overlap
            best_overlap <- overlap_pct
            best_params <- list(
              translation_x = tx, translation_y = ty,
              rotation = rotation, scale = scale
            )
            last_improvement_step <- step_count
            
            if (verbose) {
              cat(sprintf("  New best: %.2f%% (+%.2f) at tx=%g, ty=%g, rot=%.1f, scale=%.2f\n",
                         overlap_pct, improvement, tx, ty, rotation, scale))
            }
          }
          
          # Show progress
          if (verbose && (step_count %% progress_interval == 0)) {
            progress <- round(100 * step_count / total_combinations)
            cat(sprintf("  Progress: %d%% (best: %.2f%%)\n", progress, best_overlap))
          }
          
          # Early termination if no improvement for a while
          if (step_count - last_improvement_step > total_combinations * 0.3) {
            if (verbose) cat("  Early termination - no significant improvement\n")
            break
          }
        }
        if (step_count - last_improvement_step > total_combinations * 0.3) break
      }
      if (step_count - last_improvement_step > total_combinations * 0.3) break
    }
    if (step_count - last_improvement_step > total_combinations * 0.3) break
  }
  
  result <- best_params
  result$overlap <- best_overlap
  result$steps <- step_count
  return(result)
}

# Stage 2: Fine-tune basic parameters
refine_basic_params <- function(points1, points2, coarse_result, verbose = FALSE, 
                              convergence_threshold = 0.1) {
  
  # Define fine search ranges around best coarse result
  fine_tx_range <- seq(coarse_result$translation_x - 25, coarse_result$translation_x + 25, by = 5)
  fine_ty_range <- seq(coarse_result$translation_y - 25, coarse_result$translation_y + 25, by = 5)
  fine_rot_range <- seq(coarse_result$rotation - 5, coarse_result$rotation + 5, by = 1)
  fine_scale_range <- seq(coarse_result$scale - 0.1, coarse_result$scale + 0.1, by = 0.02)
  
  # Clamp ranges to reasonable bounds
  fine_tx_range <- fine_tx_range[fine_tx_range >= -300 & fine_tx_range <= 300]
  fine_ty_range <- fine_ty_range[fine_ty_range >= -300 & fine_ty_range <= 300]
  fine_rot_range <- fine_rot_range[fine_rot_range >= -30 & fine_rot_range <= 30]
  fine_scale_range <- fine_scale_range[fine_scale_range >= 0.5 & fine_scale_range <= 2.0]
  
  return(grid_search_basic_params(points1, points2, fine_tx_range, fine_rot_range, 
                                 fine_scale_range, verbose, convergence_threshold))
}

# Stage 3: Individual advanced parameter optimization
optimize_advanced_params_individual <- function(points1, points2, base_params, 
                                               verbose = FALSE, convergence_threshold = 0.1) {
  
  best_params <- base_params
  total_steps <- 0
  
  # Define smart parameter ranges for advanced parameters (small corrections)
  advanced_param_ranges <- list(
    stretch_x = seq(0.95, 1.05, by = 0.01),
    stretch_y = seq(0.95, 1.05, by = 0.01),
    keystone_h = seq(-0.1, 0.1, by = 0.02),
    keystone_v = seq(-0.1, 0.1, by = 0.02),
    shear_x = seq(-0.1, 0.1, by = 0.02),
    shear_y = seq(-0.1, 0.1, by = 0.02)
  )
  
  # Optimize each advanced parameter individually
  for (param_name in names(advanced_param_ranges)) {
    if (verbose) cat(sprintf("  Optimizing %s...\n", param_name))
    
    param_range <- advanced_param_ranges[[param_name]]
    best_value <- best_params[[param_name]]
    best_overlap_for_param <- best_params$overlap
    
    for (value in param_range) {
      total_steps <- total_steps + 1
      
      # Create test parameters
      test_params <- best_params
      test_params[[param_name]] <- value
      
      # Transform points with all current parameters
      points2_transformed <- transform_points_advanced(
        points2, test_params$translation_x, test_params$translation_y,
        test_params$rotation, test_params$scale, test_params$stretch_x,
        test_params$stretch_y, test_params$keystone_h, test_params$keystone_v,
        test_params$shear_x, test_params$shear_y
      )
      
      overlap_pct <- calculate_temporal_overlap(points1, points2_transformed)
      
      if (overlap_pct > best_overlap_for_param + convergence_threshold) {
        best_overlap_for_param <- overlap_pct
        best_value <- value
        if (verbose) {
          cat(sprintf("    %s: %.3f -> %.2f%% overlap (+%.2f)\n", 
                     param_name, value, overlap_pct, overlap_pct - best_params$overlap))
        }
      }
    }
    
    # Update best parameters
    best_params[[param_name]] <- best_value
    best_params$overlap <- best_overlap_for_param
  }
  
  best_params$steps <- total_steps
  return(best_params)
}

# Stage 4: Joint fine-tuning of all parameters
joint_parameter_refinement <- function(points1, points2, base_params, verbose = FALSE, 
                                     convergence_threshold = 0.05) {
  
  best_params <- base_params
  total_steps <- 0
  max_iterations <- 100  # Limit iterations for joint optimization
  
  if (verbose) cat("  Joint fine-tuning of all parameters...\n")
  
  # Define small refinement ranges around current best values
  refinement_ranges <- list(
    translation_x = seq(-10, 10, by = 2),
    translation_y = seq(-10, 10, by = 2),
    rotation = seq(-2, 2, by = 0.5),
    scale = seq(-0.05, 0.05, by = 0.01),
    stretch_x = seq(-0.02, 0.02, by = 0.005),
    stretch_y = seq(-0.02, 0.02, by = 0.005),
    keystone_h = seq(-0.02, 0.02, by = 0.01),
    keystone_v = seq(-0.02, 0.02, by = 0.01),
    shear_x = seq(-0.02, 0.02, by = 0.01),
    shear_y = seq(-0.02, 0.02, by = 0.01)
  )
  
  # Iterative refinement: adjust one parameter at a time
  improved <- TRUE
  iteration <- 0
  
  while (improved && iteration < max_iterations) {
    improved <- FALSE
    iteration <- iteration + 1
    
    for (param_name in names(refinement_ranges)) {
      current_value <- best_params[[param_name]]
      
      for (delta in refinement_ranges[[param_name]]) {
        total_steps <- total_steps + 1
        
        # Create test parameters
        test_params <- best_params
        test_params[[param_name]] <- current_value + delta
        
        # Apply reasonable bounds
        test_params <- clamp_parameters(test_params)
        
        # Transform points
        points2_transformed <- transform_points_advanced(
          points2, test_params$translation_x, test_params$translation_y,
          test_params$rotation, test_params$scale, test_params$stretch_x,
          test_params$stretch_y, test_params$keystone_h, test_params$keystone_v,
          test_params$shear_x, test_params$shear_y
        )
        
        overlap_pct <- calculate_temporal_overlap(points1, points2_transformed)
        
        if (overlap_pct > best_params$overlap + convergence_threshold) {
          best_params <- test_params
          best_params$overlap <- overlap_pct
          improved <- TRUE
          
          if (verbose && iteration <= 5) {  # Only show first few iterations
            cat(sprintf("    Iter %d: %s %.3f -> %.2f%% overlap\n", 
                       iteration, param_name, test_params[[param_name]], overlap_pct))
          }
        }
      }
    }
    
    if (verbose && iteration %% 10 == 0) {
      cat(sprintf("    Joint refinement iteration %d, best: %.2f%%\n", iteration, best_params$overlap))
    }
  }
  
  if (verbose) {
    cat(sprintf("  Joint refinement complete after %d iterations\n", iteration))
  }
  
  best_params$steps <- total_steps
  return(best_params)
}

# Basic transformation function (4 parameters only)
transform_points_basic <- function(points, translation_x, translation_y, rotation, scale) {
  if (nrow(points) == 0) return(points)
  
  theta <- rotation * pi / 180
  center_x <- mean(points$x)
  center_y <- mean(points$y)
  
  # Center, rotate, scale, translate
  points_centered <- points
  points_centered$x <- points$x - center_x
  points_centered$y <- points$y - center_y
  
  # Rotation
  cos_theta <- cos(theta)
  sin_theta <- sin(theta)
  x_rot <- points_centered$x * cos_theta - points_centered$y * sin_theta
  y_rot <- points_centered$x * sin_theta + points_centered$y * cos_theta
  
  # Scale and translate
  points$x <- x_rot * scale + center_x + translation_x
  points$y <- y_rot * scale + center_y + translation_y
  
  return(points)
}

# Advanced transformation function (all 10 parameters)
transform_points_advanced <- function(points, translation_x, translation_y, rotation, scale,
                                    stretch_x, stretch_y, keystone_h, keystone_v, shear_x, shear_y) {
  if (nrow(points) == 0) return(points)
  
  theta <- rotation * pi / 180
  center_x <- mean(points$x)
  center_y <- mean(points$y)
  
  # Apply transformations in order: centering -> stretching -> shear -> keystone -> rotation -> scale -> translation
  points_centered <- points
  points_centered$x <- points$x - center_x
  points_centered$y <- points$y - center_y
  
  # 1. Stretching
  x_stretch <- points_centered$x * stretch_x
  y_stretch <- points_centered$y * stretch_y
  
  # 2. Shear
  x_shear <- x_stretch + shear_x * y_stretch
  y_shear <- shear_y * x_stretch + y_stretch
  
  # 3. Keystone (normalized)
  x_norm <- x_shear / (abs(max(abs(x_shear), na.rm = TRUE)) + 1e-6)
  y_norm <- y_shear / (abs(max(abs(y_shear), na.rm = TRUE)) + 1e-6)
  x_keystone <- x_shear * (1 + keystone_h * y_norm)
  y_keystone <- y_shear * (1 + keystone_v * x_norm)
  
  # 4. Rotation
  cos_theta <- cos(theta)
  sin_theta <- sin(theta)
  x_rot <- x_keystone * cos_theta - y_keystone * sin_theta
  y_rot <- x_keystone * sin_theta + y_keystone * cos_theta
  
  # 5. Scale and translate
  points$x <- x_rot * scale + center_x + translation_x
  points$y <- y_rot * scale + center_y + translation_y
  
  return(points)
}

# Clamp parameters to reasonable bounds
clamp_parameters <- function(params) {
  params$translation_x <- pmax(-500, pmin(500, params$translation_x))
  params$translation_y <- pmax(-500, pmin(500, params$translation_y))
  params$rotation <- pmax(-180, pmin(180, params$rotation))
  params$scale <- pmax(0.3, pmin(3.0, params$scale))
  params$stretch_x <- pmax(0.5, pmin(2.0, params$stretch_x))
  params$stretch_y <- pmax(0.5, pmin(2.0, params$stretch_y))
  params$keystone_h <- pmax(-0.5, pmin(0.5, params$keystone_h))
  params$keystone_v <- pmax(-0.5, pmin(0.5, params$keystone_v))
  params$shear_x <- pmax(-0.5, pmin(0.5, params$shear_x))
  params$shear_y <- pmax(-0.5, pmin(0.5, params$shear_y))
  return(params)
}

# Legacy function for backward compatibility
grid_search_alignment <- function(points1, points2, 
                                 translation_range = NULL, 
                                 translation_range_x = NULL, 
                                 translation_range_y = NULL,
                                 rotation_range, scale_range, verbose = FALSE) {
  
  # Handle translation ranges (allow separate x,y ranges for fine search)
  if (is.null(translation_range_x)) translation_range_x <- translation_range
  if (is.null(translation_range_y)) translation_range_y <- translation_range
  
  return(grid_search_basic_params(points1, points2, translation_range_x, 
                                 rotation_range, scale_range, verbose))
}

# Extract key features from root system
extract_root_features <- function(rsml_data) {
  features <- list()
  feature_idx <- 1
  
  for (plant in rsml_data$plants) {
    for (root in plant$roots) {
      if (length(root$points) > 0) {
        # Root tip
        features[[feature_idx]] <- list(
          type = "tip",
          point = root$points[[length(root$points)]],
          root_id = root$id
        )
        feature_idx <- feature_idx + 1
        
        # Root base
        features[[feature_idx]] <- list(
          type = "base",
          point = root$points[[1]],
          root_id = root$id
        )
        feature_idx <- feature_idx + 1
        
        # Branch points (simplified - where child roots connect)
        if (!is.na(root$parent)) {
          features[[feature_idx]] <- list(
            type = "branch",
            point = root$points[[1]],
            root_id = root$id,
            parent_id = root$parent
          )
          feature_idx <- feature_idx + 1
        }
      }
    }
  }
  
  return(features)
}

# Match features between two root systems
match_root_features <- function(features1, features2) {
  matches <- list()
  match_idx <- 1
  
  # Simple nearest neighbor matching by feature type
  for (f1 in features1) {
    best_match <- NULL
    min_dist <- Inf
    
    for (f2 in features2) {
      if (f1$type == f2$type) {
        dist <- sqrt((f1$point[1] - f2$point[1])^2 + (f1$point[2] - f2$point[2])^2)
        if (dist < min_dist) {
          min_dist <- dist
          best_match <- f2
        }
      }
    }
    
    if (!is.null(best_match) && min_dist < 100) {  # Distance threshold
      matches[[match_idx]] <- list(
        point1 = f1$point,
        point2 = best_match$point,
        type = f1$type,
        distance = min_dist
      )
      match_idx <- match_idx + 1
    }
  }
  
  return(matches)
}

# Calculate temporal overlap for root growth data
# Time 1 points should be a subset of Time 2 points (since plant grew)
calculate_temporal_overlap <- function(points1, points2_transformed, threshold = 8) {
  if (nrow(points1) == 0 || nrow(points2_transformed) == 0) {
    return(0)
  }
  
  # For each Time 1 point, find the closest Time 2 point
  overlapping_count <- 0
  
  for (i in 1:nrow(points1)) {
    # Calculate distances to all Time 2 points
    distances <- sqrt((points1$x[i] - points2_transformed$x)^2 + 
                     (points1$y[i] - points2_transformed$y)^2)
    
    # Check if any Time 2 point is within threshold
    if (min(distances) <= threshold) {
      overlapping_count <- overlapping_count + 1
    }
  }
  
  # Return percentage of Time 1 points that have matches in Time 2
  overlap_percentage <- (overlapping_count / nrow(points1)) * 100
  return(overlap_percentage)
}

# Calculate overlay statistics (updated to use temporal overlap)
calculate_overlay_stats <- function(rsml1_data, rsml2_data, transform_params) {
  points1 <- extract_root_points(rsml1_data)
  points2 <- extract_root_points(rsml2_data)
  
  # Transform points2
  points2_transformed <- transform_points(
    points2,
    transform_params$translation_x,
    transform_params$translation_y,
    transform_params$rotation,
    transform_params$scale
  )
  
  # Calculate temporal overlap (Time 1 should be subset of Time 2)
  overlap_percentage <- calculate_temporal_overlap(points1, points2_transformed)
  
  return(list(
    overlap_percentage = overlap_percentage,
    points1_count = nrow(points1),
    points2_count = nrow(points2),
    transform_params = transform_params
  ))
}

# Create root structure visualization plot
create_root_structure_plot <- function(rsml1_data, rsml2_data, transform_params = NULL, show_aligned = TRUE) {
  # Extract points
  points1 <- extract_root_points(rsml1_data)
  points2 <- extract_root_points(rsml2_data)
  
  # Apply transformation if provided
  if (!is.null(transform_params) && show_aligned) {
    points2 <- transform_points(
      points2,
      transform_params$translation_x,
      transform_params$translation_y,
      transform_params$rotation,
      transform_params$scale
    )
  }
  
  # Combine data
  points1$timepoint <- "Time 1"
  points2$timepoint <- ifelse(show_aligned && !is.null(transform_params), "Time 2 (Aligned)", "Time 2")
  combined_points <- rbind(points1, points2)
  
  # Create plot
  p <- ggplot(combined_points, aes(x = x, y = -y, color = timepoint)) +
    geom_point(alpha = 0.6, size = 1.2) +
    scale_color_manual(values = c("Time 1" = "red", "Time 2" = "blue", "Time 2 (Aligned)" = "darkblue")) +
    labs(title = "Root System Structure",
         x = "X Position (pixels)",
         y = "Y Position (pixels)",
         color = "Dataset") +
    theme_minimal() +
    theme(aspect.ratio = 1)
  
  return(p)
}

# Generate analysis report data
generate_report_data <- function(rsml1_data, rsml2_data, transform_params) {
  # Calculate metrics
  length1 <- calculate_root_length(rsml1_data)
  length2 <- calculate_root_length(rsml2_data)
  
  # Count roots
  root_count1 <- sum(sapply(rsml1_data$plants, function(p) length(p$roots)))
  root_count2 <- sum(sapply(rsml2_data$plants, function(p) length(p$roots)))
  
  # Calculate overlay stats
  overlay_stats <- calculate_overlay_stats(rsml1_data, rsml2_data, transform_params)
  
  report_data <- list(
    date = Sys.Date(),
    time_step1 = list(
      total_length = length1,
      root_count = root_count1,
      metadata = rsml1_data$metadata
    ),
    time_step2 = list(
      total_length = length2,
      root_count = root_count2,
      metadata = rsml2_data$metadata
    ),
    growth = list(
      length_change = length2 - length1,
      growth_rate = ((length2 - length1) / length1) * 100,
      root_count_change = root_count2 - root_count1
    ),
    alignment = list(
      translation_x = transform_params$translation_x,
      translation_y = transform_params$translation_y,
      rotation = transform_params$rotation,
      scale = transform_params$scale,
      overlap_percentage = overlay_stats$overlap_percentage
    )
  )
  
  return(report_data)
}

# Calculate spatial distribution
calculate_spatial_distribution <- function(rsml_data) {
  points <- extract_root_points(rsml_data)
  
  if (nrow(points) == 0) {
    return(list(
      centroid = c(0, 0),
      spread_x = 0,
      spread_y = 0,
      convex_hull_area = 0
    ))
  }
  
  centroid <- colMeans(points)
  spread_x <- sd(points$x)
  spread_y <- sd(points$y)
  
  # Calculate convex hull area (simplified)
  hull_area <- 0
  if (nrow(points) >= 3) {
    # Use chull for convex hull
    hull_indices <- chull(points)
    hull_points <- points[hull_indices, ]
    
    # Calculate area using shoelace formula
    n <- nrow(hull_points)
    area <- 0
    for (i in 1:n) {
      j <- ifelse(i == n, 1, i + 1)
      area <- area + hull_points[i, 1] * hull_points[j, 2]
      area <- area - hull_points[j, 1] * hull_points[i, 2]
    }
    hull_area <- abs(area) / 2
  }
  
  return(list(
    centroid = centroid,
    spread_x = spread_x,
    spread_y = spread_y,
    convex_hull_area = hull_area
  ))
}