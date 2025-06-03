get_iggy_df <- function(i, j, iggy_n_tb, iggy_rep, iggypop_ref) {
  
  # list
  start <- (96/iggy_rep)*(i-1)+1
  if (i != iggy_n_tb) { end <- (96/iggy_rep)*i } else { end <- nrow(iggypop_ref) }
  index_list <- iggypop_ref$primer_index[start:end]
  # dataframe
  df <- matrix(NA, nrow = 8, ncol = 12) %>% as.data.frame()
  max <- ceiling(length(index_list)/2)
  
  # primer_index
  if (j == 1) {
    if (iggy_rep == 6) {
      for (k in 1:max) {
        id_1 <- 2*(k-1)+1
        df[k, 1:6] <- index_list[id_1]
        if (k<=max&&length(index_list)==max*2) {
          id_2 <- 2*k
          df[k, 7:12] <- index_list[id_2]
        }
      }
    } else {
      for (k in 1:length(index_list)) {
        df[, k] <- index_list[k]
      }
    }
  }
  
  # Replicate
  if (j == 2) {
    df_dft <- read.xlsx("./data/SampleData_IGGYPOP.xlsx", sheet = j)
    if (iggy_rep == 6) {
      df_temp <- df_dft[1:8, 2:13]
      for (k in 1:max) {
        id_1 <- 2*(k-1)+1
        df[k, 1:6] <- df_temp[k, 1:6]
        if (k<=max&&length(index_list)==max*2) {
          id_2 <- 2*k
          df[k, 7:12] <- df_temp[k, 7:12]
        }
      }
    } else {
      df_temp <- df_dft[10:17, 2:13]
      for (k in 1:length(index_list)) {
        df[, k] <- df_temp[, k]
      }
    }
  }
  
  # Primer plate
  if (j == 3) {
    df_temp <- matrix(i, nrow = 8, ncol = 12) %>% as.data.frame()
    if (iggy_rep == 6) {
      for (k in 1:max) {
        id_1 <- 2*(k-1)+1
        df[k, 1:6] <- df_temp[k, 1:6]
        if (k<=max&&length(index_list)==max*2) {
          id_2 <- 2*k
          df[k, 7:12] <- df_temp[k, 7:12]
        }
      }
    } else {
      for (k in 1:length(index_list)) {
        df[, k] <- df_temp[, k]
      }
    }
  }
  
  # Primer well
  if (j == 4) {
    df_dft <- read.xlsx("./data/SampleData_IGGYPOP.xlsx", sheet = j)
    df_temp <- df_dft[1:8, 2:13]
    if (iggy_rep == 6) {
      for (k in 1:max) {
        id_1 <- 2*(k-1)+1
        df[k, 1:6] <- df_temp[k, 1:6]
        if (k<=max&&length(index_list)==max*2) {
          id_2 <- 2*k
          df[k, 7:12] <- df_temp[k, 7:12]
        }
      }
    } else {
      for (k in 1:length(index_list)) {
        df[, k] <- df_temp[, k]
      }
    }
  }
  
  # change the colnames and rownames
  colnames(df) <- seq_len(12)
  rownames(df) <- LETTERS[seq_len(8)]
  
  ## force type
  if (j %in% 2:3) {
    df[] <- lapply(df, as.numeric)
  } else {
    df[] <- lapply(df, as.character)
  }
  
  return(df)
  
}
