library(aRxiv)
library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(stringr)

create_date_chunks <- function(start_date, end_date, chunk_months = 2) {  
  seq_dates <- seq(as.Date(start_date), as.Date(end_date), by = paste(chunk_months, "months"))
  map2(seq_dates[-length(seq_dates)], seq_dates[-1] - days(1), ~c(.x, .y))
}

collect_full_arxiv <- function(start_date = "2025-01-01", 
                               end_date = Sys.Date(),
                               output_file = "arxiv_cs_complete(2024-2025).csv",
                               max_retries = 5) {
  
  all_data <- NULL
  date_chunks <- create_date_chunks(start_date, end_date)
  total_chunks <- length(date_chunks)
  
  message("Starting collection of ", total_chunks, " date chunks...")
  
  for (i in seq_along(date_chunks)) {
    chunk <- date_chunks[[i]]
    chunk_start <- chunk[1]
    chunk_end <- chunk[2]
    
    message("\n[", i, "/", total_chunks, "] Collecting ", chunk_start, " to ", chunk_end)
    
    query <- sprintf("submittedDate:[%s TO %s] AND cat:cs.*",
                     format(chunk_start, "%Y%m%d%H%M%S"),
                     format(chunk_end, "%Y%m%d%H%M%S"))
    
    chunk_data <- NULL
    total <- 0
    batch_size <- 500  
    retry_count <- 0
    success <- FALSE
    
    while (!success && retry_count < max_retries) {
      tryCatch({
        batch <- NULL
        attempt <- 1
        
        while (is.null(batch) && attempt <= 3) {
          tryCatch({
            batch <- arxiv_search(
              query = query,
              limit = batch_size,
              start = total,
              batchsize = batch_size
            )
          }, error = function(e) {
            message("  Attempt ", attempt, " failed: ", e$message)
            Sys.sleep(5 * attempt)  
            attempt <- attempt + 1
          })
        }
        
        if (is.null(batch)) {
          stop("Failed to retrieve batch after 3 attempts")
        }
        
        if (nrow(batch) == 0) {
          success <- TRUE
          break
        }
        
        cleaned_batch <- batch %>%
          mutate(
            submitted_date = as.Date(submitted),
            updated_date = as.Date(updated),
            across(where(is.list), ~map_chr(., ~paste(na.omit(.), collapse = "|"))),
            categories = str_replace_all(categories, "\\s+", " ")
          )
        
        chunk_data <- bind_rows(chunk_data, cleaned_batch)
        total <- total + nrow(batch)
        
        message("  Collected ", total, " papers in this chunk...")
        Sys.sleep(2) 
        
        if (nrow(batch) < batch_size) {
          success <- TRUE
        }
        
      }, error = function(e) {
        retry_count <<- retry_count + 1
        message("  Chunk failed (attempt ", retry_count, "/", max_retries, "): ", e$message)
        Sys.sleep(5)  
      })
    }
    
    if (!success) {
      message("  Failed to complete chunk after ", max_retries, " attempts. Moving to next chunk.")
      next
    }
    
    if (!is.null(chunk_data)) {
      all_data <- bind_rows(all_data, chunk_data)
      message("  Chunk complete. Total collected so far: ", nrow(all_data))
      
      
      backup_file <- paste0("arxiv_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      write_csv(all_data, backup_file)
      message("  Saved backup to: ", backup_file)
    }
  }
  
  
  if (!is.null(all_data)) {
    final_data <- all_data %>%
      mutate(
        year = year(submitted_date),
        month = month(submitted_date),
        cross_listed = str_detect(categories, "cross-list")
      ) %>%
      distinct(id, .keep_all = TRUE)
    
    write_csv(final_data, output_file)
    message("\nCOMPLETED: Saved ", nrow(final_data), " papers to ", output_file)
    
    return(final_data)
  } else {
    message("No data was collected.")
    return(NULL)
  }
}


arxiv_full_data <- collect_full_arxiv()
