
# meta --------------------------------------------------------------------
#' @Title - categorise_api.R  
#' @Author - simon 
#' @Date - 2026-07-30
#' @Objective - create a rds database with a category and a theme variables 
#' coded by ai.
#' @param texts Character vector of texts to categorise
#' @param api_key DeepSeek API key (from .Renviron)
#' @param batch_size Number of texts per batch
#' @param checkpoint_file Path to save checkpoint
#' @return Dataframe with category and theme columns
#--------------------------------------------------------------------------

categorise <- function(texts, api_key, batch_size = 20, checkpoint_file = "checkpoint_categorise.rds") {
  
  n <- length(texts)
  
  # Resume from checkpoint if it exists
  if (file.exists(checkpoint_file)) {
    results <- readRDS(checkpoint_file)
    done <- nrow(results)
    cat(sprintf("Resuming from checkpoint: %d rows already done\n", done))
  } else {
    results <- data.frame()
    done <- 0
  }
  
  for (i in seq(done + 1, n, by = batch_size)) {
    end <- min(i + batch_size - 1, n)
    batch <- texts[i:end]
    batch_n <- length(batch)
    
    tryCatch({
      
      prompt <- paste0(
        "You are an expert in political discourse analysis.\n",
        "Analyze EACH of the following ", batch_n, " texts (numbered 1-", batch_n, 
        ", separated by '---').\n\n",
        "For each text, provide:\n",
        "1. 'category': EXACTLY one of ['care' = topics connected to women's traditional roles as caregivers or tied to the social sphere more broadly, 
        'feminist interests' = directly affect women as women, women-centric, women's rights or women's bodies, 
        'gendered interests' = derive from, or even reinforce, traditional gender roles, anti-feminist or conservative, 
        'other']\n",
        "2. 'theme': a specific sub-category based on the previous answer:\n",
        "   - If 'care' EXACTLY one of ['childcare', 'education', 'healthcare' (incl. LAMal), 'social welfare', 'work-family reconciliation']\n",
        "   - If 'feminist interests' EXACTLY one of ['gender equality', 'anti-discrimination', 'pay gap', 'reproductive health' (incl. menstruality), 'gender-based violence' (incl. sexual violence), 'political parity']\n",
        "   - If 'gendered interests' EXACTLY one of ['traditional family', 'morality policies', 'opposition to equalizing measures']\n",
        "   - If 'other': none\n\n",
        "Texts to analyze:\n",
        paste0("Text ", 1:batch_n, ": ", batch, collapse = "\n---\n"),
        "\n\nRespond ONLY with a JSON array of objects in the same order as the texts.\n",
        'Example: [{"category": "care", "theme": "healthcare"}, {"category": "other", "theme": "none"}]'
      )
      
      response <- POST(
        api_url,
        add_headers(
          "Authorization" = paste("Bearer", api_key),
          "Content-Type" = "application/json"
        ),
        body = toJSON(list(
          model = "deepseek-chat",
          messages = list(
            list(role = "system", content = "You are an expert in political analysis. Always respond with valid JSON."),
            list(role = "user", content = prompt)
          ),
          temperature = 0.1,
          max_tokens = 500,
          seed = 42
        ), auto_unbox = TRUE)
      )
      
      content_text <- content(response, "parsed")$choices[[1]]$message$content
      parsed <- fromJSON(content_text)  # let errors bubble to outer tryCatch
      
      if (is.data.frame(parsed)) {
        batch_df <- parsed
      } else {
        batch_df <- data.frame(
          category = sapply(parsed, function(x) if (is.null(x$category)) NA else x$category),
          theme    = sapply(parsed, function(x) if (is.null(x$theme))    NA else x$theme)
        )
      }
      
      # sanity check: did we get one row per input text?
      if (nrow(batch_df) != batch_n) {
        stop(sprintf("expected %d rows, got %d", batch_n, nrow(batch_df)))
      }
      
      results <- rbind(results, batch_df)
      saveRDS(results, checkpoint_file)
      cat(sprintf("✓ %d/%d\n", end, n))
      
    }, error = function(e) {
      cat(sprintf("⚠️ Batch %d-%d failed: %s\n", i, end, conditionMessage(e)))
    })
    
    Sys.sleep(0.3)
  }
  
  return(results)
}
