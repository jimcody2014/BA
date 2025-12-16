# ============================================================================
# Weekly Report Agent - TRUE AGENTIC VERSION
#
# This version implements real agentic behavior:
#   - Claude decides what data to request via tool calls
#   - Claude can iterate and request more data based on findings
#   - Claude chooses the report format based on analysis
#   - Multi-turn conversation loop with tool execution
#
# Usage: 
#   Sys.setenv(ANTHROPIC_API_KEY = "your-key")
#   source("agentic_report_agent.R")
#
# Required packages: httr, jsonlite, officer
# ============================================================================

library(httr)
library(jsonlite)
library(officer)

# ============================================================================
# CONFIGURATION
# ============================================================================

CONFIG <- list(
  location = "Boston, MA",
  topic = "Ever marijuana use",
  anthropic_model = "claude-sonnet-4-20250514",
  output_file = "weekly_report.docx",
  max_iterations = 15  # Safety limit on agent loops
)

# ============================================================================
# SIMULATED DATA STORE (Would be real API in production)
# ============================================================================

DATA_STORE <- list(
  # Available years
  available_years = c("2011", "2013", "2015", "2017", "2019"),
  
  # Overall rates by year
  overall = list(
    `2011` = list(value = 8.4, sampleSize = 1180, ci_low = 7.2, ci_high = 9.6),
    `2013` = list(value = 9.8, sampleSize = 1250, ci_low = 8.5, ci_high = 11.1),
    `2015` = list(value = 11.2, sampleSize = 1350, ci_low = 9.8, ci_high = 12.6),
    `2017` = list(value = 12.6, sampleSize = 1403, ci_low = 11.1, ci_high = 14.1),
    `2019` = list(value = 14.1, sampleSize = 1520, ci_low = 12.5, ci_high = 15.7)
  ),
  
  # Breakdowns by dimension and year
  byGrade = list(
    `2011` = list(`6th` = list(value = 5.2, sampleSize = 320), 
                  `7th` = list(value = 8.1, sampleSize = 410),
                  `8th` = list(value = 12.1, sampleSize = 440)),
    `2013` = list(`6th` = list(value = 6.1, sampleSize = 340),
                  `7th` = list(value = 9.5, sampleSize = 445),
                  `8th` = list(value = 13.8, sampleSize = 455)),
    `2015` = list(`6th` = list(value = 7.8, sampleSize = 380),
                  `7th` = list(value = 11.1, sampleSize = 485),
                  `8th` = list(value = 14.9, sampleSize = 470)),
    `2017` = list(`6th` = list(value = 9.0, sampleSize = 394),
                  `7th` = list(value = 12.7, sampleSize = 504),
                  `8th` = list(value = 16.5, sampleSize = 490)),
    `2019` = list(`6th` = list(value = 10.2, sampleSize = 420),
                  `7th` = list(value = 14.0, sampleSize = 540),
                  `8th` = list(value = 18.1, sampleSize = 550))
  ),
  
  bySex = list(
    `2011` = list(Female = list(value = 8.9, sampleSize = 590),
                  Male = list(value = 7.8, sampleSize = 585)),
    `2013` = list(Female = list(value = 10.5, sampleSize = 625),
                  Male = list(value = 9.0, sampleSize = 620)),
    `2015` = list(Female = list(value = 12.1, sampleSize = 680),
                  Male = list(value = 10.2, sampleSize = 665)),
    `2017` = list(Female = list(value = 14.8, sampleSize = 703),
                  Male = list(value = 10.5, sampleSize = 694)),
    `2019` = list(Female = list(value = 16.2, sampleSize = 760),
                  Male = list(value = 12.0, sampleSize = 755))
  ),
  
  byRace = list(
    `2011` = list(`Hispanic or Latino` = list(value = 11.2, sampleSize = 520),
                  `Black or African American` = list(value = 8.1, sampleSize = 280),
                  White = list(value = 5.9, sampleSize = 130),
                  Asian = list(value = 3.8, sampleSize = 95)),
    `2013` = list(`Hispanic or Latino` = list(value = 12.8, sampleSize = 560),
                  `Black or African American` = list(value = 9.5, sampleSize = 290),
                  White = list(value = 6.5, sampleSize = 140),
                  Asian = list(value = 4.2, sampleSize = 105)),
    `2015` = list(`Hispanic or Latino` = list(value = 14.5, sampleSize = 610),
                  `Black or African American` = list(value = 10.8, sampleSize = 305),
                  White = list(value = 7.2, sampleSize = 148),
                  Asian = list(value = 4.9, sampleSize = 118)),
    `2017` = list(`Hispanic or Latino` = list(value = 16.8, sampleSize = 638),
                  `Black or African American` = list(value = 11.9, sampleSize = 312),
                  White = list(value = 6.8, sampleSize = 155),
                  Asian = list(value = 5.3, sampleSize = 123)),
    `2019` = list(`Hispanic or Latino` = list(value = 18.5, sampleSize = 680),
                  `Black or African American` = list(value = 13.2, sampleSize = 340),
                  White = list(value = 8.1, sampleSize = 170),
                  Asian = list(value = 6.0, sampleSize = 135))
  ),
  
  # Additional context data
  policy_changes = list(
    `2016` = "Massachusetts legalized recreational marijuana (Nov 2016)",
    `2012` = "Massachusetts legalized medical marijuana"
  ),
  
  national_comparison = list(
    `2017` = list(national_rate = 14.0, state_rank = 28),
    `2019` = list(national_rate = 15.2, state_rank = 25)
  )
)

# ============================================================================
# TOOL DEFINITIONS (What Claude can call)
# ============================================================================

TOOLS <- list(
  list(
    name = "get_available_data",
    description = "Get metadata about what data is available: years, dimensions, and topics. Call this first to understand what you can analyze.",
    input_schema = list(
      type = "object",
      properties = setNames(list(), character(0)),
      required = list()
    )
  ),
  list(
    name = "get_overall_rate",
    description = "Get the overall marijuana use rate for a specific year. Returns rate, sample size, and confidence interval.",
    input_schema = list(
      type = "object",
      properties = list(
        year = list(type = "string", description = "Year to query (e.g., '2017')")
      ),
      required = list("year")
    )
  ),
  list(
    name = "get_breakdown",
    description = "Get marijuana use rates broken down by a demographic dimension for a specific year.",
    input_schema = list(
      type = "object",
      properties = list(
        year = list(type = "string", description = "Year to query"),
        dimension = list(type = "string", enum = c("grade", "sex", "race"),
                         description = "Dimension to break down by: 'grade', 'sex', or 'race'")
      ),
      required = list("year", "dimension")
    )
  ),
  list(
    name = "get_historical_trend",
    description = "Get the overall rate trend across all available years. Use this to identify long-term patterns.",
    input_schema = list(
      type = "object",
      properties = setNames(list(), character(0)),
      required = list()
    )
  ),
  list(
    name = "get_subgroup_trend",
    description = "Get historical trend for a specific subgroup (e.g., '8th grade' or 'Female'). Use when you spot a concerning value and want to see if it's part of a pattern.",
    input_schema = list(
      type = "object",
      properties = list(
        dimension = list(type = "string", enum = c("grade", "sex", "race")),
        subgroup = list(type = "string", description = "The specific subgroup (e.g., '8th', 'Female', 'Hispanic or Latino')")
      ),
      required = list("dimension", "subgroup")
    )
  ),
  list(
    name = "get_policy_context",
    description = "Get relevant policy changes that might explain trends in the data.",
    input_schema = list(
      type = "object",
      properties = setNames(list(), character(0)),
      required = list()
    )
  ),
  list(
    name = "get_national_comparison",
    description = "Compare local rates to national averages for context.",
    input_schema = list(
      type = "object",
      properties = list(
        year = list(type = "string", description = "Year to compare")
      ),
      required = list("year")
    )
  ),
  list(
    name = "generate_report",
    description = "Generate the final Word document report. Call this only after you have completed your analysis. You control the report structure.",
    input_schema = list(
      type = "object",
      properties = list(
        title = list(type = "string", description = "Report title"),
        subtitle = list(type = "string", description = "Report subtitle with date range and location"),
        executive_summary = list(type = "string", description = "2-3 sentence executive summary"),
        sections = list(
          type = "array",
          description = "Array of report sections. You decide what sections to include based on your analysis.",
          items = list(
            type = "object",
            properties = list(
              heading = list(type = "string"),
              content = list(type = "string", description = "Paragraph content for this section"),
              include_table = list(type = "boolean", description = "Whether to include a data table"),
              table_data = list(type = "object", description = "If include_table is true, provide table data with 'headers' array and 'rows' array of arrays"),
              alert_level = list(type = "string", enum = c("none", "watch", "warning", "critical"),
                                 description = "Visual styling for this section")
            ),
            required = list("heading", "content")
          )
        ),
        recommendations = list(
          type = "array",
          items = list(type = "string"),
          description = "List of actionable recommendations"
        ),
        methodology_notes = list(type = "string", description = "Notes on data sources and limitations")
      ),
      required = list("title", "sections")
    )
  )
)

# ============================================================================
# WEB SEARCH HELPER (Real web search via Claude API)
# ============================================================================

search_web <- function(query) {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  
  cat(sprintf("      🌐 Searching: \"%s\"\n", query))
  
  response <- POST(
    "https://api.anthropic.com/v1/messages",
    add_headers(
      "Content-Type" = "application/json",
      "x-api-key" = api_key,
      "anthropic-version" = "2023-06-01"
    ),
    body = toJSON(list(
      model = "claude-sonnet-4-20250514",
      max_tokens = 1024,
      tools = list(
        list(
          type = "web_search_20250305",
          name = "web_search",
          max_uses = 3
        )
      ),
      messages = list(list(
        role = "user", 
        content = sprintf("Search the web and provide a concise factual summary: %s", query)
      ))
    ), auto_unbox = TRUE),
    encode = "raw"
  )
  
  if (http_error(response)) {
    return(list(error = "Web search failed"))
  }
  
  result <- content(response, as = "text", encoding = "UTF-8") |>
    fromJSON(simplifyVector = FALSE)
  
  # Extract text from response
  text_parts <- sapply(result$content, function(block) {
    if (block$type == "text") block$text else NULL
  })
  
  paste(text_parts[!sapply(text_parts, is.null)], collapse = "\n")
}

# ============================================================================
# TOOL EXECUTION (Process Claude's tool calls)
# ============================================================================

execute_tool <- function(tool_name, tool_input) {
  cat(sprintf("   🔧 Executing: %s\n", tool_name))
  
  result <- switch(tool_name,
                   "get_available_data" = {
                     list(
                       location = CONFIG$location,
                       topic = CONFIG$topic,
                       available_years = DATA_STORE$available_years,
                       available_dimensions = c("grade", "sex", "race"),
                       notes = "Data comes from Youth Risk Behavior Survey (YRBS). Survey conducted every 2 years."
                     )
                   },
                   
                   "get_overall_rate" = {
                     year <- tool_input$year
                     if (!year %in% names(DATA_STORE$overall)) {
                       list(error = sprintf("Year %s not available. Available years: %s", 
                                            year, paste(DATA_STORE$available_years, collapse = ", ")))
                     } else {
                       c(list(year = year), DATA_STORE$overall[[year]])
                     }
                   },
                   
                   "get_breakdown" = {
                     year <- tool_input$year
                     dimension <- tool_input$dimension
                     dim_key <- switch(dimension, grade = "byGrade", sex = "bySex", race = "byRace")
                     
                     if (!year %in% names(DATA_STORE[[dim_key]])) {
                       list(error = sprintf("Year %s not available", year))
                     } else {
                       list(
                         year = year,
                         dimension = dimension,
                         data = DATA_STORE[[dim_key]][[year]]
                       )
                     }
                   },
                   
                   "get_historical_trend" = {
                     trend <- lapply(DATA_STORE$available_years, function(y) {
                       c(list(year = y), DATA_STORE$overall[[y]])
                     })
                     list(
                       trend = trend,
                       years_covered = DATA_STORE$available_years,
                       total_change = DATA_STORE$overall[["2019"]]$value - DATA_STORE$overall[["2011"]]$value
                     )
                   },
                   
                   "get_subgroup_trend" = {
                     dimension <- tool_input$dimension
                     subgroup <- tool_input$subgroup
                     dim_key <- switch(dimension, grade = "byGrade", sex = "bySex", race = "byRace")
                     
                     trend <- lapply(DATA_STORE$available_years, function(y) {
                       if (subgroup %in% names(DATA_STORE[[dim_key]][[y]])) {
                         c(list(year = y), DATA_STORE[[dim_key]][[y]][[subgroup]])
                       } else {
                         list(year = y, error = "Subgroup not found")
                       }
                     })
                     
                     values <- sapply(trend, function(t) if (!is.null(t$value)) t$value else NA)
                     list(
                       subgroup = subgroup,
                       dimension = dimension,
                       trend = trend,
                       total_change = if (all(!is.na(values))) values[length(values)] - values[1] else NA
                     )
                   },
                   
                   "get_policy_context" = {
                     # REAL WEB SEARCH for policy changes
                     query <- sprintf(
                       "marijuana cannabis legalization law history timeline %s state", 
                       CONFIG$location
                     )
                     search_result <- search_web(query)
                     
                     list(
                       source = "web_search",
                       location = CONFIG$location,
                       policy_summary = search_result
                     )
                   },
                   
                   "get_national_comparison" = {
                     year <- tool_input$year
                     
                     # REAL WEB SEARCH for national comparison data
                     query <- sprintf(
                       "youth marijuana use rate national average United States %s YRBS CDC statistics",
                       year
                     )
                     search_result <- search_web(query)
                     
                     # Also include local rate for comparison
                     local_rate <- if (year %in% names(DATA_STORE$overall)) {
                       DATA_STORE$overall[[year]]$value
                     } else {
                       NA
                     }
                     
                     list(
                       source = "web_search",
                       year = year,
                       local_rate = local_rate,
                       national_data = search_result
                     )
                   },
                   
                   "generate_report" = {
                     generate_word_report(tool_input)
                     list(success = TRUE, file = CONFIG$output_file)
                   },
                   
                   # Default
                   list(error = sprintf("Unknown tool: %s", tool_name))
  )
  
  return(result)
}

# ============================================================================
# REPORT GENERATOR (Called by generate_report tool)
# ============================================================================

generate_word_report <- function(report_spec) {
  cat("   📄 Building Word document with agent-specified structure...\n")
  
  doc <- read_docx()
  
  # Style definitions
  title_props <- fp_text(font.size = 24, bold = TRUE, color = "#1a365d", font.family = "Arial")
  subtitle_props <- fp_text(font.size = 12, italic = TRUE, color = "#718096", font.family = "Arial")
  heading_props <- fp_text(font.size = 14, bold = TRUE, color = "#2c5282", font.family = "Arial")
  body_props <- fp_text(font.size = 11, color = "#2d3748", font.family = "Arial")
  alert_props <- list(
    watch = fp_text(font.size = 11, color = "#D69E2E", font.family = "Arial"),
    warning = fp_text(font.size = 11, bold = TRUE, color = "#DD6B20", font.family = "Arial"),
    critical = fp_text(font.size = 11, bold = TRUE, color = "#C53030", font.family = "Arial")
  )
  
  # Title
  doc <- doc |>
    body_add_fpar(fpar(ftext(report_spec$title %||% "Youth Risk Behavior Report", title_props)), 
                  style = "centered") |>
    body_add_par("")
  
  # Subtitle
  if (!is.null(report_spec$subtitle)) {
    doc <- doc |>
      body_add_fpar(fpar(ftext(report_spec$subtitle, subtitle_props)), style = "centered") |>
      body_add_par("")
  }
  
  # Executive Summary
  if (!is.null(report_spec$executive_summary)) {
    doc <- doc |>
      body_add_fpar(fpar(ftext("Executive Summary", heading_props))) |>
      body_add_par("") |>
      body_add_fpar(fpar(ftext(report_spec$executive_summary, body_props))) |>
      body_add_par("")
  }
  
  # Dynamic sections (agent decides what to include)
  for (section in report_spec$sections) {
    # Section heading
    doc <- doc |>
      body_add_fpar(fpar(ftext(section$heading, heading_props))) |>
      body_add_par("")
    
    # Section content with optional alert styling
    text_style <- if (!is.null(section$alert_level) && section$alert_level != "none") {
      alert_props[[section$alert_level]] %||% body_props
    } else {
      body_props
    }
    
    # Add alert indicator if needed
    if (!is.null(section$alert_level) && section$alert_level %in% c("warning", "critical")) {
      prefix <- if (section$alert_level == "critical") "⚠️ CRITICAL: " else "⚠ WARNING: "
      doc <- doc |> body_add_fpar(fpar(ftext(prefix, text_style)))
    }
    
    doc <- doc |>
      body_add_fpar(fpar(ftext(section$content, text_style))) |>
      body_add_par("")
    
    # Optional data table
    if (isTRUE(section$include_table) && !is.null(section$table_data)) {
      table_df <- as.data.frame(do.call(rbind, section$table_data$rows))
      if (!is.null(section$table_data$headers)) {
        names(table_df) <- section$table_data$headers
      }
      doc <- doc |>
        body_add_table(table_df, style = "table_template") |>
        body_add_par("")
    }
  }
  
  # Recommendations
  if (!is.null(report_spec$recommendations) && length(report_spec$recommendations) > 0) {
    doc <- doc |>
      body_add_fpar(fpar(ftext("Recommendations", heading_props))) |>
      body_add_par("")
    
    for (i in seq_along(report_spec$recommendations)) {
      doc <- doc |> body_add_par(sprintf("%d. %s", i, report_spec$recommendations[[i]]))
    }
    doc <- doc |> body_add_par("")
  }
  
  # Methodology notes
  if (!is.null(report_spec$methodology_notes)) {
    doc <- doc |>
      body_add_fpar(fpar(ftext("Methodology & Data Notes", heading_props))) |>
      body_add_fpar(fpar(ftext(report_spec$methodology_notes, subtitle_props)))
  }
  
  print(doc, target = CONFIG$output_file)
}

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# ============================================================================
# AGENT LOOP (Core agentic behavior)
# ============================================================================

run_agent_loop <- function() {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key == "") {
    stop("ANTHROPIC_API_KEY environment variable not set")
  }
  
  # System prompt that gives Claude agency
  system_prompt <- sprintf('You are an autonomous public health data analyst agent. Your job is to analyze youth marijuana use data for %s and produce an insightful report.

YOU ARE IN CONTROL. You decide:
1. What data to request (use tools to explore)
2. What patterns to investigate further
3. When you have enough information
4. What sections to include in the final report
5. How to structure and present your findings

APPROACH:
- Start by understanding what data is available
- Look at recent trends, then dig into interesting patterns
- If you see something concerning, investigate it further (e.g., get historical trend for that subgroup)
- Consider policy context that might explain changes
- Compare to national data for perspective
- Only generate the report when you have developed a complete analysis

BE THOROUGH BUT EFFICIENT:
- Request data strategically, not exhaustively
- Follow up on 1-2 key anomalies, not every detail
- After 6-8 tool calls, you likely have enough—generate the report
- Stop when you have a compelling story to tell

When ready, call generate_report with a structure that fits your findings. You might include:
- A focused executive summary
- Sections highlighting key trends you discovered  
- Deep-dives into concerning subgroups
- Policy context if relevant
- Actionable recommendations

Your analysis should reflect genuine insight, not just data regurgitation.', CONFIG$location)
  
  # Initialize conversation
  messages <- list(
    list(role = "user", content = sprintf(
      "Please analyze the youth marijuana use data for %s and create an insightful report. Start by exploring what data is available, then investigate patterns and trends. Generate the report when you have completed your analysis.",
      CONFIG$location
    ))
  )
  
  iteration <- 0
  report_generated <- FALSE
  
  while (!report_generated && iteration < CONFIG$max_iterations) {
    iteration <- iteration + 1
    cat(sprintf("\n── Agent Iteration %d ──\n", iteration))
    
    # Call Claude
    response <- POST(
      "https://api.anthropic.com/v1/messages",
      add_headers(
        "Content-Type" = "application/json",
        "x-api-key" = api_key,
        "anthropic-version" = "2023-06-01"
      ),
      body = toJSON(list(
        model = CONFIG$anthropic_model,
        max_tokens = 4096,
        system = system_prompt,
        tools = TOOLS,
        messages = messages
      ), auto_unbox = TRUE, null = "null"),
      encode = "raw"
    )
    
    if (http_error(response)) {
      error_text <- content(response, as = "text", encoding = "UTF-8")
      stop(sprintf("API error: %s", error_text))
    }
    
    result <- content(response, as = "text", encoding = "UTF-8") |>
      fromJSON(simplifyVector = FALSE)
    
    # Process response content blocks
    assistant_content <- list()
    tool_results <- list()
    
    for (block in result$content) {
      if (block$type == "text") {
        cat(sprintf("   💭 %s\n", substr(block$text, 1, 100)))
        assistant_content <- append(assistant_content, list(block))
        
      } else if (block$type == "tool_use") {
        assistant_content <- append(assistant_content, list(block))
        
        # Execute the tool
        tool_result <- execute_tool(block$name, block$input)
        
        # Check if report was generated
        if (block$name == "generate_report" && isTRUE(tool_result$success)) {
          report_generated <- TRUE
        }
        
        tool_results <- append(tool_results, list(list(
          type = "tool_result",
          tool_use_id = block$id,
          content = toJSON(tool_result, auto_unbox = TRUE)
        )))
      }
    }
    
    # Add assistant response to messages
    messages <- append(messages, list(list(
      role = "assistant",
      content = assistant_content
    )))
    
    # If there were tool calls, add results and continue loop
    if (length(tool_results) > 0) {
      messages <- append(messages, list(list(
        role = "user",
        content = tool_results
      )))
    }
    
    # Check stop condition
    if (result$stop_reason == "end_turn" && length(tool_results) == 0) {
      cat("   Agent finished without generating report\n")
      break
    }
  }
  
  if (iteration >= CONFIG$max_iterations) {
    cat("   ⚠️ Reached maximum iterations\n")
  }
  
  return(report_generated)
}

# ============================================================================
# MAIN ENTRY POINT
# ============================================================================

run_agent <- function() {
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  AGENTIC WEEKLY REPORT SYSTEM\n")
  cat("  Claude drives the analysis through autonomous tool use\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")
  
  start_time <- Sys.time()
  
  tryCatch({
    success <- run_agent_loop()
    
    elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
    
    cat("\n═══════════════════════════════════════════════════════════════════\n")
    if (success) {
      cat(sprintf("  ✅ Report generated: %s\n", CONFIG$output_file))
    } else {
      cat("  ❌ Agent did not complete report generation\n")
    }
    cat(sprintf("  ⏱️  Total time: %s seconds\n", elapsed))
    cat("═══════════════════════════════════════════════════════════════════\n\n")
    
  }, error = function(e) {
    cat(sprintf("\n❌ Agent error: %s\n", e$message))
  })
}

# Run if sourced directly
run_agent()