## Code written by ChatGPT, 


# Add `categories: ["⚠️ Warning: Page Under Revision"]` to QMD YAMLs, safely.
add_warning_category <- function(root,
                                 label = "⚠️ Warning: Page Under Revision",
                                 recursive = FALSE,
                                 backup = TRUE,
                                 position = c("end","start")) {
  position <- match.arg(position)
  if (!dir.exists(root)) stop("Folder not found: ", root)
  
  # collect .qmd files
  dirs <- unique(c(root, if (recursive) list.dirs(root, recursive = TRUE, full.names = TRUE) else character()))
  dirs <- dirs[!basename(dirs) %in% c(".git",".quarto","_site","site_libs","docs","assets")]
  files <- unlist(lapply(dirs, function(d) list.files(d, "\\.qmd$", full.names = TRUE, ignore.case = TRUE)))
  if (!length(files)) { message("No .qmd files under: ", root); return(invisible()) }
  
  trim <- function(x) sub("^\\s+|\\s+$", "", x)
  dequote <- function(x) gsub('^["\']|["\']$', "", trim(x))
  esc <- function(x) gsub('"', '\\"', x, fixed = TRUE)
  
  for (f in files) {
    x <- readLines(f, warn = FALSE, encoding = "UTF-8")
    if (!length(x)) next
    
    # detect YAML front matter
    if (!grepl("^---\\s*$", x[1])) {
      # no header: create one with just categories
      new_header <- c(
        "---",
        sprintf('categories: ["%s"]', esc(label)),
        "---"
      )
      if (backup) file.copy(f, paste0(f, ".bak_", format(Sys.time(), "%Y%m%d-%H%M%S")), overwrite = TRUE)
      writeLines(c(new_header, x), f, useBytes = TRUE)
      message("Added header + category: ", f)
      next
    }
    
    end <- which(grepl("^---\\s*$", x[-1]))[1] + 1
    if (is.na(end)) next  # malformed; skip safely
    header_idx <- 1:end
    body_idx   <- (end+1):length(x)
    
    hdr <- x[2:(end-1)]
    hdr_txt <- paste(hdr, collapse = "\n")
    
    # already has the exact label?
    if (grepl(paste0("\\Q", label, "\\E"), hdr_txt, perl = TRUE)) next
    
    # find categories line (if any)
    cat_line_i <- grep("^\\s*categories\\s*:", hdr)
    if (!length(cat_line_i)) {
      # insert a new categories line just before closing ---
      hdr2 <- append(hdr, values = sprintf('categories: ["%s"]', esc(label)), after = length(hdr))
      x_new <- c("---", hdr2, "---", if (length(body_idx)) x[body_idx])
      if (backup) file.copy(f, paste0(f, ".bak_", format(Sys.time(), "%Y%m%d-%H%M%S")), overwrite = TRUE)
      writeLines(x_new, f, useBytes = TRUE)
      message("Inserted categories: ", f)
      next
    }
    
    # has categories already → try to merge
    i <- cat_line_i[1]
    line <- hdr[i]
    
    # case A: inline list  categories: [ ... ]
    if (grepl("\\[.*\\]", line)) {
      inside <- sub(".*\\[(.*)\\].*", "\\1", line)
      items  <- strsplit(inside, ",")[[1]]
      items  <- items[nzchar(items)]
      items  <- trim(items)
      items  <- dequote(items)
      if (position == "start") {
        items2 <- unique(c(label, items))
      } else {
        items2 <- unique(c(items, label))
      }
      hdr[i] <- sprintf('categories: ["%s"]', paste(esc(items2), collapse = '", "'))
    } else {
      # case B: block list under categories:
      # find the block of `- ...` lines that belong to categories
      base_indent <- nchar(sub("^([ ]*).*$", "\\1", line))
      j <- i + 1
      # first item indent (fallback to base + 2)
      if (j <= length(hdr) && grepl("^\\s*-\\s*", hdr[j])) {
        item_indent <- nchar(sub("^([ ]*).*$", "\\1", hdr[j]))
      } else {
        item_indent <- base_indent + 2
      }
      # gather item lines
      item_idx <- integer(0)
      k <- j
      while (k <= length(hdr) && grepl(sprintf("^\\s{%d,}-\\s*", item_indent), hdr[k])) {
        item_idx <- c(item_idx, k); k <- k + 1
      }
      # read existing items
      items <- character(0)
      if (length(item_idx)) {
        items <- trim(sub("^\\s*-\\s*", "", hdr[item_idx]))
        items <- dequote(items)
      }
      if (!(label %in% items)) {
        new_item <- paste0(strrep(" ", item_indent), '- "', esc(label), '"')
        if (position == "start" && length(item_idx)) {
          hdr <- append(hdr, values = new_item, after = i)
        } else if (position == "start" && !length(item_idx)) {
          hdr <- append(hdr, values = new_item, after = i)
        } else {
          # end
          after_pos <- if (length(item_idx)) max(item_idx) else i
          hdr <- append(hdr, values = new_item, after = after_pos)
        }
      } else {
        next
      }
    }
    
    # write back
    x_new <- c("---", hdr, "---", if (length(body_idx)) x[body_idx])
    if (backup) file.copy(f, paste0(f, ".bak_", format(Sys.time(), "%Y%m%d-%H%M%S")), overwrite = TRUE)
    writeLines(x_new, f, useBytes = TRUE)
    message("Updated categories: ", f)
  }
}

# Usage:
# hw (flat)
#add_warning_category("hw", recursive = FALSE, position = "start")
# labs (recursive)
#add_warning_category("labs", recursive = TRUE, position = "end")