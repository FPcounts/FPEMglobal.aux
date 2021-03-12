###-----------------------------------------------------------------------------
### * Helpers for Commonly Used Messages

## Used when 'verbose' is TRUE and loading some .rda files.
msg_loaded <- function(full_path, ob_name) {
    message("Loaded '", full_path, "'. \n.. Object is '",
                    paste(ob_name, collapse = " "), "'. \n.. Keeping '", ob_name[1],
            "'.")
    }
