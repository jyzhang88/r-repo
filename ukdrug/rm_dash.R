rm_dash = function(x) {
      if (!is.character(x)) {
            stop("Error: x must be a string.")
      } else {
            # Remove dashes from character and replace with space
            gsub("-", " ", x)
      }
}