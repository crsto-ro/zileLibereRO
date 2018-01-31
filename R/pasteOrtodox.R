# Calcul paste ortodox!
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# git push -u origin master
# pentru commit la github

pasteOrtodox <- function(anul,
                         despartitor = "-",
                         asDate = FALSE)
{
  A = anul %% 19

  B = anul %% 4

  C = anul %% 7

  D = (19 * A + 15) %% 30

  E = (2 * B + 4 * C + 6 * D + 6) %% 7

  if (asDate == TRUE)
  {
    despartitor = "-"
    if ((D + E + 4) <= 30)
    {
      return(as.Date.character(
        paste0(D + E + 4, despartitor, "04", despartitor, anul),
        "%d-%m-%Y"
      ))
    }


    return(as.Date.character(
      paste0(D + E + 4 - 30, despartitor, "05", despartitor, anul),
      "%d-%m-%Y"
    ))
  }

  else
  {
    if ((D + E + 4) <= 30)
    {
      return(paste0(D + E + 4, despartitor, "04", despartitor, anul))
    }

    return(paste0((D + E + 4 - 30), despartitor, "05", despartitor, anul))
  }
}

