best <- function(state, outcome)
{
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    namecol <- 2
    statecol <- 7
    states <- unique(data[,statecol])
    
    ## Check that state and outcome are valid
    if (outcome == 'heart attack')
    {
        outcol <- 11
    }
    else if (outcome == 'heart failure')
    {
        outcol <- 17
    }
    else if (outcome == 'pneumonia')
    {
        outcol <- 23
    }
    else
    {
        stop('invalid outcome')
    }
    data[, outcol] <- suppressWarnings(as.numeric(data[, outcol]))
    
    if (!state %in% states)
    {
        stop('invalid state')        
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    name <- NA
    lowest <- NA
    apply(data, 1, function(row) 
        if (row[statecol] == state)
        {
            outval <- as.numeric(row[outcol])
            newname <- as.character(row[namecol])
            if (!is.na(outval))
            {
                found <- F
                if (is.na(lowest))
                    found <-T
                else if (outval < lowest || outval == lowest && newname < name)
                    found <-T
                if (found)
                {
                    name <<- newname
                    lowest <<-outval
                }
            }
        }
    )
    #print(lowest)
    name
}
