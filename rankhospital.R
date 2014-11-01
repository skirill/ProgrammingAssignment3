rankhospital <- function(state, outcome, num = "best") 
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
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    matches <- data.frame(row.names=c("Name", "Value"))
    apply(data, 1, function(row) 
        if (row[statecol] == state)
        {
            newval <- as.numeric(row[outcol])
            newname <- as.character(row[namecol])
            if (!is.na(newval))
            {
                matches <<- rbind(matches, data.frame(Name=newname, Value=newval))
            }
        }
    )
    ordered <- matches[with(matches, order(Value, Name)), ]
    
    if (nrow(ordered) == 0)
    {
        name <- NA
    }
    else if (num == 'best')
    {
        name <- as.character(ordered$Name[1])
    }
    else if (num == 'worst')
    {
        name <- as.character(ordered$Name[nrow(ordered)])
    }
    else
    {
        nr <- as.numeric(num)
        if (num <= nrow(ordered))
        {
            name <- as.character(ordered$Name[nr])
        }
        else
        {
            name <-NA
        }
    }
    name
}