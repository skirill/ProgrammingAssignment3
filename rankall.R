rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    namecol <- 2
    statecol <- 7
    states <- sort(unique(data[,statecol]))
    
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
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    matches <- data.frame(row.names=c('hospital', 'state', 'value'))
    apply(data, 1, function(row) 
        {
            newhospital <- as.character(row[namecol])
            newstate <- as.character(row[statecol])
            newvalue <- as.numeric(row[outcol])
            if (!is.na(newvalue))
            {
                matches <<- rbind(matches, data.frame(hospital=newhospital, state=newstate, value=newvalue))
            }
        }
    )
    ordered <- matches[with(matches, order(value, hospital)), ]
    
    res <- data.frame(row.names=c('hospital', 'state'))
    
    lapply(states, function(state)
        {
            filtered <- ordered[ordered$state==state,]
            
            if (nrow(filtered) > 1)
            {
                for (i in 1:(nrow(filtered)-1))
                {
                    for (j in (i+1):nrow(filtered))
                    {
                        if (filtered$value[i] == filtered$value[j])
                        {
                            ni <- as.character(filtered$hospital[i])
                            nj <- as.character(filtered$hospital[j])
                            
                            if (ni > nj)
                            {
                                x <- filtered[i,]
                                filtered[i,] <- filtered[j,]
                                filtered[j,] <- x
                            }
                        }
                    }
                }
            }
            
            if (nrow(filtered) == 0)
            {
                name <- NA
            }
            else if (num == 'best')
            {
                name <- as.character(filtered$hospital[1])
            }
            else if (num == 'worst')
            {
                name <- as.character(filtered$hospital[nrow(filtered)])
            }
            else
            {
                nr <- as.numeric(num)
                if (num <= nrow(filtered))
                {
                    name <- as.character(filtered$hospital[nr])
                }
                else
                {
                    name <-NA
                }
            }
            
            res <<- rbind(res, data.frame(hospital=name, state=state))
        }
    )

    res
}