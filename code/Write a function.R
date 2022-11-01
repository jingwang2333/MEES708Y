# if else
temp = -5

if(temp < 0) {
    print("It's freezing!")
    if(temp < 32) {
        print("And kids don't go to school.")
    }
} else {
    print("It's not freezing.")
}

checkweather(-20)
checkweather <- function(x) {
    # x is temperature, degrees C
    if (x < 0) {
        print("It's freezing!")
        if (x < 32) {
            print("And kids don't go to school.")
        }
    } else {
        print("It's not freezing.")
    }
}
checkweather2 <- function(x) {
    # x is temperature, degrees C
    if (FALSE){
        x = 5
    }
    xf <- 32 + 1.8*x
    print (paste0("In Farenheit it is" , xf, "."))
    if (x < 0) {
        print("It's freezing!")
        if (x < 32) {
            print("And kids don't go to school.")
        }
    } else {
        print("It's not freezing.")
    }
    return(xf)
}
checkweather2(50)
temF = checkweather2(50)


10 %% 3 # the result will be the residual


is.leap <- function(x) {
    if ((x %% 4) == 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
    if ((x/100) == 0) {
        return(TRUE)
    }else {
        return(FALSE)
    }
    if((x %% 400) == 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }}

is.leap(2400)
is.leap(2023)

is.leap <- function(x) {
    if( (x %% 4) == 0) {
        print("This is a leap year.")
        return(TRUE)
    } else {
        print("This is a usual year")
        return(FALSE)
    }
    if ((x/100) == 0) {
        return(TRUE)
    }else {
        return(FALSE)
    }
    if((x %% 400) == 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
is.leap(2011)

is.leap <- function(year) {
    if ((year %% 4) == 0) {
        if ((year %% 100) == 0) {
            if ((year %% 400) == 0) {
                res = TRUE
            } else {
                res = FALSE
            }
        } else {
            res = TRUE
        }
    } else {
        res = FALSE
    }
    return(res)
}
