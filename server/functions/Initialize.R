Initialize <- function(install_packages,load_packages){  
    if (install_packages == 1) {
        install.packages("data.table")
        install.packages("plotrix")
        install.packages("RSQLite")
        install.packages("grid")
        install.packages("gridExtra")
        install.packages("lattice")
        install.packages("zoo")
        install.packages("httpRequest")
        update.packages()
    }
    return(NULL)
}
