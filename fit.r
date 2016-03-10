library(RMySQL)
library(MASS)
library(yaml)

config_file <- file.path(Sys.getenv("HOME"), "configure.yaml")

if (file.exists(config_file)) {
    settings <- yaml.load_file(config_file)
}



mydb = dbConnect(MySQL(), 
                user=settings$user, 
                password=settings$password, 
                dbname=settings$database, 
                host=settings$host, 
                port=settings$port)


panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1)) 
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "") 
  text(0.5, 0.6, txt)

  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "") 
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "") 
  text(0.5, 0.4, txt2)
}


for (segment in c("FUVA", "FUVB")) {

    if (segment == 'FUVA') {
        lampkey = 'ldcampat'
    } else if (segment == 'FUVB') {
        lampkey = 'ldcampbt'
    }

    query <- sprintf("SELECT stims.stim1_x,stims.stim1_y,stims.stim2_x,stims.stim2_y,stims.time,spt.%s as temp
                                    FROM stims JOIN spt ON stims.rootname = spt.rootname
                                    WHERE stims.segment = '%s' AND
                                          stims.stim1_x IS NOT NULL AND
                                          stims.stim1_y IS NOT NULL AND
                                          stims.stim2_x IS NOT NULL AND
                                          stims.stim2_y IS NOT NULL AND 
                                          stims.stim1_x != -999 AND
                                          stims.stim1_y != -999 AND
                                          stims.stim2_x != -999 AND
                                          stims.stim2_y != -999;", lampkey, segment)
    print(query)
    rs <- dbSendQuery(mydb, query)
    data <- fetch(rs, n=-1)

    summary(data)
    print(length(data$time))
    #-- to file
    pdf(sprintf('sample_plot_%s.pdf', segment), width=12, height=12)
    #png(sprintf('sample_plot_%s.png', segment), width=10, height=10)
    par(pin=c(11, 10))
    p <- pairs(data, col=topo.colors(nrow(data))[rank(abs(data$temp))], upper.panel=panel.cor)
    print(p)
    dev.off()


}

