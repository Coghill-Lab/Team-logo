# Load packages
library(tidyverse)
library(ggrepel)

## original post here: https://sdgamboa.github.io/post/2020_volcano/
## Data from article: https://www.nature.com/articles/s41598-018-32904-2

# A short function for outputting the tables
knitr_table <- function(x) {
    x %>% 
        knitr::kable(format = "html", digits = Inf, 
                     format.args = list(big.mark = ",")) %>%
        kableExtra::kable_styling(font_size = 15)
}



# Import data
data <- read_tsv("https://raw.githubusercontent.com/sdgamboa/misc_datasets/master/L0_vs_L20.tsv")
dim(data)

if (interactive()) {
    x_seed <- sample(1:10**4, 1)
    print(paste0("seed is: ", x_seed))
    set.seed(x_seed)
}

# set.seed(1751)
## set.seed(5899)
# set.seed(4668)
# set.seed(989)
# set.seed(6701)
set.seed(1671)


# p1 <-
    data %>%
    filter(-log(FDR,10) > (0.25*logFC**2 -1)) %>% 
    # filter(FDR > 0) %>% 
    slice_sample(n = 1000, weight_by = -log(FDR,10)) %>%
    slice_sample(n = 160, weight_by = -log(FDR,10)) %>%
    ggplot(aes(logFC, -log(FDR,10))) + # -log10 conversion  
    geom_point(color = "white" , size = 7, alpha = 1) +
    # xlab(expression("log"[2]*"FC")) + 
    xlab("Fold Change") +
    ylab(expression("-log"[10]*"(P)")) +
    xlim(-5, 5) + 
    ylim(0, 20) +
    theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        axis.title = element_text(colour = "white", size = 50),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "white", size = 3)
    )

    
p1


ggsave(
    filename = "volcano-plot_8x6.png", 
    plot = p1, 
    width = 8,
    height = 6,
    # path = "./.."
    bg = "transparent"
)


set.seed(1671)


p2 <-
    data %>%
    filter(-log(FDR,10) > (0.25*logFC**2 -1)) %>% 
    # filter(FDR > 0) %>% 
    slice_sample(n = 1000, weight_by = -log(FDR,10)) %>%
    slice_sample(n = 160, weight_by = -log(FDR,10)) %>%
    ggplot(aes(logFC, -log(FDR,10))) + # -log10 conversion  
    geom_point(color = "white" , size = 7, alpha = 1) +
    # xlab(expression("log"[2]*"FC")) + 
    xlab("Fold Change") +
    ylab(expression("-log"[10]*"(P)")) +
    xlim(-5, 5) + 
    ylim(0, 20) +
    theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()
    )


p2


ggsave(
    filename = "volcano-plot_8x6_points-only.png", 
    plot = p2, 
    width = 8,
    height = 6,
    # path = "./.."
    bg = "transparent"
)



###########################################################
    
x <- 1:10

df <- data.frame(x)

ggplot(df, aes(x)) +
    stat_function(fun = function(x) 0.3*x**2 -1) +
    xlim(-6, 6) + 
    ylim(0, 20)
