# ggquik
### Quick Plotting ggplot2 Wrapper

Using ggplot2, create quick bar, line, and bullet plots using standardized layouts and Red Hat color schemes.

In order to install ggquik:

``` R
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("brascoball/ggquik") 
```

## Contents

This package has three main functions: `quik_bars()`, `quik_lines()`, and `quik_bullets()`:

### quik_bars()
This function uses ggplot2 to quickly create a bar plot using ggquik plot standards. The function only requires a dataset, `dimension`, `measure`, and `bar_groups`:

``` R
data(morley)
ggq <- quik_bars(morley, dimension = 'Run', measure = 'Speed', bar_groups = 'Expt')
ggq
```

![](man/figures/README-ggquik_bars1.png)

ggquik also has a very simple theme that is very customizable:

``` R
quik_theme(ggq)
```

![](man/figures/README-ggquik_bars2.png)

And, if you want to keep the axis tick labels (`axis.text`) titles (`axis.title`):

``` R
quik_theme(ggq, axis.text = c('y'), axis.title = c('x', 'y'))
```

![](man/figures/README-ggquik_bars3.png)

### quik_lines()
This function uses ggplot2 to quickly create a line plot using ggquik plot standards. The function only requires a dataset, `dimension`, `measure`, and `line_groups`. Using the same example, since there are a LOT of labels, we'll set the `label_size` to 0, and also use the same theme parameters as 

``` R
data(morley)
ggq <- quik_lines(morley, dimension = 'Run', measure = 'Speed', line_groups = 'Expt', label_size = 0)
quik_theme(ggq, axis.text = 'y', axis.title = c('x', 'y'))
```

![](man/figures/README-ggquik_lines1.png)


