Rscript plot.r --files $(python generate_commands.py --n 1 --random) --op boxplot
xdg-open Rplots.pdf
