## timeRec
A little program to record working time, monitor it and automated print of bills.
That humble program was used in my year as a freelance biologist in 2020 and
worked well (for me :)). Maybe it helps someone else.

It is written in R. Latex is used to create the bills. Markdown could be possible as well i guess.
These two programs should be preinstalled.

### Usage
To run the time record move to the programs path and
execute in your terminal `Rscript zef_auto.R` type in your projects initials
which is stored the projects.csv (see structure) in zef_analog.R it was planned
to record set the times manually in shiny, but it doesn't work jet. However,
it can be done with excel, take care about time format.

stats_sh.R should run in rstudio. It shows a table with working times and
a graph of how much you worked for which project per day.

The billing.R file produces a pdf with a well formatted bill. It takes the data from the
timerecord files and drags them into the template.tex, after this a pdf is created.
In the template you can modify your address or format whatever you like.
For the right project data, change the variable `project`. It automatically
takes positions which are not jet included in a bill before, and set them billed.
