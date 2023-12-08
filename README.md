# Antarctic_mixotrophy
All visualizations and analyses from the Mixotrophy in Antarctica Manuscript

I have found a script that describes how to do model selection using adonis output by kdyson, however, it was broken with the newer version of R.
#github link: https://github.com/kdyson/R_Scripts - for the original location of the package/script used in the this project.
I have provided a fixed version that now works with the newer version of R. It was incompatible with how tibbles/dfs structures have "changed" so that the
for() loop was broken. It needed an addition datastructure conversion step to work properly.
