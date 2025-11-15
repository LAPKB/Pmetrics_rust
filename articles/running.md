# Running model fits

## 1 Fitting data to models

In this section, we suggest a workflow to help you maintain organized
modeling projects.

### 1.1 Setting up a Pmetrics project

Legacy  
R6

When beginning a new modeling project, it is convenient to use the
command
[`PMtree()`](https://lapkb.github.io/Pmetrics_rust/reference/PMtree.md).
This command will set up a new directory in the current working
directory named whatever you have included as the “project name”.

``` r
PMtree("DrugX")
```

In the above example, a directory called “DrugX” will be created in the
current working directory in R, which you can check with the `getwd`
function. Beneath the new DrugX directory, several subdirectories will
be also created.

- **Rscript** contains a skeleton R script to begin Pmetrics runs in the
  new project.
- **Runs** should contain all files required for a run (described next)
  and it will also contain the resulting numerically ordered run
  directories created after each Pmetrics NPAG or IT2B run.
- **Sim** can contain any files related to simulations
- **src** is a repository for original and manipulated source data files

You are free to edit this directory tree structure as you please, or
make your own entirely.

### 1.2 Getting the required inputs to run Pmetrics

R6

There is a full tutorial encoded inside Pmetrics to teach new users the
basic functionality of the whole package. To start that tutorial in R
type:

``` r
library(Pmetrics)
PM_tutorial()
```

Follow the instructions prompted in the terminal.

To setup a R6 Pmetrics run, you must provide
[`PM_data()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
and
[`PM_model()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md)
objects. Once created, it does not matter where the files (if there are
any) are located on your system because Pmetrics will use those objects,
not the files.

To bring these together, use the `PM_fit$new()` object creator. It only
needs two arguments: the name of the data file in the working directory
or in memory loaded via the Legacy
[`PMreadMatrix()`](https://lapkb.github.io/Pmetrics_rust/reference/PMreadMatrix.md)and
a model object.
[`PM_fit()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_fit.md)
will accept a model object created by `PM_model` or the name of a model
file in Legacy format and in the working directory.

``` r
# Example 1 - data and model objects
dat <- PM_data$new("data.csv")
mod1 <- PM_model$new(list(...))
fit1 <- PM_fit$new(dat, mod1)

# Example 2 - data file and PM_model object
fit2 <- PM_fit$new(data = "data.csv", model = mod1)

# Example 3 - data object and model file
PMdata <- PMreadMatrix("data.csv")
fit3 <- PM_fit$new(data = PMdata, model = "model.txt")
```

The fit object also contains a couple of methods attached to it.

``` r
fit1$check() # This will check the consistency between the model and the data

fit2$save("fit2.rds") # This will save the fit to a file in your working directory

fit2_again <- PM_fit$load("fit2.rds") # This will load the fit again from disk
```

To see the full list of methods available see
[`?PM_fit`](https://lapkb.github.io/Pmetrics_rust/reference/PM_fit.md).

Legacy

When you wish to execute a Pmetrics run, you must ensure that
appropriate Pmetrics model.txt and data.csv files are in the working
directory, i.e. the Runs subdirectory of the project directory. R can be
used to help prepare the data.csv file by importing and manipulating
spreadsheets (e.g. `read.csv`). The Pmetrics function
[`PMcheck()`](https://lapkb.github.io/Pmetrics_rust/reference/PMcheck.md)
can be used to check a .csv file or an R dataframe that is to be saved
as a Pmetrics data.csv file for errors. It can also check a model file
for errors in the context of a datafile, e.g. covariates that do not
match. `PMcheck(...,fix=T)` attempts to automatically rid data files of
errors. The function `PMwriteMatrix` can be used to write the R data
object in the correct format for use by IT2B, NPAG, or the Simulator.

### 1.3 Running the model to fit the data

R6

Once the
[`PM_fit()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_fit.md)
object is created the `$run()` method will execute the fit. Arguments
for this method can be found in the help for
[`NPrun()`](https://lapkb.github.io/Pmetrics_rust/reference/NPrun.md)
and `ITrun()`.

``` r
# default run parameters
fit3$run()

# change the cycle number from default 100
fit3$run(cycles = 500)

# change the engine from default NPAG
fit3$run(engine = "IT2B")
```

Legacy

As you will see in the skeleton R script made by
[`PMtree()`](https://lapkb.github.io/Pmetrics_rust/reference/PMtree.md)
and placed in the Rscript subdirectory, if this is a first-time run, the
R commands to run IT2B or NPAG are as follows. Recall that the “#”
character is a comment character.

``` r
library(Pmetrics)
# Run 1 - add your run description here
setwd("working directory")
NPrun() # for NPAG or ITrun() for IT2B
```

The first line will load the Pmetrics library of functions. The second
line sets the working directory to the specified path. The third line
generates the batch file to run NPAG or IT2B and saves it to the working
directory.

**NOTE**: On Mac systems, the batch file will be automatically launched
in a Terminal window. On Windows systems prior to version 1.9, the batch
file must be launched manually by double clicking the *npscript.bat* or
*itscript.bat* file in the working directory. As of version 1.9, Windows
users no longer need to do this.

`ITrun()` and
[`NPrun()`](https://lapkb.github.io/Pmetrics_rust/reference/NPrun.md)
are described in full detail via their help commands in R. At minimum,
they require a data file and a model file. If the default names of
“data.csv” and “model.txt” are used, they may be called with no
arguments. Again, the data and model files must be in the current
working directory, usually the Runs folder.

Both functions return the full path of the output directory to the
clipboard. By default, runs are placed in folders numbered sequentially,
beginning with “1”.

When you wish to execute a Pmetrics run, you must ensure that both of
the appropriate Pmetrics data.csv and model.txt files are in the working
directory, i.e. the Runs subdirectory of the project directory. The
names are supplied as arguments to `NPrun`, `ITrun`, and `ERRrun`. A
shorthand notation is to supply the number of a previous run for either
the data, model or both files so that you do not have to manually copy
them into the working directory.

``` r
# Using default names data.csv and model.txt
NPrun()

# Using custom names
ITrun(model = "model1.txt", data = "mydata.csv")

# Grab data from run 1 and use default model.txt
NPrun(data = 1)

# Use model and data from run 2 and continue where run 2 ended
NPrun(data = 2, model = 2, prior = 2, cycles = 1000)
```

You can also download sample data and scripts from the [Pmetrics
downloads](http://lapk.org/pmetrics.php) section of our website. Edit
prior versions of model files to make new model files.

## 2 Loading results after a completed run

R6

After the execution is done, you can load the output into R using
[`PM_load()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md).
Note the underscore “\_” to distinguish this function from the Legacy
[`PMload()`](https://lapkb.github.io/Pmetrics_rust/reference/PMload.md).
The argument to the function is the run number which you wish to load,
corresponding to a folder with the same number in your **Runs** folder
(if you made one).

``` r
my_run <- PM_load(1)
```

This creates a
[`PM_result()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
object called `my_run`. Detailed information about the different
elements contained in the result object can be accessed via
[`?PM_result`](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
or by typing the result object into the terminal.

After that, that object can be used to access the different elements of
the results, for example:

``` r
exRes <- PM_load(1)
exRes$final$plot() # see ?plot.PM_final
exRes$op$plot(type = "pop") # see ?plot.PM_op,
exRes$data$plot(
  overlay = F,
  line = list(pred = list(exRes$post, color = "green"), join = F),
  marker = list(symbol = "diamond-open", color = "blue"),
  log = T
) # see ?plot.PM_data
```

Legacy

Now the output of IT2B or NPAG needs to be loaded into R, so the next
command does this.

`PMload(run_number)`

Details of these commands and what is loaded are described in the R
documentation
[`?PMload`](https://lapkb.github.io/Pmetrics_rust/reference/PMload.md).
The **run_number** should be included within the parentheses to be
appended to the names of loaded R objects, allowing for comparison
between runs, e.g. `PMload(1)`. Finally, at this point other Pmetrics
commands can be added to the script to process the data, such as the
following.

``` r
plot(final.1)
plot(cycle.1)
plot(op.1, type = "pop") # or plot(op.1$pop1)
plot(op.1) # default is to plot posterior predictions for output 1
plot(op.1, type = "pop", resid = T)
```

Of course, the full power of R can be used in scripts to analyze data,
but these simple statements serve as examples.

If you do not use the `PMtree` structure, we suggest that the R script
for a particular project be saved into a folder called “Rscript” or some
other meaningful name in the working directory. Folders are not be moved
by the batch file. Within the script, number runs sequentially and use
comments liberally to distinguish runs, as shown below.

``` r
library(Pmetrics)
# Run 1 - Ka, Kel, V, all subjects
setwd("working directory")
NPrun() # assumes model="model.txt" and data="data.csv"
PMload(1)
```

Remember in R that the command `example(function)` will provide examples
for the specified function. Most Pmetrics functions have examples.
