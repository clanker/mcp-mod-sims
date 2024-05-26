Code that generates the output for the two MCP-MOD simulation studies. This code is open source in the public domain and can be freely used.

Guide to the files in this directory:

* The file sim-doses.R has all the code to generate the four plots (in Simulation Study #1).
  * Uses sim-doses-2.Rdata that is generated in the code itself (but it takes an hour to run the 30000 simulation trials).
* The file app.R runs the Shiny app.
  * Uses save-2500.Rdata and rmse-data-2500.Rdata.
* The file second-sim.R generates all the data for the two data files for app.R.
