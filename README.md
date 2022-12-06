# Advent of code 2022
## This is my attempt at the 2022 advent of code in haskell, and, at the same time, an attempt at setting up and maintaining a project. The plan is to add tests and set up CI runs as i go along.


## Instructions to get it running
* Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) 
* Clone repository
* From parent directory, run `stack build`
    * To run main (still WIP/deciding on what it should actually do) - `stack run`
    * To use the REPL and play around with the different functions - `stack ghci src/Utils.hs`
    * To run tests - `stack test :unit` / `stack test :integration` / `stack test`
    