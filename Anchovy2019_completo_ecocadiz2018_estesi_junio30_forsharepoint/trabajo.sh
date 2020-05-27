#!/bin/bash
module load gcc/6.4.0 
module load R/3.5.1
module load gadget 
        Rargs="--no-save -q"
        R $Rargs < ./Runner.r



