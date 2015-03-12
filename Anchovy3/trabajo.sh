#!/bin/bash
module load R/3.1.2
 module load Gadget/2.2.00
        Rargs="--no-save -q"
        R $Rargs < ./gadgetanchovy.r



