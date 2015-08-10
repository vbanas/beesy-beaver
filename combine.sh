#!/bin/bash

args_length=$(($#-1))
input_files=${@:1:args_length}
output_file=${@: -1}


sbcl --eval "(progn (load \"init.lisp\"))" --eval "(bb::combine-solutions (bb::get-file-names \"$input_files\") #p\"$output_file\")" --eval "(quit)"
