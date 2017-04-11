#!/bin/bash

# ------------------------------------------------------------------------------
# Write an overview.
#
# Prerequisites:
# ------------------------------------------------------------------------------
# Flags
allFlag=0;    # Run all functions.
debugFlag=0;  # Dump environment variables when true.
errorFlag=0;  # A parseOptions error occured.
helpFlag=0;   # Show help.
fctTorun="";  # The function the uses requested to run.

# ------------------------------------------------------------------------------
# Orchestrate the show.
# ------------------------------------------------------------------------------
main {
  parseOptions $@
  if [ $helpFlag == 1 -o $errorFlag == 1 ]; then
    showHelp
  elif [ $allFlag == 1 ]; then
    if [[ ${OSTYPE} =~ "linux" ]]; then
      source linux/install.sh
    elif [[ ${OSTYPE} =~ "darwin" ]]; then
      source darwin/install.sh
    else
      exitProgram
    fi
    runFunction orchestrate
  else
    runFunction $fctToRun
  fi
}
# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
orchestrate {
    funFunction beforeInstall
    runFunction install
    runFunction runBootstrap
    runFunction afterInstall
}

# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
beforeInstall {
  runFunction _beforeInstall
}

# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
install {
  runFunction _install
}

# ------------------------------------------------------------------------------
# A function to bootstrap your personalization.  
# ------------------------------------------------------------------------------
runBootstrap {
  cd ${HOME}/bootstrap
  _runBootstrap
}

# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
afterInstall {
  runFunction _afterInstall
}

# ------------------------------------------------------------------------------
# Show the help text for the script and then terminate execution.
# ------------------------------------------------------------------------------
showHelp {
   echo
   echo "Usage: install [OPTIONS]"
   echo "Run install to bootstrap a configurable bootstraper."
   echo
   echo "OPTIONS"
   echo "  --all           Install all programs."
   echo
   echo "  --debug         Echo enviroment variables to sysout."
   echo
   echo "  --function=     Internal function name to run."
   echo "             beforeInstall"
   echo "             install"
   echo "             afterInstall"
   echo
   echo "  --help          Display this help message."
}

# ------------------------------------------------------------------------------
# Parse the command line options, setting the global variables debug and fctToRun
# if those flags are encountered.   If the help flag is encountered, or if the
# command line option is not recognized, then display the help information and
# terminate gracefully.
#
# Arguments:
# A non-empty array containing the command line options.
# ------------------------------------------------------------------------------
parseOptions {
    # Iterate through the list of options.
    for var in "$@"
    do
      case $var in
        --all)
          allFlag=1
          ;;
        --debug)
          debugFlag=1
          ;;
        --function=*)
          fctToRun="${var#*=}"
          shift
          ;;
        --help)
          helpFlag=1
          ;;
        *)
          echo "Unknown argument $var"
          echo
          errorFlag=1
          ;;
      esac
    done
}

# ------------------------------------------------------------------------------
# Run a function.
#
# #Arguments:
# $1 is the function to run
# ------------------------------------------------------------------------------
runFunction {
  cd ${HMST_ROOT}
  echo "*** Entering runFunction with" $1
  time $1
  echo "*** Exiting runFuction with" $1
  cd ${HMST_ROOT}
}

# ------------------------------------------------------------------------------
# A function to exit this program and print a message. 
# ------------------------------------------------------------------------------
exitProgram {
  echo "${OSTYPE} is not installed.  Program exiting."
  exit;
}



# ------------------------------------------------------------------------------
# Kick start this script.
# ------------------------------------------------------------------------------
main $@
