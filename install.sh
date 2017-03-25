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
main() {
  parseOptions $@
  if [ $helpFlag == 1 -o $errorFlag == 1 ]; then
    showHelp
  elif [ $allFlag == 1 ]; then
    if [[ ${OSTYPE} =~ "linux" ]]; then
      runFunction aptUpdate 
      runFunction aptInstall
      runFunction installVim
      runFunction installHaskell 
      runFunction installTmux
      runFunction bootstrapPersonalization
    elif [[ ${OSTYPE} =~ "darwin" ]]; then
      runFunction installHomebrew
      runFunction installHaskell 
      runFunction bootstrapPersonalization
    else
      echo "${OSTYPE} is not supported.  It probably never will be either!"
    fi  
  else
    runFunction $fctToRun
  fi
}

# ------------------------------------------------------------------------------
# Show the help text for the script and then terminate execution.
# ------------------------------------------------------------------------------
function showHelp {
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
   echo "             installHaskell"
   echo "             installHomeBrew"
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
function parseOptions {
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
runFunction() {
  cd ${HMST_ROOT}
  echo "*** Entering runFunction with" $1
  time $1
  echo "*** Exiting runFuction with" $1
  cd ${HMST_ROOT}
}

# ------------------------------------------------------------------------------
# A function to apt-get update 
# ------------------------------------------------------------------------------
aptUpdate() {
  sudo apt-get -y update 
}

# ------------------------------------------------------------------------------
# A function to install Vim 8.0 
# ------------------------------------------------------------------------------
installVim() {
  sudo apt-get -y install \
    libncurses5-dev \
    libgnome2-dev \
    libgnomeui-dev \
    libgtk2.0-dev \
    libatk1.0-dev \
    libbonoboui2-dev \
    libcairo2-dev \
    libx11-dev \
    libxpm-dev \
    libxt-dev

  git clone http://GitHub.com/vim/vim ${HOME}/vim

  cd ${HOME}vim

  ./configure --with-features=huge --enable-cscope --enable-gui=gnome2

  cd ${HOME}/vim/src

  sudo make install

  cd ${HOME}/bootstrap
}

# ------------------------------------------------------------------------------
# A function to install haskell stack.
# ------------------------------------------------------------------------------
aptInstall() {
  sudo apt-get -y install \
    g++ \
    gcc \
    libc6-dev \
    libffi-dev \
    libgmp-dev \
    make \
    xz-utils \
    zlib1g-dev \
    git \
    gnupg \
    python
}

# ------------------------------------------------------------------------------
# A function to install Homebrew.
# ------------------------------------------------------------------------------
installHomebrew() {
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
}

# ------------------------------------------------------------------------------
# A function to install haskell.
# ------------------------------------------------------------------------------
installHaskell() {
  if [[ ${OSTYPE} =~ "linux" ]]; then
    curl -sSL https://get.haskellstack.org/ | sh
  elif [[ ${OSTYPE} =~ "darwin" ]]; then
     # haskell-stack never downloaded the Hackage index.
     # brew install haskell-stack
     brew cask install haskell-platform
  else
   echo "${OSTYPE} is not installed.  Program exiting."
  fi
}

# ------------------------------------------------------------------------------
# A function to install tmux
# ------------------------------------------------------------------------------
installTmux() {
  sudo apt-get install tmux 
}

# ------------------------------------------------------------------------------
# A function to bootstrap your personalization.  
# ------------------------------------------------------------------------------
bootstrapPersonalization() {
  cd ${HOME}/bootstrap
  stack init --force
  stack setup
  stack build
  stack exec -- bootstrap
}

# ------------------------------------------------------------------------------
# Kick start this script.
# ------------------------------------------------------------------------------
main $@
