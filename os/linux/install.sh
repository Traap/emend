# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
function _beforeInstall {
  echo "_beforeInstall"
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install)"
  sudo apt-get install build-essential
  PATH="$HOME/.linuxbrew/bin:$PATH"
  brew install gcc
}

# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
function _install {
  echo "_install"
  brew install haskell-stack
  stack init --force
  stack build
}

# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
function _runBootstrap {
  echo "_runBootstrap"
  stack exec -- bootstrap
}

# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
function _afterInstall { 
  echo "_afterInstall"
}
