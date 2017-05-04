# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
function _beforeInstall {
  echo "_beforeInstall"
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install)"
  # sudo apt-get -y install \
  #   g++ \
  #   gcc \
  #   libc6-dev \
  #   libffi-dev \
  #   libgmp-dev \
  #   make \
  #   xz-utils \
  #   zlib1g-dev \
  #   gnupg \
  #   python
}

# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
function _install {
  echo "_install"
  brew cask install haskell-platform
}

# ------------------------------------------------------------------------------
# stack init --force
# stack setup
# stack build
# stack exec -- bootstrap
# ------------------------------------------------------------------------------
function _runBootstrap {
  echo "_runBootstrap"
}

# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
function _afterInstall { 
  echo "_afterInstall"
}
