# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
function _beforeInstall {
  echo "_beforeInstall"
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install)"

  # Linuxbrew recommends build-essential after its installation completes.
  sudo apt-get install build-essential

  # Make linuxbrew binary known to this shell.
  PATH="$HOME/.linuxbrew/bin:$PATH"

  # These applications are needed install haskell-stack and build bootstrap 
  # using stack build.
  sudo apt-get -y install \
    g++ \
    gcc \
    libc-dev \
    libffi-dev \
    libgmp-dev \
    make \
    xy-utils \
    zlib1g-dev \
    gnupg \
    python
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
