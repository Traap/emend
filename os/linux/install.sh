# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
function _beforeInstall {
  echo "_beforeInstall"
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install)"

  # Linuxbrew recommends build-essential after its installation completes.
  sudo apt-get install build-essential

  # Make linuxbrew binary known to this shell.
  PATH="/HOME/.linuxbrew/bin:$PATH"
}

# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
function _install {
  echo "_install"
}

# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
function _runBootstrap {
  echo "_runBootstrap"
  ruby ruby/bootstrap.rb --nodryrun --verbose --file apps/dotfiles/dotfiles.yaml
}

# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
function _afterInstall { 
  echo "_afterInstall"
}
