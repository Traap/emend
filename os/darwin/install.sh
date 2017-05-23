# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
function _beforeInstall {
  echo "_beforeInstall"
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
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
  ruby ruby/bootstrap.rb --file ruby/apps/dotfiles/dotfiles.yaml -v
}

# ------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------
function _afterInstall {
  echo "_afterInstall"
}
