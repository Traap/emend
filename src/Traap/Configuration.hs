-- | Copyright (c) Gary Allan Howard aka Traap.
-- License BSD-3-Clause

module Traap.Configuration
  (symlinks
  ,repos
  ,github
  )where

import Traap.DataTypes

-- -----------------------------------------------------------------------------
-- | The symlinks function creates a list of SymLinks.  Each SymLink represents
-- a named symbolic link and file or directory target that is symbolically
-- linked.
symlinks :: [SymLink]
symlinks =
  [SYMLINK{sym="bash_profile",     src=".",     flag=True}
  ,SYMLINK{sym="bashrc",           src=".",     flag=True}
  ,SYMLINK{sym="gitconfig",        src=".",     flag=True}
  ,SYMLINK{sym="gitignore_global", src=".",     flag=True}
  ,SYMLINK{sym="gvimrc",           src=".",     flag=True}
  ,SYMLINK{sym="inputrc",          src=".",     flag=True}
  ,SYMLINK{sym="ssh",              src=".",     flag=False}
  ,SYMLINK{sym="config",           src=".ssh/", flag=True}
  ,SYMLINK{sym="tmux",             src=".",     flag=False}
  ,SYMLINK{sym="tmux.conf",        src=".",     flag=True}
  ,SYMLINK{sym="vim",              src=".",     flag=False}
  ,SYMLINK{sym="vimrc",            src=".",     flag=True}
  ,SYMLINK{sym="vimrc_background", src=".",     flag=True}
  ]

-- -----------------------------------------------------------------------------
-- | The repos function creates a list of Repos.  Each Repo represents
-- a repository cloned from GitHub.com.
repos :: String -> [Repo]
repos s =
  [REPO{tdir="dotfiles"
       ,url=[URL{loc=s ++ "Traap/dotfiles", here=False}]
       }
  ,REPO
       {tdir="dotfiles/vim/autoload"
       ,url=[URL{loc=s ++ "junegunn/vim-plug", here=False}]
       }
  ,REPO
       {tdir="color"
       ,url=[URL{loc=s ++ "chriskempson/base16-gnome-terminal", here=False}
            ,URL{loc=s ++ "chriskempson/base16-iterm2", here=False}
            ,URL{loc=s ++ "chriskempson/base16-shell", here=False}
            ]
       }
  ,REPO
       {tdir="ssh"
       ,url=[URL{loc=s ++ "Traap/ssh", here=True}]
       }
  ,REPO
       {tdir="tmux"
       ,url=[URL{loc=s ++ "Traap/tmux", here=True}]
       }
  ,REPO
       {tdir="tmux/plugins/tpm"
       ,url=[URL{loc=s ++ "tmux-plugins/tpm", here=True}]
       }
  ]

-- -----------------------------------------------------------------------------
-- | A string representing git clone and Github URL.
-- Github is perpended to all items in repos.
github :: String
github = "git clone http://github.com/"


-- -----------------------------------------------------------------------------
-- | OSX commands ran prior to bootstrapping symbolic links and cloning repos. 
preosx:: [Command]
preosx = 
  [COMMAND{prg="/usr/bin/ruby", arg=["-e", "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"]}
  ,COMMAND{prg="brew", arg=["install", "bash-git-prompt"]}
  ,COMMAND{prg="brew", arg=["install", "haskell-stack"]}
  ,COMMAND{prg="brew", arg=["install", "mutt"]}
  ,COMMAND{prg="brew", arg=["install", "vim", "--with-client-server"]}
  ,COMMAND{prg="brew", arg=["install", "tmux"]}
  ,COMMAND{prg="ls", arg=["-la", "foobar"]}
  ]


