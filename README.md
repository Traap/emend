The **Bootstrap** repository uses a SCRUM framework adapted to standard GitHub
tooling.  **Bootstrap** is integrated with Travis-ci.org for continuous
integration and AllanConsulting.slack.com for centralized notification.

# Installation
## Bootstrap 
```bash
$ cd $HOME
$ git clone http://github.com/Traap/bootstrap.git \
      && cd bootstrap \
      && git checkout Sprint.V1.1.2 \
      && gem build bootstrap.gemspec \
      && gem install bootstrap \
      && bootstrap --file apps/brew.yaml --verbose --nodryrun \
      && brew --version
```

## Note
This installation instructions installs my personal dotfiles in a directory named
~/git/dotfiles.

## Custom installations:
This example shows how to install TMUX.

```bash
cd ~\bootstrap
ruby ruby/bootstrap.rb --nodryrun --verbose --file apps/tmux/tmux.yaml
```

# YAML Files
## Processing Order
**bootstrap** process YAML files and their contents is the order they are
consumed.

## Include files
The **includes** link is used to process other YAML files as a logical units.
This example shows four files being included.
```
includes:
  - file:
    - name: apps/colors/colors.yaml
    - name: apps/dotfiles/dotfiles.yaml
    - name: apps/git-bash-prompt/git-bash-prompt.yaml
    - name: apps/vim/vim.yaml
```

## Symbolic Links
The **symlinks** node is used to define a map between a source and target file.
The following example creates two symbolic links.
```
symlinks:
  - symlink:
    - file: ~/git/dotfiles/bash_profile
      link: ~/.bash_profile
      directory: false

    - file: ~/git/dotfiles/bashrc
      link: ~/.bashrc
      directory: false
```

## Clone Repositories
The **repos** node is used to clone git-based repositories to your local
machine.  The example below clones Traap's dotfiles.
```
repos:
  - repo:
      url: http://github.com
          paths:
                - source: Traap/dotfiles
                        target: ~/git/dotfiles
```

## Installations
The **installation** node installs programs or runs programs on your local
machine.  The example below is use to install and compile Vim from source code.
Valid name tags are: all, darwin and linux.
```
installations:
 - os:
   - name: darwin
     command:
      - sudo: false
        program: brew
        argument: install Caskroom/cask/xquartz

   - name: any 
     command:
      - sudo: false
        program: brew
        argument: install vim --with-client-server
```

## bootstrap command line
ruby ruby/bootstrap.rb --help

Usage: bootstrap.rb [options]

Specific options:
    -n, --nodryrun                   No Dryrun
    -f, --file x,y,x MADATORY        Filename
    -v, --verbose                    Verbose
    -h, --help                       Show this message
        --version                    Show version

## --nodryrun
By default, **bootstrap** does not modify your computer.  You must explicitly
use the **--nodryrun** options to cause side effects.  The commands that would
have been executed are echoed to system out.

## --file
A comma-separated list of file names **bootstrap** is to process.

## --verbose
Echo commands to system output.

## --help
Show this message.
## --version
1.0.0 is this the current version.

# Project Management
Please refer to my [Lightweight Project Management](https://github.com/Traap/lpm)
for the project management strategy I use.
