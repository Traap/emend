u!/usr/local/bin/ruby
otstrap.rb --file ../apps/vim/vim.yaml

# Copyright (c) Gary Allan Howard aka Traap.
# License BSD-3-Clause

require 'open3'

class ShellError < StandardError; end

#actual function:
def system_quietly(*cmd)
  exit_status=nil
  err=nil
  out=nil
  puts cmd
  Open3.popen3(*cmd) do |stdin, stdout, stderr, wait_thread|
    err = stderr.gets(nil)
    out = stdout.gets(nil)
    [stdin, stdout, stderr].each{|stream| stream.send('close')}
    exit_status = wait_thread.value
  end
  if exit_status.to_i > 0
    err = err.chomp if err
    raise ShellError, err
  elsif out
    return out.chomp
  else
    return true
  end
end

#calling it:
begin
  # puts system_quietly('which', 'ruby')
  # puts system_quietly('ls', '-la')
  # puts system_quietly('git', 'clone', 'git@github.com:Traap/dotfiles.git')
  # puts system_quietly('ls', '-la', 'dotfiles/')
  # puts system_quietly('rm', '-frv', 'dotfiles')
  # puts system_quietly('ls', '-la')
  puts system_quietly('ifconfig | grep 192')

rescue ShellError
  abort "Looks like you don't have the `ruby` command. Odd."
end
