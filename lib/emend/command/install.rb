# frozen_string_literal: true

module Emend
  # This class is used to install a program.
  class Install < Command
    def install_artifact
      puts 'Installing programs'
      @data.each do |node|
        node['os'].each do |osname|
          next unless install_on_this_os? osname['name']

          do_each_command osname
        end
      end
      puts ''
    end

    def do_each_command(osname)
      osname['command'].each do |c|
        sudo = sudo_or_nil c
        @command = "#{sudo}#{c['program']} #{c['argument']}"
        do_command false
      end
    end

    def sudo_or_nil(cmd)
      return nil unless cmd['sudo']

      %w[cygwin mingw32].include?(RbConfig::CONFIG['host_os']) ? nil : 'sudo '
    end

    def install_on_this_os?(opsys)
      return true if opsys == 'any'
      return true if RbConfig::CONFIG['host_os'].start_with? opsys
    end
  end
end
