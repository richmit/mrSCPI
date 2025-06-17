#!/usr/bin/env -S ruby
# -*- Mode:ruby; Coding:us-ascii; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      teAlias.rb
# @author    Mitch Richling http://www.mitchr.me/
# @brief     Take an instrument alias, and print an instrument connection URL string to STDOUT.@EOL
# @std       Ruby3
# @copyright 
#  @parblock
#  Copyright (c) 2023, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
#
#  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following disclaimer.
#
#  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#
#  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without
#     specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
#  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
#  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
#  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#  @endparblock
#########################################################################################################################################################.H.E.##

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
if ENV['MRSCPIPATH'] then                                # First look in MRSCPIPATH
  require File.join(ENV['MRSCPIPATH'], 'mrSCPI.rb')        # Note if MRSCPIPATH is set and require fails, then we want the script to fail!
else
  begin                                                  # Then look in path with the script is
    require_relative 'mrSCPI.rb'
  rescue LoadError
    begin                                                # Then look on LOADPATH
      require 'mrSCPI.rb'
    rescue LoadError                                     # Then look on PATH
      require ENV['PATH'].split(File::PATH_SEPARATOR).map {|x| File.join(x, 'mrSCPI.rb')}.find {|x| FileTest.exist?(x)}
    end
  end
end

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
if (ARGV.length < 1) || (ARGV.first.match(/^-+[hH]/)) then
  puts("USE: teAlias.rb [format] thingy")
  puts("       thingy is one of netqork@nickname, @nickname, or nickname")
  puts("         network is a network name in the mrSCPIrc file")
  puts("         nickname is a nickname name in the mrSCPIrc file")
  puts("       format is a format string for how to print the result. Default: '%u'")
  puts("         %s -- replaced by the scheme (http, https, soip, etc...)")
  puts("         %h -- replaced by the IP address or DNS name")
  puts("         %p -- replaced by the port number")
  puts("         %u -- replaced by the full URL")
  puts("       Example: telnet `teAlias '%h %p' @foobar`")
  puts("       Example: ssh `teAlias '-p %p %h' @foobar`")
  puts("       Example: chromium-browser `teAlias.rb @foobar`")
  puts("                Works because '%u' is the default format.")
  puts("")
  puts("See SCPIrcFile.lookupURLnickname for full details.")
  exit 0
elsif (ARGV.length == 1) then
  print(SCPIrcFile.instance.lookupURLnickname(ARGV[0]))
elsif (ARGV.length == 2) then
  urlBits = SCPIrcFile.instance.urlParser(ARGV[1])
  print(ARGV[0].gsub('%h', urlBits[:ip_address]).gsub('%s', urlBits[:net_protocol].to_s).gsub('%u', urlBits[:url]).gsub('%p', urlBits[:net_port].to_s))
end

