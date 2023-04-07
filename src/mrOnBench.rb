#!/usr/bin/env -S ruby
# -*- Mode:ruby; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      mrOnBench.rb
# @author    Mitch Richling http://www.mitchr.me/
# @brief     Print "ON BENCH" if the computer running this script is on my electronics workbench network, and print "OFF BENCH" otherwise.@EOL
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
if SCPIrcFile.instance.lookupNetwork == 'bench' then
  puts('ON BENCH')
  exit(0)
else
  puts('OFF BENCH')
  exit(1)
end
