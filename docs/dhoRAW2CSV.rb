#!/usr/bin/env -S ruby
# -*- Mode:ruby; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      dhoRAW2CSV.rb
# @author    Mitch Richling http://www.mitchr.me/
# @brief     Convert RAW preamble & waveform data from Rigol DHO2000/4000 series oscilloscopes into a CSV files.@EOL
# @std       Ruby 3
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
require 'optparse'
require 'optparse/time'
require 'fileutils' 
require 'set' 

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Process start time
processStartTime = Time.new

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Print stuff to STDOUT & STDERR immediately -- important on windows
$stdout.sync = true
$stderr.sync = true

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Get options
outTime   = true   # Output time or time and voltage
outTitle  = true   # Output titles
outSep    = ','    # Output separator
verbose   = 1      # Verbose level
outFileN  = nil
preFileN  = nil
opts = OptionParser.new do |opts|
  opts.banner = ""
  opts.separator("Transform Rigol DHO2000/4000 waveform data into CSV                  ")
  opts.separator("                                                                     ")
  opts.separator("Usage: sdsRAW2CSV.rb [options] waveform_data_file                    ")
  opts.separator("                                                                     ")
  opts.separator("  Options:                                                           ")
  opts.on("-h",      "--help",          "Show this message                             ")  { STDERR.puts(opts); exit                }
  opts.on("-v INT",  "--verbose INT",   "Verbose level                                 ")  { |v| verbose   = v.to_i                 }
  opts.separator("        1 - Errors                                                   ")
  opts.separator("        2 - Warnings                                                 ")
  opts.separator("        3 - Progress (DEFAULT)                                       ")
  opts.separator("        5 - Arguments                                                ")
  opts.separator("        7 - Metadata                                                 ")
  opts.on("-t Y/N",  "--title Y/N",     "Print titles (DEFAULT: Y)                     ")  { |v| outTitle  = !!(v.match(/^[yt1]/i)) }
  opts.on("-s SEP",  "--separator SEP", "Separator (DEFAULT: comma)                    ")  { |v| outSep    = v                      }
  opts.on("-p FILE", "--preamble FILE", "File containing preamble data                 ")  { |v| preFileN  = v                      }
  opts.on("-o FILE", "--output FILE",   "Output file                                   ")  { |v| outFileN  = v                      }
  opts.separator("                                                                     ")
  opts.separator("Only tested on the Rigol DHO4000 series.                             ")
  opts.separator("                                                                     ")
  opts.separator("The longer record lengths of the DHO series make a CSV workflow a bit")
  opts.separator("cumbersome. A 500M point waveform requires about 1GB of space in     ")
  opts.separator("binary form, but expands to over 11GB in CSV form. Thus a CSV based  ")
  opts.separator("process (read raw file, write CSV, read CSV into analysis tool) is at")
  opts.separator("least 20x slower than reading the raw file directly into the analysis")
  opts.separator("tool.  On my modest tablet computer the difference between the two   ")
  opts.separator("methodologies is 5 seconds vs 3 minutes.  Things get even worse on my")
  opts.separator("Raspberry Pi.   See: readDHO4k.m                                     ")
  opts.separator("                                                                     ")
end
opts.parse!(ARGV)

if ( !(outFileN)) then
  STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - ERROR(1): Must provide an output file!\n\n")
  STDERR.puts(opts)
  exit
end

if ( !(preFileN)) then
  STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - ERROR(1): Must provide a preamble file!\n\n")
  STDERR.puts(opts)
  exit
end

outFileD = nil
if (outFileD == '-') then
  outFileD = STDOUT
else
  outFileD = open(outFileN, "wb");
end

dataFileName = nil
if (ARGV.length < 1) then
  STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - ERROR(1): Must provide a waveform data file!\n\n")
  STDERR.puts(opts)
  exit
elsif (ARGV.length > 1) then
  STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - ERROR(1): May provide only one waveform data file!\n\n")
  STDERR.puts(opts)
  exit
else
  dataFileName = ARGV[0]
end

if verbose >= 5 then  
  STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(5): outTime ........ #{outTime.inspect      }")
  STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(5): outTitle ....... #{outTitle.inspect     }")
  STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(5): outSep ......... #{outSep.inspect       }")
  STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(5): verbose ........ #{verbose.inspect      }")
  STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(5): outFileN ....... #{outFileN.inspect     }")
  STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(5): preFileN ....... #{preFileN.inspect     }")
  STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(5): dataFileName ... #{dataFileName.inspect }")
end
 
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
if verbose >= 3 then
  STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(3): Read, Parse, & Extract preamble data")
end

preamble_data_hash  = ['FORMat', 'MODE', 'POINts', 'AVGcount', 
                       'XINCrement', 'XORigin', 'XREFerence', 
                       'YINCrement', 'YORigin', 'YREFerence'].zip(File.read(preFileN, :encoding=>'binary').strip.split(',')).to_h

predFORMat     = preamble_data_hash['FORMat'].to_i;
predMODE       = preamble_data_hash['MODE'].to_i;
predPOINts     = preamble_data_hash['POINts'].to_i;
predAVGcount   = preamble_data_hash['AVGcount'].to_i;
predXINCrement = preamble_data_hash['XINCrement'].to_f;
predXORigin    = preamble_data_hash['XORigin'].to_f;
predXREFerence = preamble_data_hash['XREFerence'].to_f;
predYINCrement = preamble_data_hash['YINCrement'].to_f;
predYORigin    = preamble_data_hash['YORigin'].to_f;
predYREFerence = preamble_data_hash['YREFerence'].to_f;

# Report on preamble data
if verbose >= 7 then  
  ['FORMat', 'MODE', 'POINts', 'AVGcount', 'XINCrement', 'XORigin', 'XREFerence', 'YINCrement', 'YORigin', 'YREFerence'].each do |k|
    STDERR.printf("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(7): %11s : %s\n", k, preamble_data_hash[k])
  end
end

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
if verbose >= 3 then
  STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(3): Reading Y data")
end
tmp = File.read(dataFileName, :encoding=>'binary')
if verbose >= 3 then
  STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(3): Converting Y data & printing results")
end

if outTitle then
    if outTime then
      outFileD.puts("t#{outSep}v")
    else
      outFileD.puts('v')
    end
end
if (predFORMat != 2) then
  # Binary waveform data starts with a variable length ASCII header!!  The header starts with the literal '#' character.  This is followed by a single ASCII
  # HEX digit indicating the number of digits that follow.  The remaining characters are ASCII DECIMAL digits indicating the number of bytes that follow.  
  datPfxLen = tmp[1].to_i(16) + 2;
  if verbose >= 7 then
    STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(7): Binary prefix length: #{datPfxLen.inspect}")
  end

  pfxDat = tmp.slice!(0, datPfxLen)
  if verbose >= 7 then
    STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(7): Binary prefix: #{pfxDat.inspect}")
  end

  wave_unpack_code = nil;
  if (predFORMat == 0) then
    wave_unpack_code = 'C*'  # 1 byte, Unsigned, no-endian
  elsif (predFORMat == 1) then
    wave_unpack_code = 'S<*' # 2 byte, Unsigned, little-endian
  else
    if verbose >= 1 then
      STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - ERROR(1): Unrecognized FORMat: #{predFORMat.inspect}")
    end
    exit
  end

  estOutSizeValue = (12.0 * (outTime ? 2 : 1) * tmp.length / (predFORMat == 0 ? 1 : 2));
  estOutSizeUnits = 'B'
  ['KB', 'MB', 'GB'].each do |unitS|
    if estOutSizeValue > 1024 then
      estOutSizeValue = estOutSizeValue / 1024.0
      estOutSizeUnits = unitS
    end
  end
  if (verbose >= 3) then
    STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(3): Estimated output file size: #{'%0.1f' % estOutSizeValue} #{estOutSizeUnits} ")
  end
  if (verbose >= 2) then
    if (estOutSizeUnits == 'GB') then
      STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - WARNING(2): WARNING! LARGE OUTPUT FILE! WARNING!")
    end
  end

  # We do the unpack in batches because if we do them all at one time we run out of RAM, and if we do them one at a time it takes forever.
  numBytesToProcess = tmp.length
  pIdx = 0
  begin
    tmpB = tmp.slice!(0, 32*1024*1024)
    tmpV = tmpB.unpack(wave_unpack_code)
    tmpV.each do |yptI|
      yptV = (yptI - predYORigin - predYREFerence) * predYINCrement
      if outTime then
        xptV = predXORigin + predXREFerence + (pIdx * predXINCrement)
        outFileD.puts("#{'%.7g' % xptV}#{outSep}#{'%.7g' % yptV}")
      else
        outFileD.puts("#{'%.7g' % yptV}")
      end
      pIdx += 1
    end
    if (verbose >= 3) then
      STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(3): #{(100.0-100.0*tmp.length/numBytesToProcess).to_i}% complete")
    end
  end while ( !(tmp.empty?))
  if verbose >= 7 then
    STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(7): Number of points in output: #{pIdx}")
  end
else
  pIdx = 0
  tmp.chomp.split(',') do |yptS|
    yptV = yptS.to_f
    if outTime then
      xptV = predXORigin + predXREFerence + (pIdx * predXINCrement)
      outFileD.puts("#{'%.7g' % xptV}#{outSep}#{'%.7g' % yptV}")
    else
      outFileD.puts("#{'%.7g' % yptV}")
    end
    pIdx += 1
  end
end

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
if verbose >= 3 then
  STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(3): Total runtime: #{Time.new - processStartTime}")
  STDERR.puts("dhoRAW2CSV: #{Time.new.inspect.ljust(35, ' ')} - INFO(3): Finished")
end
