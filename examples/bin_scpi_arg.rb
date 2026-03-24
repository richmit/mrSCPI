#!/usr/bin/env -S ruby
# -*- Mode:ruby; Coding:us-ascii; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      bin_scpi_arg.rb
# @author    Mitch Richling http://www.mitchr.me/
# @brief     Demonstrate mrSCPI as a Ruby library and SCPI binary blocks.@EOL
# @std       Ruby 3 mrSCPI
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
# @filedetails
#
#  In the code below we provide 8192, 14-bit sample points as an IEEE-488.2 binary data block containing 16-bit integers to an Agilent 33210A arbitrary
#  waveform generator.  This function generator can accept ASCII formatted floats, ASCII formatted integers, and binary integers.  With only 8k points there
#  isn't much practical benefit to using binary mode, but it gives us a nice way to demonstrate how to do so with mrSCPI!
#
#########################################################################################################################################################.H.E.##

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
require ENV['PATH'].split(File::PATH_SEPARATOR).map {|x| File.join(x, 'mrSCPI.rb')}.find {|x| FileTest.exist?(x)}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Number of data points for our curve.
size = 8192

if (size < 1) then
  STDERR.puts("ERROR: Data size too small!")
  exit
elsif (size > 8192) then
  STDERR.puts("ERROR: Data size too big!!")
  exit
end

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Populate floating point data: fdata
# We evaluate |sin(t)| over [0, 2pi].
fdata = Array.new(size)
fdata.each_index do |i|
  fdata[i] = (Math.sin(2 * Math::PI * i.to_f / size.to_f)).abs
end

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Populate short integer data: sdata
# We convert and scale the data in fdata, and store it in sdata.
fmin = fdata.min
fmax = fdata.max

bmin = -8191
bmax =  8191

if ((fmax - fmin) < 1.0e-8) then
  STDERR.puts("WARNING: Curve is constant.")
  m = 0
  b = 0
else
  m = (bmax - bmin).to_f / (fmax - fmin)
  b = bmax.to_f - m * fmax
end

sdata = Array.new(size)
fdata.each_with_index do |v, i|
  sdata[i] = [[(m * v + b).to_i, bmax].min, bmin].max
end

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Construct a string with the contents of the IEEE-488.2 binary block with our waveform data.
blen    = (size * 2).to_s
blenlen = blen.length.to_s
bdata   = "#" + blenlen + blen + sdata.pack('n*')

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Send everything to the device
theSPCIsession = SCPIsession.new(:url         => '@33210a',   # Change for your instrument's IP address
                                 :result_type => nil)         # None of the following commands have any output to capture
theSPCIsession.set({:cmd => '*RST'})                          # Reset the unit to defaults.
theSPCIsession.set({:cmd => ':DATA:DAC VOLATILE, ' + bdata }) # Send our data
theSPCIsession.set({:cmd => ':FUNCtion:USER VOLATILE'})       # Set the ARB waveform to 'VOLATILE'.
theSPCIsession.set({:cmd => ':APPLy:USER 2000,4,0'})          # 2 kHz, 4 Vpp, 0 V offset
