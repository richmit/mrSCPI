% -*- Mode:octave; Coding:us-ascii-unix; fill-column:120 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%.H.S.%%
%% Copyright (c) 2023, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
%%   Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
%%   following conditions are met:
%%       1. Redistributions of source code must retain the above copyright notice, this list of conditions,
%%          and the following disclaimer.
%%       2. Redistributions in binary form must reproduce the above copyright notice, this list of
%%          conditions, and the following disclaimer in the documentation and/or other materials provided
%%          with the distribution.
%%       3. Neither the name of the copyright holder nor the names of its contributors may be used to
%%          endorse or promote products derived from this software without specific prior written permission.
%%   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
%%   USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
%% File      readDHO4k.m          
%% Author    Mitch Richling http://www.mitchr.me/
%% Std       octave_7 matlab_2022b           
%% See       getwave_dho4k.rb dhoRAW2CSV.rb
%% Usage 
%%  Description
%%    Read Rigol DHO2000/4000 waveform data into Matlab/Octave
%%  Calling Forms
%%     vData                 = readDHO4k(prefix_file, data_file)
%%    [vData, period]        = readDHO4k(prefix_file, data_file)
%%    [vData, period, tData] = readDHO4k(prefix_file, data_file)
%%  Inputs
%%    - prefix_file ... File with the results of :WAVeform:PREamble?
%%    - data_file ..... File with the results of :WAVeform:DATA?
%%  Outputs
%%    - vData ......... Voltage values for waveform
%%    - period ........ Time between samples
%%    - tData ......... Time points for waveform
%%  Details
%%    Only tested on DHO4000 series, but should work on DHO2000 as well. Tested on both Matlab & Octave. This function
%%    supports binary waveform files -- both BYTE & WORD.  ASCII files are not supported -- use dlmread for that. I
%%    normally use this function with files generated via the getwave_dho4k.rb script. The dhoRAW2CSV.rb script can
%%    convert the kinds of data this function reads to CSV; however, the CSV file will be much larger than the RAW file
%%    (11GB vs 1GB).  In addition, the time required to produce the CSV and then load it into Matlab/Octave is an order
%%    of magnitude greater. This function can read a 500M point file, with 16-bits per point, in about 5 seconds on my
%%    tablet.
%%  Examples
%%    Example 1 -- Plot the waveform with valid time data
%%      [vdata, per, tdata] = readDHO4k('dho4k_FW00.02.02_word10k_CHANnel1_waveform.pre', ...
%%                                      'dho4k_FW00.02.02_word10k_CHANnel1_waveform.dat');
%%      plot(tdata, vdata);
%%      signalAnalyzer(timeseries(vdata, tdata));  % Matlab only!
%%    Example 2 -- If we don't care about the time data we can just leave it off
%%      vdata = readDHO4k('dho4k_FW00.02.02_word10k_CHANnel1_waveform.pre',  ...
%%                        'dho4k_FW00.02.02_word10k_CHANnel1_waveform.dat');
%%      plot(vdata);
%%    Example 3 -- We don't need the time points to display a pspectrum
%%      [vdata, per] = readDHO4k('dho4k_FW00.02.02_word10M_CHANnel1_waveform.pre',  ...
%%                               'dho4k_FW00.02.02_word10M_CHANnel1_waveform.dat');
%%      pspectrum(vdata, 1/per);
%%      plotFreqSpec(vdata, per);
%%      signalAnalyzer(vdata, 'SampleTime', per);  % Matlab only!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%.H.E.%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function varargout = readDHO4k(prefix_file, data_file)
  %% Check arguments
  validateattributes(prefix_file, {'char', 'string'}, {'nonempty'});
  validateattributes(data_file,   {'char', 'string'}, {'nonempty'});
  %% Check returns
  if (nargout < 1) 
    error("readDHO4k: No return value requested!  Aborting run!")
  end
  if (nargout > 3) 
    error("readDHO4k: Two many return values requested.  Aborting run!")
  end
  %% Read in the waveform preamble data
  fid = fopen(prefix_file, 'r');
  if (fid < 0)
    error("readDHO4k: Unable to open prefix_file (%s)!", prefix_file);    
  end
  predFORMat     = fscanf(fid, '%d,', 1);
  predMODE       = fscanf(fid, '%d,', 1);
  predPOINts     = fscanf(fid, '%d,', 1);
  predAVGcount   = fscanf(fid, '%d,', 1);
  predXINCrement = fscanf(fid, '%f,', 1);
  predXORigin    = fscanf(fid, '%f,', 1);
  predXREFerence = fscanf(fid, '%f,', 1);
  predYINCrement = fscanf(fid, '%f,', 1);
  predYORigin    = fscanf(fid, '%f,', 1);
  predYREFerence = fscanf(fid, '%f,', 1);
  fclose(fid);
  %% Figure out the size of the samples
  if (predFORMat == 0) 
    sampleWidth    = 1;
    sampleType     = 'uint8';
  elseif (predFORMat == 1) 
    sampleWidth    = 2;
    sampleType     = 'uint16';
  else   
    error("readDHO4k: Unsupported waveform format (use BYTE or WORD)!");
  end
  %% Read the data file
  fid = fopen(data_file, 'r');
  if (fid < 0)
    error("readDHO4k: Unable to open data_file (%s)!", data_file);    
  end
  prefixWidth = fscanf(fid, '#%1x', 1);
  dataWidth   = fscanf(fid, ['%', num2str(prefixWidth), 'd'], 1);
  if ((predPOINts * sampleWidth) ~= dataWidth)
    error('readDHO4k: Prefix predFORMat value (%d) incompatable data file size (%d)', predPOINts, dataWidth);
  end
  varargout{1} = fread(fid, predPOINts, sampleType);
  fclose(fid);
  %% Convert data to voltage and construct time points
  varargout{1} = (varargout{1} - predYORigin - predYREFerence) * predYINCrement;
  if (nargout >= 2) 
    varargout{2} = predXINCrement;
  end
  if (nargout >= 3) 
    varargout{3} = predXORigin + predXREFerence + ([0:(predPOINts-1)] * predXINCrement);
  end
  %% That's it!
  return;
end
