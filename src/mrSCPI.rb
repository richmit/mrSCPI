#!/usr/bin/env -S ruby
# -*- Mode:Ruby; Coding:us-ascii; fill-column:158 -*-
################################################################################################################################################################
##
# @file      mrSCPI.rb
# @author    Mitch Richling <https://www.mitchr.me>
# @date      2021-07-02
# @version   2021-07-04
# @brief     Ruby API, domain specific language, and command line tool for instrument control via SCPI@EOL
# @keywords  scpi gpib serial tcp/ip ethernet
# @std       Ruby 2.0
# @copyright
#  @parblock
#  Copyright (c) 2021, Mitchell Jay Richling <https://www.mitchr.me> All rights reserved.
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
#  Note: This file contains both a library, and a program that uses that library -- it's a Ruby thing.  That is to say you can execute this script just like
#  any normal Ruby script, or you can 'require' it into another Ruby script to access the API functionality.
#
#  Potential Exit Values:
#   - 70 :url was malformed
#   - 71 Option can't be set post new
#   - 72 Unknown Session Option
#   - 73 Required option is invalid
#   - 74 :cmd must not appear in options list when calling 'command'!"
#   - 75 Result not found with given name
#   - 76 Unable to execute.  Required option is invalid
#   - 77 unknown protocol
#   - 78 No nicknams loaded
#   - 79 Nickname was not found in the RC file
#   - 80 Invalid nickname string
#   - 81 RC config file errors
#   - 82 Unable to open TCP/IP socket!
#   - 83 Failed to open RC file!
#   - 84 Failed to open file (:out_file or :err_file)
#   - 85 Failed to open script file (only happens when running mrSCPI.rb)
#   - 86 :lxi is not currently supported -- it will be someday!! (TODO: Future Feature)
#   - 87 Unable to read SCPI file response (:net_protocol == :file)          
#   - 88 Unknown variable without default value! -- used when interpolateing vairable values
#   - 89 Unknown regexp
#   - 90 RESERVED
#   - 91 RESERVED
#   - 92 RESERVED
#   - 93 RESERVED
#   - 94 RESERVED
#   - 95 RESERVED
#   - 96 RESERVED
#   - 97 RESERVED
#   - 98 RESERVED
#   - 99 RESERVED
################################################################################################################################################################

################################################################################################################################################################
require 'set'
require "socket"
require 'uri'
require 'net/http'
require 'net/https'
require 'ipaddr'
require 'singleton'

################################################################################################################################################################
# Print stuff to STDOUT & STDERR immediately -- important on windows
$stdout.sync = true
$stderr.sync = true

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
# = Introduction
#
# SCPIregexLibrary provides access to regular expressions used across mrSCPI.  Many of them are related to SCPI and IEEE 488.2-1992, and are thus useful for
# scripts using mrSCPI.  Others are related to generic text processing as it related to SCPI results, or even mrSCPI internals. Regular expressions may be
# naturally anchored (available via the #a member) or unanchored (available via the #u member).
#
# = Use
#
# The SCPIregexLibrary is a Singleton.  The normal way to use it is as follows
#    aString.match?(SCPIregexLibrary.instance.a(:split_line))
#
# = Available Regexes
#
# - For instrument response strings as defined in IEEE 488.2-1992 (anchor: both sides)
#   - +:o488_NR1+::       Strict response NR1 integer. Ref: 8.7.2.2 of IEEE 488.2-1992 p96. Submatches: sign, value
#   - +:o488_NR2+::       Strict response NR2 float. Ref: 8.7.3.2 of IEEE 488.2-1992 p97. Submatches: sign, value
#   - +:o488_NR3+::       Strict response NR2 float. Ref: 8.7.4.2 of IEEE 488.2-1992 p98. Submatches: man sgn, man value, exp sgn, exp value
#   - +:o488_NR123+::     Matches any of +:o488_NR1+, +:o488_NR2+, +:o488_NR3+. Submatches: man sgn
#   - +:o488_HEX+::       Strict response HEX integer. Ref: 8.7.5.2 of IEEE 488.2-1992 p99. Submatches: digits
#   - +:o488_OCT+::       Strict response OCT integer. Ref: 8.7.6.2 of IEEE 488.2-1992 p100. Submatches: digits
#   - +:o488_BIN+::       Strict response BIN integer. Ref: 8.7.7.2 of IEEE 488.2-1992 p101. Submatches: digits
#   - +:o488_STRING+::    Strict response string. Ref: 8.7.8.2 of IEEE 488.2-1992 p102. Submatches: chars 
#   - +:o488_NegInf+::    Negative infinity in NR1, NR2, or NR3 format                              
#   - +:o488_PosInf+::    Positive infinity in NR1, NR2, or NR3 format                              
#   - +:o488_NaN+::       NaN NR1, NR2, or NR3 format                                               
# - For instrument response binary block headers as defined in IEEE 488.2-1992 (anchor: front)
#   - +:o488_FBLOCK+::    Fixed sized data block first 2 chars. Ref: 8.7.9.2 of IEEE 488.2-1992 p103. Submatches: digits 
#   - +:o488_FBLOCKh+::   Like +:o488_FBLOCK+, but supports HEX second char. Submatches: Digit 
#   - +:o488_VBLOCK+::    Variable sized data block header. Ref: 8.7.9.2 of IEEE 488.2-1992 p103.   
# - For instrument program or response strings as used in practice (anchor: both sides)
#   - +:x488_TRUE+::      Matches conventional SCPI +ON+ or +1+ messages                            
#   - +:x488_TRUEx+::     Like +:o488_TRUE+, but adds +T+, +TRUE+, +Y+, & +YES+
#   - +:x488_FALSE+::     Matches conventional SCPI +OFF+ or +0+ messages                           
#   - +:x488_FALSEx+::    Like +:o488_FALSE+, but adds +F+, +FALSE+, +N+, +NO+, +NIL+, & +NULL+                   
# - For program input strings sent to an instrument as defined in IEEE 488.2-1992 (anchor: both sides)
#   - +:i488_NRf+::       Strict input NRf float.    Ref: 7.7.2.2 of IEEE 488.2-1992 p 73. Submatches: man sgn, man value, exp sgn, exp value
#   - +:i488_NRfNS+::     Like +:i488_NRf+, but no whitespace allowed. Submatches: man sgn, man value, exp sgn, exp value
# - For splitting strings (anchor: NONE)
#   - +:split_csv+::      Split on commas surrounded by optional white space                       
#   - +:split_ssv+::      Split on semicolons surrounded by optional white space                   
#   - +:split_space+::    Split on blocks of whitespace (horizontal or vertical)                   
#   - +:split_line+::     Split on blocks of vertical whitespace                                   
# - Handy regexes for mrSCPI internals (anchor: NONE)
#   - +:mrs_url+::        Match mrSCPI URL. Submatches: protocol, ip address, port number   
#   - +:mrs_nickname+::   Match mrSCPI nickname. Submatches: network, name    
#   - +:mrs_branch+::     Match mrSCPI script branching statements (+:skip_if+, +:next_if+). Submatches: LHS, operator, RHS    
#   - +:mrs_assign+::     Match mrSCPI script assignment statements (+:eval+, +:var+). Submatches: LHS, RHS      
#   - +:mrs_varexp+::     Match mrSCPI script variable expantion expressions.  Submatches: variable name, default value
class SCPIregexLibrary
  include Singleton

  def initialize()
    @reDatabase = [ [ :o488_NR1,     '([+-]?)([0-9]+)',                                              '^', '$',                      Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_NR2,     '([+-]?)([0-9]+\.[0-9]+)',                                      '^', '$',                      Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_NR3,     '([+-]?)([0-9]+\.[0-9]+)e([+-]?)([0-9]+)',                      '^', '$', Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_NR123,   '([+-]?)([0-9]+|[0-9]+\.[0-9]+|[0-9]+\.[0-9]+e[+-]?[0-9]+)',    '^', '$', Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_HEX,     '#h([0-9a-f]+)',                                                '^', '$', Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_OCT,     '#q([0-7]+)',                                                   '^', '$', Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_BIN,     '#b([01]+)',                                                    '^', '$', Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_STRING,  '"((""|[^"])*)"',                                               '^', '$',                      Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_NegInf,  '(-9.9e\+?37|-990{36})(\.|\.[0-9]+)?',                          '^', '$', Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_PosInf,  '(\+?9.9e\+?37|\+?990{36})(\.|\.[0-9]+)?',                      '^', '$', Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_NaN,     '(\+?9.91e\+?37|\+?9910{35})(\.|\.[0-9]+)?',                    '^', '$', Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_FBLOCK,  '#([1-9])',                                                     '^', '',                       Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_FBLOCKh, '#([1-9a-f])',                                                  '^', '',                       Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_VBLOCK,  '#0',                                                           '^', '',                       Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :i488_NRf,     '([+-]?)([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)\s*e\s*([+-]?)([0-9]+)', '^', '$', Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :i488_NRfNS,   '([+-]?)([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)e([+-]?)([0-9]+)',       '^', '$', Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_TRUE,    '(on|1)',                                                       '^', '$', Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_TRUEx,   '(on|1|true|t|yes|y)',                                          '^', '$', Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_FALSE,   '(off|0)',                                                      '^', '$', Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :o488_FALSEx,  '(off|0|false|f|no|n|nil|null)',                                '^', '$', Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :split_csv,    '\s*,\s*',                                                      '',  '',                       Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :split_ssv,    '\s*;\s*',                                                      '',  '',                       Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :split_space,  '\s+',                                                          '',  '',                       Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :split_line,   '\s*\R+\s*',                                                    '',  '',                       Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :mrs_url,      '(?:([a-zA-Z0-9]+):\/\/)?([^:\/]+)(?::([0-9]+)){0,1}',          '^', '$',                      Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :mrs_nickname, '(?:([^@:\/\s]*)@)?([^@:\/\s]+)',                               '^', '$',                      Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :mrs_branch,   '([^=!~]+)(=|!=|~|!~)(.+)',                                     '^', '$', Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :mrs_assign,   '([^=]+)=(.+)',                                                 '^', '$',                      Regexp::EXTENDED | Regexp::MULTILINE ],
                    [ :mrs_varexp,   '\$\{([^\}:]+)(?::([^\}]+))?\}',                                '',  '',                       Regexp::EXTENDED | Regexp::MULTILINE ],
                  ]
    @reListDef  = Hash.new # REs with default anchors
    @reListNone = Hash.new # REs with no anchors (floating)

    @reDatabase.each do |reSymb, reStr, reDefAnchL, reDefAnchR, reDefFlag|
      @reListDef[reSymb]  = Regexp.new(reDefAnchL + reStr + reDefAnchR, reDefFlag)
      @reListNone[reSymb] = Regexp.new(             reStr,              reDefFlag)
    end
  end

  # Returns regexp with the default anchors.  Returns +nil+ if no regexp found.
  def a(aSymb)
    ret = @reListDef[aSymb]
    if ret.nil? then
      PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Unknown regexp (#{aSymb.inspect})!", self, 89)
    end
    return ret
  end

  # Returns a regexp with no anchors.  Returns +nil+ if no regexp found.
  def u(aSymb)
    ret = @reListNone[aSymb]
    if ret.nil? then
      PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Unknown regexp (#{aSymb.inspect})!", self, 89)
    end
    return ret
  end

end

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
# = Introduction
#
# SCPIsession manages network connections and connection state for SPCI sessions.  
#
# = SCPIsession State Parameters
#
# The object is controlled via changing the object's state parameters. I know, that's not much to go on.  It's really pretty simple.  A few examples are
# probably the best way to understand it.  See the mrSCPI readme.
#
# == SCPIsession State Parameters: Global behavior
#
# - +:verbose+(P/Integer):: How much to print to +:log_file+ (DEFAULT: +1+)
#   Values:  
#   - +0+: Silent
#   - +1+: Errors
#   - +2+: Warnings
#   - +3+: Information
#   - +4+: Debug-4
#   - +5+: Debug-5 Same as Debug4 (TODO: Future Feature)
#   - +6+: Debug-6 Same as Debug4 (TODO: Future Feature)
#   - +7+: Debug-7 Same as Debug4 (TODO: Future Feature)
#   - +8+: Debug-8 Print out data structures
# - +:exit_on_error+(L/Boolean):: Exit if an error is encountered (DEFAULT: true)
#   See: PrintyPrintyBangBang
# - +:exit_0+(L/Boolean):: Abnormal exits are always zero -- required for org-mode (DEFAULT: +false+)
#   See: PrintyPrintyBangBang
# - +:execute_on_cmd+(Boolean):: Immediately execute when :cmd is updated (DEFAULT: +true+)
#   If false, then the #execute method must be used to force command execution
# - +:log_file+(L/String):: Name of file for log messages (DEFAULT: <tt>"STDERR"</tt>)
#   Some file names are special: <tt>"STDOUT"</tt>, <tt>"STDERR"</tt>, & <tt>"/dev/null"</tt>
#   See: PrintyPrintyBangBang
#
# == SCPIsession State Parameters: Command string specification
#
# - +:cmd+(String):: The current command string
# - +:scpi_prefix+(String):: An SCPI prefix to be added when :cmd is sent to instrument (DEFAULT: <tt>""</tt>)
# - +:eol+(String):: End of line string to append to command strings sent to instrument (DEFAULT: <tt>"\n"</tt>)
#
# == SCPIsession State Parameters: Instrument I/O tuning
#    
# - +:read_timeout_first_byte+(Integer/milliseconds):: Timeout for first byte of data from instrument.
#   Default depends on :net_protocol: 
#   +:raw+::    1000
#   +:soip+::   500
#   +:plgx+::   500
#   +:file+::   0
# - +:read_timeout_next_byte+(Integer/milliseconds):: Timeout for more data after receiving a response from instrument
#   Default depends on :net_protocol: 
#   +:raw+::    2000
#   +:soip+::   100
#   +:plgx+::   100
#   +:file+::   0
# - +:delay_before_first_read+(Integer/milliseconds):: Time to wait before trying to read a result from instrument
#   Default depends on :net_protocol: 
#   +:raw+::    100
#   +:soip+::   50
#   +:plgx+::   50
#   +:file+::   0
# - +:delay_after_complete+(Integer/milliseconds):: Time to wait after a command has completed before returning
#   Default depends on :net_protocol: 
#   +:raw+::    0
#   +:soip+::   100
#   +:plgx+::   50
#   +:t3k+::    200
#   +:file+::   0
# - +:read_retry_delay+(Integer/milliseconds):: Time to wait before retrying a read operation
#   Default depends on :net_protocol: 
#   - +:raw+::  200
#   - +:soip+:: 200
#   - +:plgx+:: 100
#   - +:file+:: 0
# - +:read_eot_sentinel+(nil|String):: Stop reading when the character appears on the input stream (DEFAULT: +nil+)
#
#   This can dramatically speed up query commands, but requires the instrument send an "end of transmission" character.
#   Frequently we can exploit the fact that an instrument will terminate some responses with a newline character, and thus
#   use the newline as an EOT character.  For Prologix devices, an EOT character may be defined; however, this can interfere
#   when binary data is to be transmitted from an instrument.
# - +:good_std_eot+(P/Boolean):: Set +:read_eot_sentinel+ to <tt>"\n"</tt> if +true+, and to +nil+ if +false+.
# - +:socket_close_write+(Boolean):: For raw & soip do a write_close on the TCP socket after sending command string (DEFAULT: +false+)
# - +:socket_close+(Boolean):: For raw & soip do a close on the TCP socket after reading is complete (DEFAULT: +false+)
# - +:read_buffer_size+(Integer/bytes):: Maximum number of bytes to attempt to read at once (DEFAULT: +1048576+)
# - +:read_max_bytes+(Integer/bytes):: Stop reading after we get this many bytes or more (DEFAULT: +nil+)
#   This is useful when transfusing data from the instrument and you know exactly how much data to expect
#
# == SCPIsession State Parameters: Connection Parameters
# Connection parameters may only be set when an SCPIsession object is created (via new).
# - +:url+(P/String):: Used to set +:ip_address+ and potentially +:net_protocol+ & +:net_port+. If any of these options are explicitly set, then the explicitly set value wins. 
#   URL formats: 
#   - <tt>[net_protocol://]ip_address[:net_port]</tt>
#   - <tt>[network]@nickname</tt> --- Note nicknames *always* contain an <tt>@</tt>
#   See: SCPIrcFile.urlParser & SCPIrcFile.lookupURLnickname
# - +:ip_address+(String):: IP address
# - +:net_protocol+(Symbol):: Network protocol. 
#   Values: 
#   - +:raw+:: Raw socket connection (DEFAULT)
#     By far the most well tested mode as most of my equipment has an Ethernet port.
#   - +:soip+:: Serial Over IP
#     Designed for serial console servers or Ethernet to serial converters with a raw TCP/IP socket
#     interface.  The +:socket_close_write+ parameter may break connections on console servers that use
#     connection status to set serial hardware flow control.  These devices may require longer timeouts.
#     This feature has been tested with the following hardware:
#     - Lantronix EDS4100 Terminal Server
#     - Keysight 34401A Bench Digital Multimeter
#     - Tektronix TDS3052B Digital Phosphor Oscilloscope
#     - Tektronix TDS2024 Digital Storage Oscilloscope
#   - +:t3k+:: SCPI over HTTP for TDS3000B series
#     Sends SCPI commands via an HTTP form, and scrapes the result out of the returned HTML document.  Do
#     not use commands that return binary data.  Also note that TCP/IP read loop is bypassed for this mode,
#     and so read timeouts and delays are ignored.
#   - +:lxi+:: VX11/LXI over IP (TODO: Future Feature)
#   - +:plgx+:: SCPI via a Prologix Ethernet adapter
#     The Prologix adapter configured with the following commands when the TCP/IP port is opened:
#          ++savecfg 0 ..... Prevents permanent adapter config changes, and extends EPROM life
#          ++mode 1 ........ Sets adapter to be a controller
#          ++auto 0 ........ Turns off read after write avoiding errors on some equipment
#          ++eoi 1 ......... Turns on end of input
#          ++read_tmo_ms ... Set to :read_timeout_first_byte - 50
#          ++eos ........... Set based on :eol value
#     
#     If +:result_type+ is non-+nil+, then an <tt>'++read eoi'</tt> command is issued before the TCP/IP read loop.  This is sent *before* the +:delay_before_first_read+.
#     This feature has been tested with the following hardware:
#     - Keysight 34401A Bench Digital Multimeter
#     - Tektronix TDS3052B Digital Phosphor Oscilloscope
#     If +:read_timeout_first_byte+ or +:eol+ is changed after the socket is opened, set +:socket_close+ to close the socket after the next command.  This will be fixed someday.
#   - +:file+:: Read response from a file with the same name as :cmd parameter
#     This feature is used for simulating SCPI responce strings without a physical device.  It is used for testing.
# - +:net_port+(Integer):: TCP/IP Port number (DEFAULT: 0 for +:file+, +:soip+, & +:lxi+.  5025 for +:raw+. 1234 for +:plgx+. 80 for +:t3k+)
#
# == SCPIsession State Parameters: Output Control
#    
# - +:out_file+(L/String):: Filename for print operations -- not log operations (DEFAULT: <tt>"STDOUT"</tt>)
#   Some file names are special: <tt>"STDOUT"</tt>, <tt>"STDERR"</tt>, & <tt>"/dev/null"</tt>
#   See: PrintyPrintyBangBang
# - +:print_raw_result+(Boolean):: Output raw results (the raw string returned from the instrument) to :out_file (DEFAULT: +false+)
# - +:print_result+(Boolean):: Output post processed results to :out_file (DEFAULT: +false+)
#   Nothing is printed when +:result_type+ is +nil+
#   WARNING: SCPIsequence sets this to true when it constructs an SCPIsession object
# - +:print_result_puts+(Boolean):: Use +puts+ for prints -- i.e. insure all prints have a newline at the end
# - +:print_cmd+(Boolean):: Output command to :out_file (DEFAULT: +false+)
#   Output is prefixed and postfixed by <tt>'>>'</tt> and followed by a newline
#   WARNING: SCPIsequence sets this to true when it constructs an SCPIsession object
# - +:echo+(P/Boolean):: Sets both +:print_cmd+ & +:print_result+ at the same time (DEFAULT: +false+)
# - +:print_debug+(L/Boolean):: All printed results are in Ruby +.inspect+ format and followed by a newline
#   See: PrintyPrintyBangBang
# - +:print_max_len+(L/Integer or nil):: Limit the number of characters printed (DEFAULT: +nil+)
#   If a print is truncated, then the next line in :out_file will be something like:
#        LAST LINE TRUNCATED (:print_max_len=N)
#   See: PrintyPrintyBangBang
#
# == SCPIsession State Parameters: Result processing
#    
# Strings returned from instruments can be processed via a standard set of transformations controlled by the options in this section.  The actions occur in
# the order they are documented in this section.  For example, operations after +:result_split+ are applied each array element resulting from the split.
# If +:result_type+ is +nil+, none of these actions are taken.
# - +:result_extract_block+(Boolean):: If true and a block header is present, then replace the result sting with the data payload.  Otherwise leave the string unmodified.  
#   A block header starts with a literal pound character <tt>#</tt> and a single hex digit indicating the number of decimal digits that follow.  
#   If this digit is zero, then we have a variable sized block that ends with an EOL terminator.  
#   If the digit is non-zero, then it tells us the number of ASCII decimal digits that follow it indicate the length of the data payload.
# - +:result_split+(Symbol):: If non-nil, then split the result sting into an array. (DEFAULT: +nil+)
#   Values:
#   - +nil+::     Don't do anything -- no split at all
#   - +:char+::   Split on the characters in +:result_split_arg+
#   - +:string+:: Split on the *string* in  +:result_split_arg+
#   - +:unpack+:: Split via an unpack string in +:result_split_arg+
#     WARNING: unpack may produce an array containing non-strings, and thus :result_type must usually be set to :mixed!
#   - +:block+::  Split into two element array containing block header and block payload
#   - +:regex+::  Split via the regex string in +:result_split_arg+
#   - +:line+::   Split on horizontal whitespace
#   - +:space+::  Split on /\s+/ (i.e. whitespace separated values)
#   - +:csv+::    Split on /\s*,\s*/ (i.e. comma separated values)
#   - +:ssv+::    Split on /\s*;\s*/ (i.e. semicolon separated values)
#     Handy for devices (R&S) that return semicolon separated values on compound SCPI statements
#     Example: 
#         :MEASure:SCALar:VOLTage:DC?; :MEASure:SCALar:CURRent:DC?
#         "0.0; 0.0"
# - +:result_split_arg+(String or nil):: Argument required for the +:result_split+ parameter.  (DEFAULT: +nil+)
# - +:result_squeeze+(Boolean):: If true, adjacent whitespace characters are squeezed into a single character -- a single space or newline
# - +:result_chomp+(Boolean):: If true, the stored/returned result string is chomped (DEFAULT: +false+)
#   Note overlap with +:result_strip+!
# - +:result_strip+(Boolean):: If true, the stored/returned result string is stripped of whitespace (DEFAULT: +false+)
#   Zaps +NL+ (null), +HT+ (horizontal tab), +LF+ (line feed), +VT+ (vertical tab), +FF+ (form feed), +CR+ (carriage return), & +SP+ (space)
# - +:result_last_word+(Boolean):: If true, the stored/returned result string is right stripped and only the last word is returned (DEFAULT: +false+)
#   Some instruments return the query command string followed by the value, and this can be a handy way to get just the value.
#   Example: 
#         MEASUrement:IMMed:VALue?
#         :MEASUREMENT:IMMED:VALUE 9.9E37
# - +:result_type+(Boolean):: Expected return from SCPI command (DEFAULT: +nil+)
#   Used to coerce values into Ruby types
#   Values:
#   - +nil+::     no data -- A response is not even read from instrument
#   - +:mixed+::  When +:result_split+ is non-NIL, leave returned data as is.
#     NOTE: Usually the correct value when +:result_split+ is :unpack!
#   - +:string+:: string.  Usually a NOP except, when +:result_split+ values of :unpack.  In the :unpack case, this will convert any non-string elements in an array into strings.
#   - +:float+::  float (NR2, NR3, NRf) -- NRf is only partly supported in that whitespace is not allowed around the "E".
#   - +:int+::    integer (NR1)
#   - +:bool+::   Convert strings to +true+, +false+, or +nil+
#     - +true+::  if result =~ /^\s*(on|1|true|yes)\s*$/i
#     - +false+:: if result =~ /^\s*(off|0|false|no)\s*$/i
#     - +nil+::   otherwise
#     Non-strings are left untouched
#
# == SCPIsession State Parameters: Result storage
#
# - +:name+(String)::                Name of current command.
# - +:store_named_in_var+(Boolean):: Store named results into a variable of the same name (DEFAULT: +true+)
# - +:store_last_in_ans+(Boolean)::  Store result into a variable named 'ANS' (DEFAULT: +true+)
# - +:store_results+(Boolean)::      Store the instrument response on the results stack (DEFAULT: +true+)
# - +:store_raw_results+(Boolean)::  Store the raw instrument response string on the results stack (DEFAULT: +true+)
# - +:store_cmd+(Boolean)::          Store the raw command string sent to the instrument on the results stack (DEFAULT: +true+)
#
# == SCPIsession State Parameters: Macros
#    
# The following pseudo-options are provided for connivance, and are equivalent to setting several other options simultaneously.  Like all option-like
# entities, these pseudo-options take an argument.  This argument *must* be provided even if the macro ignores it.
#
#      - Result processing & Output Control Macros
#                                       :result_extract_block :result_split :result_chomp :result_strip :result_last_word :result_type :print_max_len :print_debug
#        - :result_macro_bin (P) ....... false                 nil           false         false         false             :string      nil            false
#        - :result_macro_block (P) ..... true                  nil           false         false         false             :string      nil            false
#        - :result_macro_ascii (P) ..... false                 nil           true          true          false             :string      nil            false
#        - :result_macro_debug (P) ..... false                 nil           true          true          false             :string      256            true
#        - :result_macro_csv (P) ....... false                 ','           true          true          false             :string      nil            false
#
class SCPIsession
  ################################################################################################################################################################
  def initialize(options)
    @re           = SCPIregexLibrary.instance
    @gblOpt       = Hash.new
    @tcpSocket    = nil
    @results      = Array.new
    @varList      = Hash.new
    @validProto   = [:raw, :soip, :lxi, :plgx, :t3k, :file]
    @validType    = [nil, :mixed, :string, :float, :int, :bool]
    @validSplits  = [nil, :char, :string, :space, :unpack, :block, :regex, :line, :csv, :ssv]
    @gblOptReqPsu = Set.new(PrintyPrintyBangBang.instance.validOptions + [ :url ])
    @gblOptReqNew = { :ip_address               => lambda { |x| (x.is_a?(String)) && (x.length > 1)               },
                      :net_port                 => lambda { |x| (x.is_a?(Integer)) && (x >= 0) && (x <= 65535)    },
                      :net_protocol             => lambda { |x| @validProto.member?(x)                            },
                    }
    @gblOptReqRun = { :cmd                      => lambda { |x| (x.is_a?(String)) && (x.length > 1)               },
                      :delay_after_complete     => lambda { |x| (x.is_a?(Integer)) && (x >= 0)                    },
                      :delay_before_first_read  => lambda { |x| (x.is_a?(Integer)) && (x >= 0)                    },
                      :eol                      => lambda { |x| (x.is_a?(String))                                 },
                      :execute_on_cmd           => lambda { |x| [true, false].member?(x)                          },
                      :name                     => lambda { |x| x.nil? || ((x.is_a?(String)) && (x.length > 0))   },
                      :print_cmd                => lambda { |x| [true, false].member?(x)                          },
                      #print_debug              Part of PrintyPrintyBangBang
                      #print_max_len            Part of PrintyPrintyBangBang
                      :print_raw_result         => lambda { |x| [true, false].member?(x)                          },
                      :print_result             => lambda { |x| [true, false].member?(x)                          },
                      :read_buffer_size         => lambda { |x| (x.is_a?(Integer)) && (x > 0)                     },
                      :read_eot_sentinel        => lambda { |x| x.nil? || ((x.is_a?(String)) && (x.length > 0))   },
                      :read_max_bytes           => lambda { |x| x.nil? || ((x.is_a?(Integer)) && (x > 0))         },
                      :read_retry_delay         => lambda { |x| (x.is_a?(Integer)) && (x >= 0)                    },
                      :read_timeout_first_byte  => lambda { |x| (x.is_a?(Integer)) && (x >= 0)                    },
                      :read_timeout_next_byte   => lambda { |x| (x.is_a?(Integer)) && (x >= 0)                    },
                      :result_chomp             => lambda { |x| [true, false].member?(x)                          },
                      :result_extract_block     => lambda { |x| [true, false].member?(x)                          },
                      :result_last_word         => lambda { |x| [true, false].member?(x)                          },
                      :result_split             => lambda { |x| @validSplits.member?(x)                           },
                      :result_split_arg         => lambda { |x| x.nil? || ((x.is_a?(String)) && (x.length > 0))   },
                      :result_squeeze           => lambda { |x| [true, false].member?(x)                          },
                      :result_strip             => lambda { |x| [true, false].member?(x)                          },
                      :result_type              => lambda { |x| @validType.member?(x)                             },
                      :scpi_prefix              => lambda { |x| x.is_a?(String)                                   },
                      :socket_close             => lambda { |x| [true, false].member?(x)                          },
                      :socket_close_write       => lambda { |x| [true, false].member?(x)                          },
                      :store_cmd                => lambda { |x| [true, false].member?(x)                          },
                      :store_last_in_ans        => lambda { |x| [true, false].member?(x)                          },
                      :store_named_in_var       => lambda { |x| [true, false].member?(x)                          },
                      :store_raw_results        => lambda { |x| [true, false].member?(x)                          },
                      :store_results            => lambda { |x| [true, false].member?(x)                          },
                      :var                      => lambda { |x| x.nil? || ((x.is_a?(String)) && (x.length > 0))   },
                    }

    # Set initial parameter values
    set({ #delay_after_complete     Set from proto
          #delay_before_first_read  Set from proto
          :eol                  =>        "\n",
          :execute_on_cmd       =>        true,
          :name                 =>         nil,
          :exit_0               =>       false,
          :exit_on_error        =>        true,
          :print_cmd            =>       false,
          :print_debug          =>       false,
          :print_max_len        =>         nil,
          :print_raw_result     =>       false,
          :print_result         =>       false,
          :read_buffer_size     =>     1048576,
          :read_eot_sentinel    =>         nil,
          :read_max_bytes       =>         nil,
          #read_retry_delay         Set from proto
          #read_timeout_first_byte  Set from proto
          #read_timeout_next_byte   Set from proto
          :result_chomp         =>       false,
          :result_extract_block =>       false,
          :result_last_word     =>       false,
          :result_split         =>         nil,
          :result_split_arg     =>         nil,
          :result_squeeze       =>       false,
          :result_strip         =>       false,
          :result_type          =>     :string,
          :scpi_prefix          =>          '',
          :socket_close         =>       false,
          :socket_close_write   =>       false,
          :store_cmd            =>        true,
          :store_last_in_ans    =>        true,
          :store_named_in_var   =>        true,
          :store_raw_results    =>        true,
          :store_results        =>        true,
          :var                  =>         nil,
        }.merge(options))
  end

  ################################################################################################################################################################
  # Set state parameters -- see the parameter list in the main documentation for the SCPIsession class.
  def set(options=Hash.new)
    objectUnderConstruction = @gblOpt.empty?
    #---------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Take care of pseudo-option :good_std_eot
    if options.member?(:good_std_eot) then
      if options[:good_std_eot] 
        options[:read_eot_sentinel] = "\n"
      else
        options[:read_eot_sentinel] = nil
      end
      options.delete(:good_std_eot)
    end
    #---------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Take care of pseudo-options :result_macro_*
    { :result_macro_bin   => { :result_extract_block => false, :result_split => nil,  :result_chomp => false, :result_strip => false, :result_last_word => false, :result_type => :string, :print_max_len =>  nil, :print_debug => false },
      :result_macro_block => { :result_extract_block => true,  :result_split => nil,  :result_chomp => false, :result_strip => false, :result_last_word => false, :result_type => :string, :print_max_len =>  nil, :print_debug => false },
      :result_macro_ascii => { :result_extract_block => false, :result_split => nil,  :result_chomp => true,  :result_strip => true,  :result_last_word => false, :result_type => :string, :print_max_len =>  nil, :print_debug => false },
      :result_macro_debug => { :result_extract_block => false, :result_split => nil,  :result_chomp => true,  :result_strip => true,  :result_last_word => false, :result_type => :string, :print_max_len => 1024, :print_debug => true  },
      :result_macro_csv   => { :result_extract_block => false, :result_split => :csv, :result_chomp => true,  :result_strip => true,  :result_last_word => false, :result_type => :string, :print_max_len =>  nil, :print_debug => false },
    }.each do |macro_sym, macro_options|
      if options.member?(macro_sym) then
        macro_options.each do |macro_opt_sym, macro_opt_val|
          options[macro_opt_sym] = macro_opt_val
        end
        options.delete(macro_sym)
      end
    end
    #---------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Take care of pseudo-options for PrintyPrintyBangBang
    PrintyPrintyBangBang.instance.set(options)
    #---------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Take care of pseudo-option :echo
    if options.member?(:echo) then
      options[:print_cmd] = options[:echo]
      options[:print_result] = options[:echo]
      options.delete(:echo)
    end
    #---------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Take care of pseudo-option :url -> zap it and expand it into :net_protocol, :ip_address, :net_port
    if options.member?(:url) then
      urlBits = SCPIrcFile.instance.urlParser(options[:url])
      if !(urlBits.nil?) then
        urlBits.each do |k, v|
          if !(v.nil?) then
            if (options.member?(k)) then
              PrintyPrintyBangBang.instance.logPrinter(3, "INFO: Explicit #{k.inspect} option (#{options[k].inspect}) overrode value in URL: #{options[:url].inspect}", self)
            else
              options[k] = v;
            end
          end
        end
      end
      options.delete(:url)
    end
    #---------------------------------------------------------------------------------------------------------------------------------------------------------------
    # Set values provided in arguments
    options.each do |k, v|
      if @gblOptReqNew.member?(k) || @gblOptReqRun.member?(k) || @gblOptReqPsu.member?(k) then
        if !(objectUnderConstruction) && @gblOptReqNew.member?(k) then
          PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Option can't be set post new: #{k.inspect}", self, 71)
        end
        @gblOpt[k] = v
      else
        PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Unknown Session Option: #{k.inspect} => #{v.inspect}", self, 72)
      end
    end
    #---------------------------------------------------------------------------------------------------------------------------------------------------------------
    # If we are constructing the object, then work harder
    if objectUnderConstruction then
      if !(@gblOpt.member?(:net_protocol)) then
        @gblOpt[:net_protocol] = :raw
      end
      # Apply rules to set unset options
      { :net_port                 => {:file=> nil, :raw=> 5025, :soip=> nil, :plgx=> 1234, :lxi => nil, :t3k=>  80},
        :delay_after_complete     => {:file=>   0, :raw=>    0, :soip=> 100, :plgx=>   50, :lxi =>   0, :t3k=> 200},
        :delay_before_first_read  => {:file=>   0, :raw=>  100, :soip=>  50, :plgx=>   10, :lxi =>   0, :t3k=>   0},
        :read_retry_delay         => {:file=>   0, :raw=>  200, :soip=> 200, :plgx=>  100, :lxi =>   0, :t3k=>   0},
        :read_timeout_first_byte  => {:file=>   0, :raw=> 1000, :soip=> 500, :plgx=>  600, :lxi =>   0, :t3k=>   0},
        :read_timeout_next_byte   => {:file=>   0, :raw=> 2000, :soip=> 100, :plgx=>   50, :lxi =>   0, :t3k=>   0}
      }.each do |k, defs|
        if !(@gblOpt.member?(k)) || @gblOpt[k].nil? then
          @gblOpt[k] = defs[@gblOpt[:net_protocol]]
          if @gblOpt[k].nil? then
            PrintyPrintyBangBang.instance.logPrinter(2, "WARNING: Could not set #{k} to a default value!", self)
          else
            PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Successfully set #{k} to a default value!", self)
          end
        end
      end
      # Make sure all options required by new are set and valid
      @gblOptReqNew.each do |k, validator|
        if !(validator.call(@gblOpt[k])) then
          PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Option required by new is invalid: #{k} = #{@gblOpt[k].inspect}!", self, 73)
        end
      end
      if [:lxi].member?(@gblOpt[:net_protocol]) then # (TODO: Future Feature)
        PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: :lxi is not currently supported -- it will be someday!!", self, 86)
      end
      PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Object fully constructed.", self)
    else
    end
    #---------------------------------------------------------------------------------------------------------------------------------------------------------------
    @gblOpt.keys.sort.each do |k|
      PrintyPrintyBangBang.instance.logPrinter(8, "DEBUG-8: @gblOpt: #{(k.to_s+' ').ljust(25, '.')} #{@gblOpt[k].inspect}", self)
    end
    #---------------------------------------------------------------------------------------------------------------------------------------------------------------
    # If we got a :cmd & :execute_on_cmd is on, then we attempt to execute
    if options.member?(:cmd) && @gblOpt[:execute_on_cmd] then
      PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Attempting automatic execution", self)
      return execute()
    end

  end

  ################################################################################################################################################################
  # While <tt>.set</tt> perfectly capable of setting the <tt>:cmd</tt> parameter, this method provides provides a more readable alternative.
  def command(cmd, options=nil)
    if options && options.member?(:cmd) then
      PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: :cmd must not appear in options list when calling 'command'!", self, 74)
    end
    # Set options first -- so we don't trigger automatic execution.
    if options
      set(options)
    end
    # Now set command by ourselves without the set method so we can execute the command ourselves and return a result if :execute_on_cmd is on
    @gblOpt[:cmd] = cmd
    if @gblOpt[:execute_on_cmd] then
      PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Attempting automatic execution", self)
      return execute()
    end
  end

  ################################################################################################################################################################
  # Access, or set when <tt>value</tt> is non-<tt>nil</tt>, a SCPIsession variable.
  def variable (name, value=nil)
    if value.nil? then
      if @varList.member?(name) then
        return @varList[name]
      else
        PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Result not found with given name: #{name.inspect}!", self, 75)
      end
    else
      @varList[name]  = value
      return value
    end
  end

  ################################################################################################################################################################
  # Return a new string with constructs like ${variable_name} & ${variable_name:default_value} expanded into the value of the variable(s).
  def expand (instr)
    expCnt = 0
    instr.gsub!(@re.a(:mrs_varexp)) do |m|
      varName    = $1
      varDefault = $2
      expCnt += 1
      if @varList.member?(varName) then
        next @varList[varName]
      else
        if varDefault then
          next varDefault
        else
          PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Unknown variable (#{varName}) without default value!", self, 88)
          next s
        end
      end
    end
    return instr
  end

  ################################################################################################################################################################
  # This method executes the command specified by the <tt>:cmd</tt> parameter.  This is method is completely unnecessary if the <tt>:execute_on_cmd</tt>
  # parameter is non-<tt>nil</tt>.
  def execute()
    time_exe_start = Time.now
    # Make sure all required options required by new are set and valid
    @gblOptReqRun.each do |k, validator|
      if !(validator.call(@gblOpt[k])) then
        PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Unable to execute.  Execution option is invalid: #{k.inspect} = #{@gblOpt[k].inspect}!", self, 76)
      end
    end
    PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Execution options are good", self)
    # We are good to go!
    curCmd = @gblOpt[:cmd]
    strToSend = @gblOpt[:scpi_prefix] + @gblOpt[:cmd]
    if @gblOpt[:net_protocol] == :plgx then
      strToSend = strToSend + "\n"
    else
      strToSend = strToSend + @gblOpt[:eol]
    end
    if @gblOpt[:print_cmd] then
      PrintyPrintyBangBang.instance.outPrinter(">>#{strToSend.chomp.strip}>>", newline=true)
    end
    strWeGot = ''
    if @gblOpt[:net_protocol] == :file then
      PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Opening file", self)
      begin
        strWeGot = open(curCmd, "rb").read();
      rescue
        PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Unable to open file #{curCmd}!", self, 87)
      end
      if @gblOpt[:print_raw_result] then
        PrintyPrintyBangBang.instance.outPrinter(strWeGot, newline=false)
      end
    else
      if [:raw, :soip, :plgx].member?(@gblOpt[:net_protocol]) then
        if @tcpSocket.nil? then
          PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Opening Socket", self)
          begin
            @tcpSocket = TCPSocket.open(@gblOpt[:ip_address], @gblOpt[:net_port])
          rescue
            PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Unable to open TCP/IP socket!", self, 82)
          end
          if @gblOpt[:net_protocol] == :plgx then
            PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Sending :plgx startup instructions", self)
            @tcpSocket.write("++mode 1\n")
            @tcpSocket.write("++savecfg 0\n")
            @tcpSocket.write("++auto 0\n")
            @tcpSocket.write("++eoi 1\n")
            # We only do these when we open the socket.  Unfortunatly that means we miss :eol and :read_timeout_next_byte changes after socket open... BUG TODO
            @tcpSocket.write("++read_tmo_ms #{[50, @gblOpt[:read_timeout_first_byte]-50].max}\n")
            @tcpSocket.write("++eos " + ({"\0d\0a" => '0', "\0d" => '1', "\0a" => '2'}[@gblOpt[:eol]] || '3') + "\n")
            #@tcpSocket.write("++ifc\n")
          end
        end
        PrintyPrintyBangBang.instance.logPrinter(3, "INFO: Sending Command: #{strToSend.inspect}", self)
        @tcpSocket.write(strToSend)
        if !(@gblOpt[:result_type].nil?) && (@gblOpt[:net_protocol] == :plgx) then
          PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Sending :plgx ++read instruction.", self)
          @tcpSocket.write("++read eoi\n")
        end
        if @gblOpt[:socket_close_write] then
          PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Closing TCP write connection.", self)
          @tcpSocket.close_write()
        end
        if !(@gblOpt[:result_type].nil?) then
          if @gblOpt[:delay_before_first_read] > 0 then
            PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: DELAY: Pre-read delay_time = #{@gblOpt[:delay_before_first_read]}ms.", self)
            sleep(@gblOpt[:delay_before_first_read]/1000.0)
          end
          PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Read loop starting...", self)
          time_send = Time.now
          time_read = nil
          total_recvd = 0
          loop do
            begin
              recvd_data = @tcpSocket.recv_nonblock(@gblOpt[:read_buffer_size])
              strWeGot += recvd_data
              recvd_length = recvd_data.length()
              total_recvd += recvd_length
              if time_read.nil? then
                PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Received #{recvd_length} bytes in first read.", self)
              else
                PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Received #{recvd_length} bytes in last read.  Total of #{total_recvd} so far.", self)
              end
              time_read = Time.new
              if @gblOpt[:print_raw_result] then
                PrintyPrintyBangBang.instance.outPrinter(recvd_data, newline=false)
              end
              if recvd_length < 1 then
                if total_recvd == 0 then
                  PrintyPrintyBangBang.instance.logPrinter(3, "INFO: Received no data.  Transmission complete.", self)
                else
                  PrintyPrintyBangBang.instance.logPrinter(3, "INFO: Received nothing in last read.  Total of #{total_recvd}.  Transmission complete.", self)
                end
                break
              end
              if @gblOpt[:read_eot_sentinel] && recvd_data.include?(@gblOpt[:read_eot_sentinel]) then
                PrintyPrintyBangBang.instance.logPrinter(3, "INFO: Received #{total_recvd} bytes and EOT sentinel.  Transmission complete.", self)
                break
              end
              if @gblOpt[:read_max_bytes] && (total_recvd >= @gblOpt[:read_max_bytes]) then
                if total_recvd == @gblOpt[:read_max_bytes] then
                  PrintyPrintyBangBang.instance.logPrinter(3, "INFO: Received #{total_recvd}.  As expected.  Transmission complete.", self)
                else
                  PrintyPrintyBangBang.instance.logPrinter(2, "WARNING: Received #{total_recvd}.  Only expected #{@gblOpt[:read_max_bytes]}.  Transmission complete.", self)
                end
                break
              end
            rescue IO::WaitReadable
              time_now = Time.now
              if time_read.nil? then
                delta = time_now - time_send
                if delta > 1 then
                  PrintyPrintyBangBang.instance.logPrinter(2, "WARNING: Instrument has not responded in #{delta} seconds", self)
                else
                  PrintyPrintyBangBang.instance.logPrinter(3, "INFO: Instrument has not responded in #{delta} seconds", self)
                end
                if @gblOpt[:read_timeout_first_byte] < (delta*1000) then
                  PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Giving up due to first byte timeout", self, nil);
                  break
                else
                  if @gblOpt[:read_retry_delay] > 0 then
                    PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: DELAY: io_retry_delay = #{@gblOpt[:delay_before_first_read]}ms.", self)
                    sleep(@gblOpt[:read_retry_delay]/1000.0)
                  end
                end
              else
                delta = time_now-time_read
                if delta > 1 then
                  PrintyPrintyBangBang.instance.logPrinter(3, "INFO: Received #{total_recvd} bytes. No new data received for #{delta} seconds", self)
                else
                  PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Received #{total_recvd} bytes. No new data received for #{delta} seconds", self)
                end
                if @gblOpt[:read_timeout_next_byte] < (1000*delta) then
                  PrintyPrintyBangBang.instance.logPrinter(3, "INFO: Received #{total_recvd} bytes. Assuming no more data to receive.", self);
                  break
                else
                  if @gblOpt[:read_retry_delay] > 0 then
                    PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: DELAY: io_retry_delay = #{@gblOpt[:delay_before_first_read]}ms.", self)
                    sleep(@gblOpt[:read_retry_delay]/1000.0)
                  end
                end
              end
              retry
            end
          end
        else
          PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Not waiting for response from instrument (:result_type nil).", self)
        end
        if @gblOpt[:socket_close] || @gblOpt[:socket_close_write] then
          PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Closing TCP connection.", self)
          @tcpSocket.close()
          @tcpSocket = nil
        end
      elsif @gblOpt[:net_protocol] == :t3k then
        PrintyPrintyBangBang.instance.logPrinter(3, "INFO: Sending Command: #{strToSend.inspect}", self)
        res = Net::HTTP.post_form(URI.parse("http://#{@gblOpt[:ip_address]}:#{@gblOpt[:net_port]}/Comm.html"), { 'COMMAND' => strToSend })
        if !(res.nil?) then
          if !(@gblOpt[:result_type].nil?) then
            strWeGot = res.body.sub(/^.*NAME="name">/m, '').sub(/<\/TEXTAREA.*$/m, '')
            if strWeGot.length > 0 then
              if @gblOpt[:result_type] && @gblOpt[:print_raw_result] then
                PrintyPrintyBangBang.instance.outPrinter(strWeGot, newline=false)
              end
            end
          end
        else
          PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: unknown protocol: #{@gblOpt[:net_protocol].inspect}", self, 77)
        end
      end
    end
    time_exe_done = Time.now
    # Command execution is complete
    PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Raw result string: #{strWeGot[0..100].inspect}", self)
    PrintyPrintyBangBang.instance.logPrinter(3, "INFO: Execution time: #{time_exe_done - time_exe_start}", self)

    thingyWeReturn = strWeGot
    if (@gblOpt[:result_extract_block]) && (thingyWeReturn.length >= 2) && (thingyWeReturn[0] == '#') then
      blockHeadLen = thingyWeReturn[1, 1].to_i(16)
      if (blockHeadLen.zero?) then
        thingyWeReturn = thingyWeReturn.slice(2)
      else
        blockBodyLen = thingyWeReturn[2, blockHeadLen].to_i()
        thingyWeReturn = thingyWeReturn[blockHeadLen+2, blockBodyLen]
      end
    end
    case @gblOpt[:result_split]
      when :char
        thingyWeReturn = thingyWeReturn.split(Regexp.new("[#{@gblOpt[:result_split_arg]}]", Regexp::EXTENDED | Regexp::MULTILINE))
      when :regex
        thingyWeReturn = thingyWeReturn.split(Regexp.new(@gblOpt[:result_split_arg],        Regexp::EXTENDED | Regexp::MULTILINE))
      when :space
        thingyWeReturn = thingyWeReturn.split(@re.a(:split_space))
      when :line
        thingyWeReturn = thingyWeReturn.split(@re.a(:split_line))
      when :csv
        thingyWeReturn = thingyWeReturn.split(@re.a(:split_csv))
      when :ssv
        thingyWeReturn = thingyWeReturn.split(@re.a(:split_ssv))
      when :string
        thingyWeReturn = thingyWeReturn.split(@gblOpt[:result_split_arg])
      when :unpack
        thingyWeReturn = thingyWeReturn.unpack(@gblOpt[:result_split_arg])
      when :block
        if (thingyWeReturn.length >= 2) && (thingyWeReturn[0] == '#') then
          blockHeadLen = thingyWeReturn[1, 1].to_i(16)
          if blockHeadLen.zero? then
            thingyWeReturn = [ thingyWeReturn[0, 2], thingyWeReturn.slice(2) ]
          else
            blockBodyLen = thingyWeReturn[2, blockHeadLen].to_i()
            thingyWeReturn = [ thingyWeReturn[0, blockHeadLen+2], thingyWeReturn[blockHeadLen+2, blockBodyLen] ]
          end
        else
          thingyWeReturn = [ "", thingyWeReturn ]
        end
      else
        thingyWeReturn = [ thingyWeReturn ]
    end
    if @gblOpt[:result_squeeze] then
      thingyWeReturn = thingyWeReturn.map { |x| (x.is_a?(String) ? x.gsub(@re.a(:split_space)) { |m| (m.match?(@re.a(:split_line)) ? "\n" : " "); } : x) }
    end
    if @gblOpt[:result_chomp] then
      thingyWeReturn = thingyWeReturn.map { |x| (x.is_a?(String) ? x.chomp : x) }
    end
    if @gblOpt[:result_strip] then
      thingyWeReturn = thingyWeReturn.map { |x| (x.is_a?(String) ? x.strip : x) }
    end
    if @gblOpt[:result_last_word] then
      thingyWeReturn = thingyWeReturn.map { |x| (x.is_a?(String) ? x.rstrip.sub(/^.* /, '') : x) }
    end
    if @gblOpt[:result_type] != :mixed then  # It can't be nil if we get to this line
      case @gblOpt[:result_type]
        when :string
          thingyWeReturn = thingyWeReturn.map(&:to_s)
        when :float
          thingyWeReturn = thingyWeReturn.map { |x| ((x.is_a?(String) || x.is_a?(Numeric)) ? x.to_f : x) }
        when :int
          # TODO: Add support for hex, oct, & binary integers
          thingyWeReturn = thingyWeReturn.map { |x| ((x.is_a?(String) || x.is_a?(Numeric)) ? x.to_f : x) }
        when :bool
          thingyWeReturn = thingyWeReturn.map { |x| (x.is_a?(String) ? (x.strip.match?(@re.a(:o488_TRUEx)) ? true : (x.strip.match?(@re.a(:o488_FALSEx)) ? false : nil)) : x) }
      end
    end
    if !(@gblOpt[:result_split]) then  # If we didn't split, then we return a scaler
      thingyWeReturn = thingyWeReturn.first
    end
    PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: Result we return: #{thingyWeReturn.to_s[0..100].inspect}", self)
    @results.push( { :name    => @gblOpt[:name],
                     :cmd_str => (@gblOpt[:store_cmd]         ? strToSend      : nil),
                     :ret_val => (@gblOpt[:store_results]     ? thingyWeReturn : nil),
                     :raw_str => (@gblOpt[:store_raw_results] ? strWeGot       : nil)
                   })
    if @gblOpt[:store_last_in_ans] then
      @varList['ANS'] = thingyWeReturn
    end
    if @gblOpt[:store_named_in_var] && @gblOpt[:name] then
      @varList[@gblOpt[:name]] = thingyWeReturn
    end
    if @gblOpt[:result_type] && @gblOpt[:print_result] then
      PrintyPrintyBangBang.instance.outPrinter(thingyWeReturn.to_s, newline=false)
    end
    if @gblOpt[:delay_after_complete] > 0 then
      PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: DELAY: between_timeout = #{@gblOpt[:delay_after_complete]}ms.", self)
      sleep(@gblOpt[:delay_after_complete]/1000.0)
    end
    return thingyWeReturn
  end

  ################################################################################################################################################################
  # After execution SCPI commands have several data elements stored, and this method provides access to that data.
  # - +index+:: Specify SCPI command for which to return data.
  #   - An integer: Return the result at the given array index. Some notes:
  #     - This option is fast at O(1).
  #     - <tt>-1</tt> is the last SCPI command
  #     - +0+ is the first one. 
  #   - A string: Return an <b>array</b> of all results with a matching +:name+ ordered by index.  Some notes:
  #     - An empty array is returned if no matches are found. 
  #     - This option is slow at O(n) -- n # results.
  #     - The option +:store_named_in_var+ and the #variable method are much faster; however, this only gets tht <b>last</b> result with the given name.
  # - +type+:: A symbol specifing the data to return. The symbol must be one of: 
  #   - +:all+::      A hash with all data
  #   - +:name+::     String with the command name/variable
  #   - +:cmd_str+::  String sent to the instrument
  #   - +:ret_val+::  Processed result from the command
  #   - +:raw_str+::  String received from the instrument
  def result(index: -1, type: :ret_val)
    if index.instance_of?(String) then
      ret = @results.find_all { |x| x[:name] == index }
      if type == :all then
        return ret.clone
      else
        if !(ret.empty?) then
          return ret.map { |x| x[type] }.clone
        else
          return ret.clone
        end
      end
    else
      if type == :all then
        return @results[index].clone
      else
        return @results[index][type]
      end
    end
  end

end

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
# = Introduction
#
# SCPIsequence is essentially a container for an ordered sequence of SCPIsession options (added via #SCPIsequence.add) that may eventually be applied to an
# SCPIsession in order (via SCPIsequence.execute).  In addition to SCPIsession options, this class may also house a few additional parameters which provide
# various programming constructs: variables (with comprehensive substitution & ruby code execution), conditionals, & jumps.  With these additions, the mrSCPI
# scripting language becomes Turing complete, and an SCPIsequence can used to house full scripts.
#
# = Additional "options" for an SCPIsequence
#
# - +:eval+::    <tt>{VAR}={RUBY_CODE}</tt>
# - +:var+::     <tt>{VAR}={VALUE}</tt>
# - +:skip_if+:: <tt>{LHS}{=|!=|~|!~}{RHS}</tt>
# - +:next_if+:: <tt>{LHS}{=|!=|~|!~}{RHS}</tt>
# - +:stop+::    +[MESSAGE]+ -- +MESSAGE+ is required when +:stop+ is given on the command line!
# - +:print+::   +[MESSAGE]+ -- +MESSAGE+ is required when +:stop+ is given on the command line!
# - +:goto+::    <tt>{LABEL}</tt>
# - +:lab+::     <tt>{LABEL}</tt>
#
# == Use
#
# - Create a new SCPIsequence object.
# - Repeatedly call the #add method to add options.
# - When done adding options, call the #execute method to run the sequence.
#
# === How mrSCPI uses SCPIsequence
#
# - For mrSCPI scripts::       The script file is read, and #add is called for each line.  Once the file is read, the #execute method is called.
# - For command line scripts:: Command line arguments added to an SCPIsequence in order.  Once all the arguments are processed, the #execute method is called.
class SCPIsequence
  def initialize (convertStrings=nil)
    # Store the instance to shorten lines referencing it
    @re           = SCPIregexLibrary.instance
    # These are only passed to the session constructor
    @optsForNew  = Set.new([:ip_address, :net_port, :net_protocol, :url])
    # These are sequence options not session options
    @optsForSeq  = Set.new([:skip_if, :next_if, :stop, :print, :goto, :lab, :var, :eval])
    # How we convert strings to option values.
    @optValToStr = { :cmd                     => lambda { |x| x                                                         }, # Properties
                     :delay_after_complete    => lambda { |x| x.to_i                                                    },
                     :delay_before_first_read => lambda { |x| x.to_i                                                    },
                     :echo                    => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :result_macro_bin        => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :result_macro_block      => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :result_macro_ascii      => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :result_macro_debug      => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :result_macro_csv        => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :good_std_eot            => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :eol                     => lambda { |x| "\"#{x}\"".undump                                         },
                     :eval                    => lambda { |x| md=x.match(@re.a(:mrs_assign)); [md[1], md[2]]            },
                     :execute_on_cmd          => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :exit_0                  => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :exit_on_error           => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :goto                    => lambda { |x| x                                                         },
                     :ip_address              => lambda { |x| x                                                         }, # new properties
                     :lab                     => lambda { |x| x                                                         },
                     :log_file                => lambda { |x| x                                                         },
                     :name                    => lambda { |x| (x=='' ? nil : x)                                         },
                     :net_port                => lambda { |x| x.to_i                                                    },
                     :net_protocol            => lambda { |x| x.sub(/^:/, '').to_sym                                    },
                     :next_if                 => lambda { |x| md=x.match(@re.a(:mrs_branch)); [md[1], md[2], md[3]]     },
                     :out_file                => lambda { |x| x                                                         },
                     :print                   => lambda { |x| x                                                         },
                     :print_cmd               => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :print_raw_result        => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :print_result            => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :read_buffer_size        => lambda { |x| x.to_i                                                    },
                     :read_eot_sentinel       => lambda { |x| ( ['', 'nil'].member?(x) ? nil : "\"#{x}\"".undump )      },
                     :read_max_bytes          => lambda { |x| ( ['0', 'nil'].member?(x) ? nil : x.to_i )                },
                     :print_max_len           => lambda { |x| ( ['0', 'nil'].member?(x) ? nil : x.to_i )                },
                     :print_debug             => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :read_retry_delay        => lambda { |x| x.to_i                                                    },
                     :read_timeout_first_byte => lambda { |x| x.to_i                                                    },
                     :read_timeout_next_byte  => lambda { |x| x.to_i                                                    },
                     :result_squeeze          => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :result_chomp            => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :result_last_word        => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :result_extract_block    => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :result_split            => lambda { |x| (x.nil? ? nil : x.sub(/^:/, '').to_sym)                   },
                     :result_split_arg        => lambda { |x| "\"#{x}\"".undump                                         },
                     :result_strip            => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :result_type             => lambda { |x| (x.match?(/^n/i) ? nil : x.sub(/^:/, '').to_sym)          },
                     :scpi_prefix             => lambda { |x| x                                                         },
                     :seq_verbose             => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :skip_if                 => lambda { |x| md=x.match(@re.a(:mrs_branch)); [md[1], md[2], md[3]]     },
                     :socket_close            => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :socket_close_write      => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :stop                    => lambda { |x| x                                                         },
                     :store_cmd               => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :store_raw_results       => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :store_results           => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :store_named_in_var      => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :store_last_in_ans       => lambda { |x| !x.match?(@re.a(:o488_FALSEx))                            },
                     :url                     => lambda { |x| x                                                         },
                     :var                     => lambda { |x| md=x.match?(@re.a(:mrs_assign)); [md[1], md[2]]           },
                     :verbose                 => lambda { |x| x.to_i                                                    },
                   }
    @newOpts = Hash.new
    @seqOpts = Array.new
    @convertStringsP = convertStrings
  end

  ################################################################################################################################################################
  # Return true if property is valid, and nil if not -- note :execute_on_cmd is not allowed
  def add (property, value)
    if @convertStringsP && (property.is_a?(String)) then
      property = property.sub(/^(--|:)/, '')
      property = property.to_sym
    end
    if (property != :execute_on_cmd) && @optValToStr.member?(property) then
      if @convertStringsP && (value.is_a?(String)) then
        value = @optValToStr[property].call(value.strip)
      end
      if @optsForNew.member?(property) then
        if @newOpts.member?(property) then
          PrintyPrintyBangBang.instance.logPrinter(2, "WARNING: Ignoring: #{property.inspect} => #{value.inspect}.  Using value: #{@newOpts[property].inspect}", self)
        else
          @newOpts[property] = value
        end
      else
        @seqOpts.push([property, value])
      end
      return true
    else
      return nil
    end
  end

  ################################################################################################################################################################
  # Execute the sequence of stored parameters
  def execute ()
    theSession = SCPIsession.new(@newOpts)
    theSession.set(:execute_on_cmd => true)
    labelIndex = Hash.new
    lookingForLab = nil
    skipStep = false
    currentLineNumber = 0
    while currentLineNumber <= (@seqOpts.length-1) do
      (par, val) = @seqOpts[currentLineNumber]
      PrintyPrintyBangBang.instance.logPrinter(4, "DEBUG-4: LINE: #{currentLineNumber} : #{@seqOpts[currentLineNumber].inspect}", self)
      if skipStep then
        skipStep = false
      else
        if !(lookingForLab) || ((par == :lab) && (lookingForLab == val)) then
          if @optsForSeq.member?(par) then
            if (par == :skip_if) || (par == :next_if) then
              tmpLHS = theSession.expand(val[0])
              tmpRHS = theSession.expand(val[2])
              pred = ( val[1].match?("=") ? (tmpLHS == tmpRHS) : !!(tmpLHS.match?(tmpRHS)) )
              pred = ( val[1].match?("!") ? !(pred) : pred )
              skipStep = ((par == :skip_if) && pred) || ((par == :next_if) && !(pred))
            elsif par ==  :stop then
              if !(val.empty?) then
                tmpMSG = theSession.expand(val)
                PrintyPrintyBangBang.instance.outPrinter(tmpMSG, newline=true)
              end
              break
            elsif par ==  :print then
              tmpMSG = theSession.expand(val)
              PrintyPrintyBangBang.instance.outPrinter(tmpMSG, newline=true)
            elsif par ==  :goto then
              if labelIndex.member?(val) then
                currentLineNumber = labelIndex[val] # Jump back
              else
                lookingForLab = val                 # Jump forward
              end
              next
            elsif par ==  :lab then
              lookingForLab = nil
              labelIndex[val] = currentLineNumber
            elsif (par == :var) || (par == :eval) then
              tmpVAL = theSession.expand(val[1])
              if par == :eval then
                tmpVAL = eval(tmpVAL).to_s
              end
              theSession.variable(val[0], tmpVAL)
            end
          else
            tmpVal = (val.is_a?(String) ? theSession.expand(val) : val)
            theSession.set(par => tmpVal)
          end
        end
      end
      currentLineNumber += 1
    end
  end

end

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
# = Introduction
#
# SCPIrcFile provides access to data housed in the mrSCPI RC file found at <tt>$HOME/.mrSCPIrc</tt> or at <tt>$MRSCPIRC</tt>.
#
# = RC File Content
#
# The file has (for now) two kinds of lines: SCPIrcFile@Nickname & SCPIrcFile@Network.
#
# == Nickname
#
# Nickname lines look like this:
#   <space_separated_list_of_nicknames> => <space_separated_list_of_network_annotated_urls>
# The components are as follows:
# - <tt>nickname</tt>:: Is a sequence of non-whitespace characters.  It may not contain <tt>@</tt>, <tt>:</tt>, or <tt>/</tt>.
# - <tt>network_annotated_url</tt>:: Is of the form: <tt>[network_name]@[net_protocol://]ip_address[:net_port]</tt>
#   - <tt>net_protocol</tt>::          Is one of <tt>raw</tt>, <tt>soip</tt>, <tt>lxi</tt>, or <tt>t3k</tt>.  
#   - <tt>net_port</tt>::              Is an integer.
#   - <tt>ip_address</tt>::            May be an IP address or host DNS name.
#
# == Network
#
# Network lines look like this:
#   <network_name> <space_separated_list_of_networks>
# The components are as follows:
# - <tt>network_name</tt>::          Is a sequence of non-whitespace characters.  It may not contain <tt>@</tt>, <tt>:</tt>, or <tt>/</tt>.
#
# == Use
#
# In general SCPIrcFile provides three services:
# - #urlParser::         Parse URLs, expanding nicknames as required.
# - #lookupURLnickname:: Lookup nickname in the RC file
# - #lookupNetwork::     Lookup networks in the RC file (given an IP address or automatically querying the current host's IP)
#
# When nickname strings provided as arguments have no explicitly given network, then the network lines in the RC file are used to try and find one.  The host
# IP addresses are tested for inclusion in each domain listed in the RC file -- in the order they are found in the file.  If the host has an IP address that
# is included in one of the networks, then that network name is used for the nickname lookup.  If nothing is found, then the network name is left as the empty
# string -- so it will match a <tt>network_annotated_url</tt> with an empty network.
class SCPIrcFile
  include Singleton

  ################################################################################################################################################################
  def initialize ()
    @rcNetworks     = nil
    @rcURLnicknames = nil
    if ENV['HOME'] || ENV['MRSCPIRC'] then
      configFileName = ENV['MRSCPIRC'] || (ENV['HOME'] + '/.mrSCPIrc')
      if FileTest.exist?(configFileName) then
        begin
          configFileFD = open(configFileName, "r")
        rescue
          PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Failed to open RC file!", self, 83)
        end
        if(configFileFD) then
          @rcNetworks     = Array.new
          @rcURLnicknames = Hash.new
          configFileFD.each_line do |line|
            line = line.chomp.gsub(/#.*/, '').downcase.strip.gsub(/  +/, ' ')
            configLineMatchData = line.match(/^([a-zA-Z]+) (.+)$/)
            if configLineMatchData then
              if configLineMatchData[1] == 'nickname' then
                if configLineMatchData[2].match?('=>') then
                  (aliases, urls) = configLineMatchData[2].split('=>').map { |x| x.strip.split(/ +/) }
                  aliases.each do |a|
                    @rcURLnicknames[a] = (urls.map {|e| ( e.match?('@') ? e : "@#{e}" ).split('@') }).to_h
                  end
                else
                  PrintyPrintyBangBang.instance.logPrinter(2, "WARNING: RC file: Syntax error in nickname: #{line.inspect}", self)
                end
              elsif configLineMatchData[1] == 'network' then
                netSpecs = configLineMatchData[2].split(/ +/)
                netName  = netSpecs.shift
                netSpecs = netSpecs.map { |x| IPAddr.new(x) }
                @rcNetworks.push([netName, netSpecs])
              else
                PrintyPrintyBangBang.instance.logPrinter(2, "WARNING: RC file: Unrecognized configuration statement: #{line.inspect}", self)
              end
            else
              if line != '' then
                PrintyPrintyBangBang.instance.logPrinter(2, "WARNING: RC file: Syntax error: #{line.inspect}", self)
              end
            end
          end
          configFileFD.close
        else
          PrintyPrintyBangBang.instance.logPrinter(2, "WARNING: RC file: Unable to open file: #{configFileName.inspect}", self)
        end
      else
        PrintyPrintyBangBang.instance.logPrinter(2, "WARNING: RC file missing.", self)
      end
    else
      PrintyPrintyBangBang.instance.logPrinter(2, "WARNING: Can't load RC file (no HOME or MRSCPIRC variable).", self)
    end
  end

  ################################################################################################################################################################
  # Helper function to lookup instrument nicknames to URLs.  The <tt>nicknameString</tt> can take one of three forms:
  # - <tt>network@nickname</tt>
  # - <tt>@nickname</tt>
  # - <tt>nickname</tt>
  # <tt>SCPIsession</tt> only uses the first two forms.
  # Returns an <tt>nil</tt> if the lookup failed.  Otherwise returns the match as a string.
  def lookupURLnickname (nicknameString)
    if @rcURLnicknames then
      nicknameMatch = nicknameString.match(SCPIregexLibrary.instance.a(:mrs_nickname))
      if nicknameMatch then
        inNetwork  = (nicknameMatch[1].nil? ? '' : nicknameMatch[1].downcase)
        inName     = nicknameMatch[2].downcase

        # Now figure out what network we should use
        if inNetwork == '' then
          inNetwork = lookupNetwork() || inNetwork
        end
        # Now we look for our nickname
        if @rcURLnicknames.empty? then
          PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: No nicknams loaded.", self, 78)
          return nil
        elsif @rcURLnicknames.member?(inName) && @rcURLnicknames[inName].member?(inNetwork) then
          return @rcURLnicknames[inName][inNetwork]
        else
          PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Nickname was not found in the RC file", self, 79)
          return nil
        end
      else
        PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Invalid nickname string", self, 80)
        return nil
      end
    else
      PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: RC config file errors", self, 81)
      return nil
    end
  end

  ################################################################################################################################################################
  # Find a network in the RC file that contains the given <tt>ip_address</tt>. If <tt>ip_address</tt> is <tt>nil</tt>, then the IP addresses of the
  # current host are used.  If nothing is found, then an empty string is returned.
  def lookupNetwork (ip_address=nil)
    if !(@rcNetworks.empty?) then
      if ip_address.nil? then
        ip_address = Socket.ip_address_list.select(&:ipv4?).map { |x| IPAddr.new(x.ip_address) }
      end
      dMatch = @rcNetworks.find { |network, nets| nets.any? { |n| ip_address.any? { |a| n.include?(a) } } }
      if dMatch then
        return dMatch.first
      end
    end
    return ''
  end

  ################################################################################################################################################################
  # Parse URLs.  Lookup nicknames if requried.  Return nil if urlString is invalid and a hash otherwise
  def urlParser (urlString)
    if urlString.match?('@') then
      urlString = SCPIrcFile.instance.lookupURLnickname(urlString)
      if urlString.nil? then
        return nil
      end
    end
    urlMatch = urlString.match(SCPIregexLibrary.instance.a(:mrs_url))
    if urlMatch then
      return { :url          => urlString,
               :net_protocol => (urlMatch[1] && urlMatch[1].to_sym),
               :ip_address   => urlMatch[2],
               :net_port     => (urlMatch[3] && urlMatch[3].to_i) }
    else
      PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: :url was malformed: #{urlString.inspect}", self, 70)
      return nil
    end
  end

end

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
# = Introduction
#
# PrintyPrintyBangBang is a funny little class designed to do two things:
# - PrintyPrinty:: Print things via #logPrinter
# - BangBang:: Trigger abnormal process exits via #logPrinter
#
# = Use
#
# A typical mrSCPI application will not directly use PrintyPrintyBangBang instead depending on a SCPIsession instance to manage the global
# PrintyPrintyBangBang instance.  For example, while PrintyPrintyBangBang maintains a list of global options and a PrintyPrintyBangBang#set method, most
# mrSCPI applications will use the SCPIsession#set in the SCPIsession class -- which will forward the appropriate options to the global PrintyPrintyBangBang
# instance.  That said, it is perfectly acceptable to use PrintyPrintyBangBang directly -- if you wish to log something outside of the SCPIsession for
# example.
#
# PrintyPrintyBangBang is a Singleton.  The normal way to use it is as follows
#    PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: :url was malformed: #{urlString.inspect}", self, 70)
#
# Note that when PrintyPrintyBangBang is made aware of a new output file, it will open that file and <b>keep it open</b> until #close is called or another
# file is specified.  Output files are specified by *strings* -- not file handles!
#
class PrintyPrintyBangBang
  include Singleton

  ################################################################################################################################################################
  def initialize ()
    @gblOpt    = { :log_file       => 'STDERR',
                   :out_file       => 'STDOUT',
                   :verbose        => 1,
                   :exit_0         => false,
                   :exit_on_error  => false,
                   :print_max_len  => 0,
                   :print_debug    => false
                 }
    @openFiles = { 'STDERR'    => STDERR,
                   'STDOUT'    => STDOUT,
                   '/dev/null' => nil
                 }
  end

  ################################################################################################################################################################
  # Return an array of valid option keys.
  #
  # This is used by SCPIsession to forward options to the global PrintyPrintyBangBang instance.
  def validOptions()
    return @gblOpt.keys
  end

  ################################################################################################################################################################
  # Set options in <tt>@gblOpt</tt>.  Possible options:
  # - +:log_file+::      
  # - +:out_file+::      
  # - +:verbose+::       
  # - +:exit_0+::        
  # - +:exit_on_error+:: 
  # - +:print_max_len+:: 
  # - +:print_debug+::   
  #
  # See the SCPIsession class for details on these options.
  #
  # In normal mrSCPI operation, one sets these options on a SCPIsession#set, and the SCPIsession object will propagate the options through to the global
  # PrintyPrintyBangBang instance.  That said, in Ruby programs using SCPIsession, one can directly call PrintyPrintyBangBang#set if desired.
  def set(options)
    optionsWacked = Array.new
    [ :log_file, :out_file].each do |k|
      if options.member?(k) then
        if !(@openFiles.member?(options[k])) then
          begin
            @openFiles[options[k]] = open(options[k], "wb")
          rescue
            @openFiles.delete(options[k])
            # Note: The previous :log_file will still be set at this point, so hopefully we can print this error
            PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Failed to open file: #{options[k].inspect} for #{k.inspect}!", self, 84)
          end
        end
        optionsWacked.push(k)
      end
    end
    options.each do |opt, val|
      if @gblOpt.member?(opt) then
        @gblOpt[opt] = val
        optionsWacked.push(opt)
        options.delete(opt)
      end
    end
    return optionsWacked
  end

  ################################################################################################################################################################
  # Print a message to <tt>@gblOpt[:log_file]</tt>
  # - +level+::    An integer.  If <tt>@gblOpt[:verbose]>=level</tt>, then the message will be printed
  # - +msg+::      The message to print (usually a string)
  # - +source+::   The source of the message.  It is prepended to all printed messages.  Usually set to +self+.
  # - +exitCode+:: If non-+nil+ and <tt>@gblOpt[:exit_on_error]</tt> is non-+nil+, then causes a process exit with the given code
  def logPrinter (level, msg, source, exitCode=nil)
    #lineNumberThatCalledUs = caller.first.to_s.sub(/^.*:([0-9]+):in.*$/, '\1')
    begin
      if @gblOpt[:verbose] >= level then
        if @openFiles[@gblOpt[:log_file]] then
          @openFiles[@gblOpt[:log_file]].puts("#{((source && source.class.to_s.upcase) || 'UNKNOWN')}: #{msg}")
          @openFiles[@gblOpt[:log_file]].flush
          #@openFiles[@gblOpt[:log_file]].fsync
        end
      end
    rescue
        if @openFiles[@gblOpt[:log_file]] != 'STDERR' then
          @openFiles[@gblOpt[:log_file]] = 'STDERR'
          retry
        else
          # Not really sure what to do here...  We couln't print to the error log file or to STDERR...
        end
    end
    if exitCode && @gblOpt[:exit_on_error] then
      if @gblOpt[:exit_0] then
        exit 0
      else
        exit exitCode
      end
    end
  end

  ################################################################################################################################################################
  # Print a message to <tt>@gblOpt[:out_file]</tt>
  # - +newline+:: If non-+nil+, then +puts+ will be used.  Otherwise +write+ is used.
  def outPrinter (msg, newline=true)
    if @openFiles[@gblOpt[:out_file]] then
      msgToPrint = msg
      truncMsg   = ''
      if @gblOpt[:print_debug] then
        msgToPrint = msgToPrint.inspect
      end
      if @gblOpt[:print_max_len] && (@gblOpt[:print_max_len] > 0) && (msg.length > @gblOpt[:print_max_len]) then
        msgToPrint = msgToPrint.slice(0, @gblOpt[:print_max_len]);
        truncMsg = "LAST LINE TRUNCATED (:print_max_len=#{@gblOpt[:print_max_len]})"
      end
      if newline || @gblOpt[:print_debug] then
        @openFiles[@gblOpt[:out_file]].puts(msgToPrint)
      else
        @openFiles[@gblOpt[:out_file]].write(msgToPrint)
      end
      if !(truncMsg.empty?) then
        @openFiles[@gblOpt[:out_file]].puts(truncMsg)
      end
      @openFiles[@gblOpt[:out_file]].flush
      #@openFiles[@gblOpt[:out_file]].fsync
    end
  end

  ################################################################################################################################################################
  # Close all open files except <tt>STDERR</tt>, <tt>STDOUT</tt>, & <tt>/dev/null</tt>.
  # Sets <tt>@gblOpt[:log_file]</tt> to <tt>'STDERR'</tt> &  <tt>@gblOpt[:out_file]</tt> to <tt>'STDOUT'</tt>.
  def close
    @openFiles.each do |fileName, fileDescriptor|
      if fileDescriptor
        fileDescriptor.close
      end
    end
  end

end

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
if __FILE__ == $0 then

  #---------------------------------------------------------------------------------------------------------------------------------------------------------------
  optMap = { '-c' => '--cmd',
             '-a' => '--url',
             '-b' => '--delay_after_complete',
             '-l' => '--eol',
             '-s' => '--net_protocol',
             '-D' => '--var',
             '-o' => '--out_file',
             '-p' => '--net_port',
             '-t' => '--result_type',
             '-v' => '--verbose',
           }

  metaOpt = { '-f' => nil,      # input file.  'STDIN' means STDIN
              '-V' => nil,
              '-E' => nil
            }

  #---------------------------------------------------------------------------------------------------------------------------------------------------------------
  theSequence = SCPIsequence.new(convertStrings=true)
  theSequence.add(:out_file,   'STDOUT')
  theSequence.add(:print_cmd,    true)
  theSequence.add(:print_result, true)

  #---------------------------------------------------------------------------------------------------------------------------------------------------------------
  begin
    opt = ARGV.shift
    par = ARGV.shift
    if optMap.member?(opt) then
      opt = optMap[opt]
    end

    if par.nil? && opt && !(opt.match?(/^-/)) then
      metaOpt['-f'] = opt
    elsif par && opt && (metaOpt.member?(opt)) then
      metaOpt[opt] = par
      if opt == '-V' then
        PrintyPrintyBangBang.instance.set(:verbose => par.to_i)
      elsif opt == '-E' then
        PrintyPrintyBangBang.instance.set(:log_file => par)
      end
    elsif opt.nil? || !(opt.match?(/^--/)) || !(theSequence.add(opt, par)) then
      STDERR.puts("mrSCPI.rb Use")
      STDERR.puts("")
      STDERR.puts("  mrSCPI.rb [options] [script_file_name]")
      STDERR.puts("")
      STDERR.puts("Options are generally the same as the option names for the SCPIsession object.")
      STDERR.puts("For more information on these options, see the SCPIsession documentation in mrSCPIlib.rb")
      STDERR.puts("Sorry about that.  I'll put together some proper documentation someday.")
      STDERR.puts("")
      STDERR.puts("Several single letter options are shortcuts for long options:")
      optMap.each do |sopt, lopt|
        STDERR.puts("  #{sopt} ... #{lopt}")
      end
      STDERR.puts("")
      STDERR.puts("Only the first instance of :url, :ip_address, :net_port, & :net_protocol are used")
      STDERR.puts("")
      STDERR.puts("Finally a couple of non-SCPIsession options are also supported: ")
      STDERR.puts("  -f FILE_NAME                     Script file to process")
      STDERR.puts("  -E FILE_NAME                     Set the inital error log file")
      STDERR.puts("  -V VALUE                         Set the inital value for global verbosity")
      STDERR.puts("  -h           --help              Print this message")
      STDERR.puts("")
      exit
    end
  end while !(ARGV.empty?)

  # If we have a file argument, then we read it now.
  if metaOpt['-f'] then
    begin
      ifile = (metaOpt['-f'] == 'STDIN' ? STDIN : open(metaOpt['-f'], "rb"))
    rescue
      PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Failed to open script file: #{metaOpt['-f']}!", 'MAIN', 85)
    end
    if ifile then
      lineNum = 1
      ifile.each_line do |line|
        line.chomp!.sub!(/#.*/, '')
        if line.length > 0 then
          mData = line.match(/^(:\S+)\s+(.+)$/)
          if mData.nil? || !(theSequence.add(mData[1], mData[2])) then
            PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Syntax error (#{metaOpt['-f']}:#{lineNum}) : #{line.inspect}", 'MAIN', nil)
            exit 0
          end
        end
      end
      ifile.close
    else
      PrintyPrintyBangBang.instance.logPrinter(1, "ERROR: Could not open script file: #{metaOpt['-f']}", 'MAIN', nil)
      exit 0
    end
  end

  #---------------------------------------------------------------------------------------------------------------------------------------------------------------
  theSequence.execute

  #---------------------------------------------------------------------------------------------------------------------------------------------------------------
  exit 0
end
