#!/usr/bin/env -S sh
# -*- Mode:Shell-script; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      deploy.sh
# @author    Mitch Richling http://www.mitchr.me/
# @brief     Move *.el files into my dot file repository.@EOL
# @keywords  
# @std       POSIX sh
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
#  I use a dotfile management system to put dotfiles in place on the many computers I use.  At the heart of the system is a repository of dotfiles housed in
#  ~/world/dotfiles.  This script simply copies the Emacs code in this directory into that repository.  As such, this script is of no real use to anyone other
#  than myself.  For instructions regarding how to install the Emacs code in your own environment, see the readme.org file one level up.
#
#########################################################################################################################################################.H.E.##

################################################################################################################################################################
cp mrscpi-mode.el    ~/world/dotfiles/.emacs.d/mrscpi-mode.el--SS-X-X-X-HOME-X
cp ob-mrscpi.el      ~/world/dotfiles/.emacs.d/ob-mrscpi.el--SS-X-X-X-HOME-X
cp mrscpi-buttons.el ~/world/dotfiles/.emacs.d/mrscpi-buttons.el--SS-X-X-X-HOME-X
