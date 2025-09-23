#!/usr/bin/env ruby

#*****************************************************************************#
#                                                                             #
#  tsi_header.rb                                          TTTTTTTT SSSSSSS II #
#                                                            TT    SS      II #
#  By: igors.oleinikovs@students.tsi.lv                      TT    SSSSSSS II #
#                                                            TT         SS II #
#  Created: Sep 23 2025 11:19 igors.oleinikovs               TT    SSSSSSS II #
#  Updated: Sep 23 2025 11:19 igors.oleinikovs                                #
#                                                                             #
#   Transport and Telecommunication Institute - Riga, Latvia                  #
#                       https://tsi.lv                                        #
#*****************************************************************************#

require_relative 'tsi_header/header_info'
require_relative 'tsi_header/header_generator'
require_relative 'tsi_header/header_extractor'
require_relative 'tsi_header/header_parser'
require_relative 'tsi_header/delimiters'
require_relative 'tsi_header/configuration'

module TSIHeader
  VERSION = "1.0.0"
end