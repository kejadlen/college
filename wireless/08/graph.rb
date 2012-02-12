#!/usr/bin/env ruby

require 'gruff'

g = Gruff::Line.new
g.title = 'Throughput vs. Time'
g.theme_keynote

data_1 = File.open("teamA/test1/iperf_1A.log").readlines.join
data_2 = File.open("teamA/test1/iperf_2A.log").readlines.join
data_1 = data_1.scan(/([.\d]* \w)bits\/sec/)
data_2 = data_2.scan(/([.\d]* \w)bits\/sec/)
data_1.flatten!
data_2.flatten!
data_1.map! {|i| i.split }
data_2.map! {|i| i.split }
data_1.map! {|i| i[1] == "M" ? i[0].to_f * 1024 : i[0].to_f }
data_2.map! {|i| i[1] == "M" ? i[0].to_f * 1024 : i[0].to_f }

g.data("iperf_1A.log", data_1)
g.data("iperf_2A.log", data_2)

g.labels = {0 => '0s', 16 => '4 min', 32 => '8 min'}

g.write('graph.png')
