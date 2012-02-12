#!/usr/bin/env ruby

require 'gruff'

g = Gruff::Bar.new
g.title = 'Data Rate vs. Throughput'
g.theme_keynote

g.data("Without Interference", [0.67, 1.31, 4.49])
g.data("With Interference", [0.50, 0.83, 2.52])

g.labels = {0 => '1Mb', 1 => '2Mb', 2 => 'Auto'}

g.write('graph.png')
