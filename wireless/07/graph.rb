#!/usr/bin/env ruby

require 'gruff'
require 'yaml'

g = Gruff::Line.new
g.title = 'Throughput vs. Packet Size'

data_yml = YAML.load_file('data.yml')

data_yml.each do |data_type, data|
#  data.each do |k,v|
#    v = v.split(", ").map {|i| i.to_i }
#    data_yml[data_type][k] = v.inject(0) {|n,i| n + i } / v.size
#  end

  g.data(data_type, data.sort.map {|i| i[1].to_i })
end

g.labels = { 0 => '50' }
1.upto(7) do |i|
  g.labels[i] = (i * 200).to_s
end

g.write('graph.png')
