#!/usr/bin/env ruby

require 'highline/import'
require 'RMagick'
require 'optparse'

HISTOGRAM_BINS = 10

options = Hash.new
opts = OptionParser.new do |opts|
  opts.on("-t", "--threshold THRESHOLD", Float,
          "Choose a threshold value between 0.0 and 1.0") do |threshold|
    options[:threshold] = threshold
  end
end

opts.parse!

raise ArgumentError, "Must pass two filenames" unless ARGV.length == 2

options[:input] = ARGV[0]
options[:output] = ARGV[1]

begin
  image = Magick::ImageList.new(options[:input])

  unless options[:threshold]
    puts "Generating histogram..."

    histogram = image.quantize(HISTOGRAM_BINS, Magick::GRAYColorspace).color_histogram
    pixels = histogram.keys.sort_by {|pixel| histogram[pixel]}
    max_color = histogram.values.max
    histogram.each do |k,v|
      histogram[k] = v.to_f / max_color
    end

    pixels.each_with_index do |pixel,i|
      print "%02d: " % [i]
      print "(%0.2f) " % [histogram[pixel]]
      stars = (HISTOGRAM_BINS * histogram[pixel]).to_i
      stars.times { print '*' }
      puts
    end

    options[:threshold] = ask("Choose a threshold value from 0.0 to 1.0: ", Float)
  end

  image = image.white_threshold(Magick::MaxRGB * options[:threshold])
  image.image_type = Magick::GrayscaleType
  image.write(options[:output])
rescue => detail
  puts detail.message
end
