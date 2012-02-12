#!/usr/bin/env ruby

require 'open-uri'
require 'active_record'

data = open("http://informationflow.netherweb.com/util/alpha_whore.php").readlines.map! {|r| r.chomp }

ActiveRecord::Base.establish_connection(
  :adapter  => 'sqlite3',
  :dbfile   => 'cyberart.db'
  )

class Picture < ActiveRecord::Base
end

Picture.delete_all

data.each do |row|
  Picture.new do |p|
    filename, p.category, p.width, p.height = row.split(',')
    p.filename = filename.gsub(/\.[^.]*$/,'')
    p.save
  end
end
