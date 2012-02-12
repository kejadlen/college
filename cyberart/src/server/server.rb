#!/usr/bin/env ruby

require 'rubygems'
require 'active_record'
require 'drb'
require 'enumerator'
require 'logger'
require 'pp'
require 'yaml'

LOG_OUT = STDOUT
LOG_LEVEL = Logger::INFO

MIN_SIZE = 3   # inches

def create_image_wait
  sleep(1 + rand(3))
end

def change_category_wait
  sleep(60 + rand(120))
end

def image_speed
  2 + rand(3)
end

class Fixnum
  def displayed
    true
  end
end

class Object
  def pretty
    PP::pp(self, '').enum_for(:each_line).map {|l| "\t" + l }.join.chomp
  end
end

class Array
  def random_element
    self[rand(size)]
  end
end

Image = Struct.new( :picture, :position, :montinents, :speed, :send_time, :displayed )

Montinent = Struct.new( :host, :location, :scale, :size, :drb )
class Montinent
  def will_display?(picture, position)
    x = position + MIN_SIZE * picture.width / picture.dimension
    [x, position].inject(false) {|n,i| n ||= i >= location[:x] and i <= location[:x] + size[:x] }
  end
end

ActiveRecord::Base.establish_connection( :adapter  => 'sqlite3', :dbfile   => 'cyberart.db' )
class Picture < ActiveRecord::Base
  def dimension
    @dimension ||= (width > height) ? width.to_f : height.to_f
  end
end

class CyberArtServer
  include DRbUndumped

  attr_reader :config, :montinents, :categories

  def initialize(config_file = 'config.yml')
    # Start logging
    @log = Logger.new(LOG_OUT)
    @log.level = LOG_LEVEL

    # Load montinents from config
    @config = YAML.load_file(config_file)
    @montinents = @config[:montinents]
    @montinents.map! do |montinent|
      scale = Math.sqrt(montinent[:resolution][:x]**2 + montinent[:resolution][:y]**2) / montinent[:size]
      size = { :x => montinent[:resolution][:x] / scale,
               :y => montinent[:resolution][:y] / scale }
      Montinent.new(
                    montinent[:host],
                    montinent[:location],
                    scale,
                    size
                   )
    end

    # Log config
    @log.debug "Config loaded:\n#{@config.pretty}"

    # Start DRb
    DRb.start_service("druby://:#{@config[:server_port]}", self)
    @log.info "DRb started: #{DRb.uri}"

    # Initialize instance variables
    @images = Array.new
    @semaphore = Mutex.new
    @categories = Picture.find(:all).map {|p| p.category }.uniq
    @pictures = Picture.find_all_by_category( @categories.random_element )
  end

  # Start server
  def start
    Thread.new { loop { create_image; create_image_wait } }
    Thread.new { loop { change_category; change_category_wait } }
    loop { @semaphore.synchronize { send_image } }
  end

  def create_image
    # set file
    picture = @pictures.random_element
    image = Image.new( picture )

    # set position
    image.position = rand(10 * @config[:size][:x]) / 10.0

    # set montinents
    image.montinents = Array.new
    @montinents.each do |montinent|
      image.montinents << montinent if montinent.will_display? picture, image.position
    end

    # sort montinents
    image.montinents.sort {|a, b| a.location[:y] <=> b.location[:y] }

    # set speed
    image.speed = image_speed

    # set time
    image.send_time = Time.now + @montinents.first.location[:y] / image.speed

    # add to image array
    unless image.montinents.empty?
      @log.info "#{image.object_id}: created, dropping in [#{image.montinents.map {|i| i.host }.join ', '}]"
      @log.debug "#{image.pretty}"
      @semaphore.synchronize { @images << image }
    end
  end

  def change_category
    category = @categories.random_element
    @pictures = Picture.find_all_by_category( category )
    @log.info "Category changed to #{category}"
  end
  
  # Sends images to server
  def send_image
    @images.select {|i| !i.displayed and i.send_time < Time.now }.each do |image|
      montinent = image.montinents.first
      if montinent.drb
        image.displayed = true
        x = (image.position - montinent.location[:x]) * montinent.scale
        speed = image.speed * montinent.scale
        scale = MIN_SIZE * montinent.scale / image.picture.dimension
        montinent.drb.add_image( image.object_id, image.picture.filename, x, speed, image.picture.width * scale, image.picture.height * scale )
        @log.info "#{image.object_id}: Sent to #{montinent.host}"
      else
        @log.info "#{image.object_id}: Skipping #{montinent.host} (not connected)"
        next_montinent( image )
      end
    end
  end

  # 
  def next_montinent(image)
    montinent = image.montinents.shift
    unless image.montinents.empty?
      image.send_time = Time.now + (image.montinents.first.location[:y] - montinent.location[:y] - montinent.size[:y]) / image.speed
      image.displayed = false
    else
      @images.delete image
      @log.info "#{image.object_id}: finished"
    end
  end

  # Networking (DRb) code follows:
  #
  def check_in(montinent)
    @log.info "#{montinent} checked in"
    m = @montinents.find {|m| m[:host] == montinent }
    unless m.nil?
      m[:drb] = DRbObject.new(nil, "druby://#{montinent}:#{@config[:client_port]}")
      @log.info "Connected to #{montinent}"
    else
      @log.info "No known montinent #{montinent}"
    end
  end

  def image_back(id)
    @log.info "#{id}: returned from client"
    @semaphore.synchronize do
      next_montinent(@images.select {|i| i.object_id == id }[0])
    end
  end

  def check_out(montinent)
    @log.info "#{montinent} checked out"
    m = @montinents.find {|m| m[:host] == montinent }
    m[:drb] = nil
  end
end

if $0 == __FILE__
  server = CyberArtServer.new("config.yml")

  server.start
end
