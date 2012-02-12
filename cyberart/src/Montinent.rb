require 'osx/cocoa'
require 'CAImage'
require 'drb'

# The view which holds the dropped images. Images are added
# to this view via add_image. After they have dropped out of
# view, they are removed from the view and the "trackback" is
# sent to the server.

class Montinent <  OSX::NSView
  include DRbUndumped

  ns_overrides 'initWithFrame:', 'drawRect:', 'isFlipped'

  # Initialization code: sets @images and @server appropriately
  # and sets up the animation timer
  def initWithFrame (frame)
    super_initWithFrame(frame)
    
    @images = Array.new
    @semaphore = Mutex.new
    @server = nil

    OSX::NSTimer.scheduledTimerWithTimeInterval(
      1/60.0,
      :target, self,
      :selector, :tick,
      :userInfo, nil,
      :repeats, true
    )

    return self
    end

  # Draw each of the images
  def drawRect (rect)
    @images.each {|image| image.draw }
  end
  
  # We are working in a flipped orientation for ease in
  # calculating with dropping images
  def isFlipped
    true
  end
  
  # Add a new image to the view
  def add_image( id, image_name, x_position, speed, width, height )
    OSX::NSLog("Received image ##{id} (#{image_name})")
#    image_name = OSX::NSBundle.mainBundle.pathForImageResource( image_name )
    image_name = "/Users/alpha/src/cyberart/images/#{image_name}.jpg"
    image = CAImage.new( id, image_name, x_position, speed, width, height )
    @semaphore.synchronize { temp_images = @images.dup }
    temp_images << image
    @semaphore.synchronize { @images = temp_images }
  end
  
  # Connect to the server via DRb
  def check_in
    defaults = OSX::NSUserDefaults.standardUserDefaults
    server_host = defaults.stringForKey("server_host")
    server_port = defaults.integerForKey("server_port")
    
    drb_url = "druby://#{server_host}:#{server_port}"
    
    OSX::NSLog("Trying to connect to #{drb_url}")
    
    @server = DRbObject.new(nil, drb_url)
    @host = DRb.uri.scan(/druby:\/\/(.*):\d*/)[0][0]
    begin
      @server.check_in @host
      OSX::NSLog("Connected to server")
    rescue
      @server = nil
      OSX::NSLog("Couldn't connect to server: #{$!}")
    end
  end
  
  # Drop each image and remove it if it has fallen off the screen
  def tick
    @semaphore.synchronize { @images.each {|image| image.apply_transform! } }

    finished = Array.new

    # Remove finished images
    @semaphore.synchronize do
      finished = @images.select {|image| image.y > self.bounds.height }
    end
	
    finished.each do |image|
        OSX::NSLog "Removing #{image} from view"
      Thread.new do
        begin
          @server.image_back( image.id ) unless @server.nil?
        rescue
          OSX::NSLog "Couldn't connect to server"
        @server = nil
        end
      end
      @semaphore.synchronize { @images.delete image }
    end

    # Refresh the view
    self.setNeedsDisplay true
  end
  
  # Check out from the server when closing
  def exit
    @server.check_out(@host) unless @server.nil?
  end

end
