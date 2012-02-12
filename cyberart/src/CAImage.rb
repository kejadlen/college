require 'osx/cocoa'

# CAImage holds the images which are dropping down the Montinent view.

class CAImage
  attr :id

  def initialize( id, imageName, x, dy, width, height )
    @id = id

    # Load the image
    @image = OSX::NSImage.alloc.initWithContentsOfFile( imageName )
    	
    # The image rectangle is scaled
    @image_rect = OSX::NSRect.new( 0.0, 0.0, width, height)

    # Start the image above the view
    @location = OSX::NSPoint.new( x, -@image_rect.size.height )

    # Create the transform which actually moves the image
    @dy = OSX::NSAffineTransform.transform.retain
    @dy.translateXBy_yBy( 0, dy/60.0 )
    
    # Set image attributes, including resizing the image
    @image.setScalesWhenResized true
    @image.setSize @image_rect.size
    @image.setFlipped true    
  end
  
  # Move the image according to the stored transform
  def apply_transform!
    @location = @dy.transformPoint( @location )
  end
  
  # Draw the image to the screen
  def draw
    @image.drawAtPoint( @location,
      :fromRect, @image_rect,
      :operation, OSX::NSCompositeSourceOver,
      :fraction, 1.0
    )
  end
  
  # Return the image's vertical location
  def y
    @location.y
  end
end
