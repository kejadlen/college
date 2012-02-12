require 'osx/cocoa'
require 'drb'
OSX.ns_import :CAFullscreenHelper
 
# Main class for FlatEarth
 
class CAController < OSX::NSWindowController
  ib_outlets :montinent

  def initialize
    default_values = OSX::NSMutableDictionary.dictionary
    
    default_values.setObject_forKey_("mirepoix.local", "server_host")
    default_values.setObject_forKey_("3142", "server_port")
    
    OSX::NSUserDefaults.standardUserDefaults.registerDefaults default_values
  end

  def applicationDidFinishLaunching(note)
    DRb.start_service('druby://mirepoix.local:3141', @montinent)
    OSX::NSLog("DRb service started: #{DRb.uri}")
#    check_in
  end
  
  def applicationShouldTerminate(sender)
    @montinent.exit
    return true
  end

  def awakeFromNib
    self.window.setBackgroundColor OSX::NSColor.blackColor
    
    # Set up instance variables
    @wScreenWindow = self.window
    @mScreenWindow = nil
    @preferenceController = nil
    @fullscreen = false
  end
  
  # Show preference panel
  def show_preference_panel
    if @preferenceController.nil?
      @preferenceController = PreferenceController.alloc.init
    end
    @preferenceController.showWindow self
  end
  
  # Go in and out of fullscreen
  def fullscreen
    if @fullscreen
      goWindowed
    else
      goFullScreen
    end
  end
  
  # Check in with server
  def check_in
    begin
      @montinent.check_in
    rescue
    end
  end
  
  def goWindowed
    # Get the screen information
    mainScreen = OSX::NSScreen.mainScreen
    screenInfo = mainScreen.deviceDescription
    screenID = screenInfo.objectForKey("NSScreenNumber")
    
    # Relase the display
    OSX::CAFullscreenHelper.releaseDisplay screenID
    
    # Set the content for the window
    @wScreenWindow.setContentView @montinent
    self.setWindow @wScreenWindow
    
    # Kill fullscreen window
    # NOTE: Is there a better way to do this?
    @mScreenWindow.close
    @mscreenWindow = nil
    
    @fullscreen = false
    OSX::NSCursor.unhide
  end
  
  def goFullScreen
    # Get the screen information
    mainScreen = OSX::NSScreen.mainScreen
    screenInfo = mainScreen.deviceDescription
    screenID = screenInfo.objectForKey("NSScreenNumber")
    
    # Capture the display
    OSX::CAFullscreenHelper.captureDisplay screenID
  
    window_rect = mainScreen.frame
    unless @mScreenWindow
      @mScreenWindow = OSX::NSWindow.alloc.initWithContentRect( window_rect,
                              :styleMask, OSX::NSBorderlessWindowMask,
                              :backing, OSX::NSBackingStoreBuffered,
                              :defer, false,
                              :screen, OSX::NSScreen.mainScreen
                            )
      
      # Establish the window attributes
      @mScreenWindow.setReleasedWhenClosed false
      @mScreenWindow.setDisplaysWhenScreenProfileChanges true
      @mScreenWindow.setDelegate self
      @mScreenWindow.setBackgroundColor OSX::NSColor.blackColor
    end
    
    # Set the content for the window
    @mScreenWindow.setContentView @montinent
    @montinent.setNeedsDisplay true
    
    # Make the screen window the current document window
    self.setWindow @mScreenWindow
    
    # The window has to be above the level of the shield window
    shield_level = OSX::CAFullscreenHelper.getShieldingWindowLevel
    @mScreenWindow.setLevel shield_level
    
    # Show the window
    @mScreenWindow.makeKeyAndOrderFront self
    
    @fullscreen = true
    OSX::NSCursor.hide
  end
end
