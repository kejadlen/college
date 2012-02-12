require 'osx/cocoa'

class PreferenceController < OSX::NSWindowController
  ib_outlets :host, :port
  
  def init
    initWithWindowNibName("Preferences")
  end
  
  def reset(sender)
    defaults = OSX::NSUserDefaults.standardUserDefaults

    defaults.setObject_forKey_("mirepoix.local", "server_host")
    defaults.setObject_forKey_("3142", "server_port")

    window.setViewsNeedDisplay true
  end
end
