#import "CAFullscreenHelper.h"

@implementation CAFullscreenHelper

+ (CGDisplayErr)captureDisplay:(NSNumber *)screenID
{
  CGDirectDisplayID displayID = (CGDirectDisplayID)[screenID longValue]; 
  return CGDisplayCapture(displayID);
}

+ (CGDisplayErr)releaseDisplay:(NSNumber *)screenID
{
  CGDirectDisplayID displayID = (CGDirectDisplayID)[screenID longValue]; 
  return CGDisplayRelease(displayID);
}

+ (int32_t)getShieldingWindowLevel
{
  return CGShieldingWindowLevel();
}

@end
