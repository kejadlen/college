#import <Cocoa/Cocoa.h>

/*
 This class is only used to help FlatEarth go in and
 out of fullscreen. Displays need to be captured and
 released, and the shielding window level needs to be
 retrieved, all of which require Objective-C.
 */
@interface CAFullscreenHelper : NSObject {
}

+ (CGDisplayErr)captureDisplay:(NSNumber *)screenID;
+ (CGDisplayErr)releaseDisplay:(NSNumber *)screenID;
+ (int32_t)getShieldingWindowLevel;

@end
