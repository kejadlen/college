//
//  MyDocument.h
//  assignment_1
//
//  Created by Alpha Chen on 2/17/06.
//  Copyright 2006 . All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <QuartzCore/QuartzCore.h>

#import "TWindowController.h"
#import "ThresholdFilter.h"


@interface TDocument : NSDocument
{
    TWindowController *windowController;
    
    NSImage *originalImage;
    NSBitmapImageRep *imageRep;
    CIImage *ciImage;
    NSImage *thresholdedImage;
    
    CIFilter *thresholdFilter;
}

- (void)thresholdImage:(float)threshold;

- (NSImage *)image;

@end
