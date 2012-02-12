//
//  TWindowController.m
//  assignment_1
//
//  Created by Alpha Chen on 2/17/06.
//  Copyright 2006 __MyCompanyName__. All rights reserved.
//

#import "TWindowController.h"
#import "TImageView.h"
#import "TDocument.h"

@implementation TWindowController

// Resize the window to fit the dimensions of the image. If the image is
// too big, scale it down.
- (void)windowDidLoad
{
    float xscale, yscale, scale;
    NSSize imagesize;
    NSSize screensize;
    
    NSImage *image = [tdoc image];
    
    if(image)
    {
        imagesize = [image size];
        screensize = [[NSScreen mainScreen] frame].size;
        if (imagesize.width > screensize.width || imagesize.height > (screensize.height - 50))
        {
            // compute scale needed
            xscale = screensize.width / imagesize.width;
            yscale = (screensize.height - 50) / imagesize.height;
            scale = (yscale < xscale) ? yscale : xscale;
            imagesize.width *= scale;
            imagesize.height *= scale;
            imagesize.width = ceil(imagesize.width);
            imagesize.height = ceil(imagesize.height);
        }        
        
        [[self window] setContentSize:NSMakeSize(imagesize.width, imagesize.height + 50)];
    }
    
    [view setImage:image];
    [self setThreshold:0.0];
}

- (void)setDocument:(NSDocument *)document
{
    [super setDocument:document];
    tdoc = (TDocument *)document;
}

// The target of the controls from the TWindow nib.
- (IBAction)changeThreshold:(id)sender
{
    [self setThreshold:([sender intValue] / 100.0)];
}

// Threshold the image
- (void)setThreshold:(float)_threshold
{
    if ( _threshold >= 0 )
        threshold = _threshold;
  
    [thresholdSliderControl setIntValue:(threshold * 100)];
    [thresholdTextControl setIntValue:(threshold * 100)];
    
    [tdoc thresholdImage:threshold];
    [view setImage:[tdoc image]];
    [view setNeedsDisplay:YES];
}

@end
