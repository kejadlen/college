//
//  TWindowController.h
//  assignment_1
//
//  Created by Alpha Chen on 2/17/06.
//  Copyright 2006 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@class TImageView;
@class TDocument;

@interface TWindowController : NSWindowController {
    IBOutlet TImageView *view;
    IBOutlet NSTextField *thresholdTextControl;
    IBOutlet NSSlider *thresholdSliderControl;
    
    TDocument *tdoc;
    float threshold;
}

- (void)setThreshold:(float)_threshold;
- (IBAction)changeThreshold:(id)sender;

@end
