//
//  ThresholdFilter.h
//  assignment_1
//
//  Created by Alpha Chen on 2/18/06.
//  Copyright 2006. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <QuartzCore/QuartzCore.h>

@interface ThresholdFilter : CIFilter {
    CIImage *inputImage;
    NSNumber *inputThreshold;
}

@end
