//
//  ThresholdFilter.m
//  assignment_1
//
//  Created by Alpha Chen on 2/18/06.
//  Copyright 2006. All rights reserved.
//

/*
 Pretty much taken straight from the Apple documentation
 */

#import "ThresholdFilter.h"


@implementation ThresholdFilter

static CIKernel *thresholdKernel = nil;

- (id)init {
    if(thresholdKernel == nil) {
        NSBundle    *bundle = [NSBundle bundleForClass: [self class]];
        NSString    *code = [NSString stringWithContentsOfFile:[bundle pathForResource: @"Threshold" ofType: @"cikernel"]];
        NSArray     *kernels = [CIKernel kernelsWithString: code];
        thresholdKernel = [[kernels objectAtIndex:0] retain];
    }
    return [super init];
}

+ (void)initialize {
    [CIFilter registerFilterName:@"Threshold"
                     constructor:self
                 classAttributes:[NSDictionary dictionaryWithObjectsAndKeys:
                     @"Threshold", kCIAttributeName,
                     [NSArray arrayWithObjects:
                         kCICategoryColorAdjustment, kCICategoryVideo, 
                         kCICategoryStillImage,kCICategoryInterlaced,
                         kCICategoryNonSquarePixels,nil], kCIAttributeFilterCategories,
                     [NSDictionary dictionaryWithObjectsAndKeys:
                         [NSNumber numberWithDouble:0.0], kCIAttributeMin,
                         [NSNumber numberWithDouble:1.0], kCIAttributeMax,
                         [NSNumber numberWithDouble:0.0], kCIAttributeSliderMin,
                         [NSNumber numberWithDouble:1.0], kCIAttributeSliderMax,
                         [NSNumber numberWithDouble:0.2], kCIAttributeDefault,
                         [NSNumber numberWithDouble:0.0], kCIAttributeIdentity,
                         kCIAttributeTypeScalar, kCIAttributeType,
                         nil], @"inputThreshold",
                     nil]];
}

- (CIImage *)outputImage {
    CISampler *src = [CISampler samplerWithImage: inputImage];

    return [self apply: thresholdKernel, src, inputThreshold,
        kCIApplyOptionDefinition, [src definition], nil];
}

+ (CIFilter *)filterWithName:(NSString *)name {
    CIFilter  *filter;
    
    filter = [[self alloc] init];
    return [filter autorelease];   
}

@end
