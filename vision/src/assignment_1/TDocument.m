//
//  MyDocument.m
//  assignment_1
//
//  Created by Alpha Chen on 2/17/06.
//  Copyright 2006 . All rights reserved.
//

#import <QuartzCore/QuartzCore.h>

#import "TDocument.h"

@implementation TDocument

- (id)init
{
    self = [super init];
    if (self) {
        [ThresholdFilter class];

        thresholdFilter = [CIFilter filterWithName:@"Threshold"];
        [thresholdFilter retain];
    }
    return self;
}

- (void)makeWindowControllers
{
    windowController = [[TWindowController alloc] initWithWindowNibName:@"TWindow"];
    [self addWindowController:windowController];
}

- (void)windowControllerDidLoadNib:(NSWindowController *) aController
{
    [super windowControllerDidLoadNib:aController];
}

// Saving files
- (NSData *)dataOfType:(NSString *)typeName error:(NSError **)outError {
    // Get the bitmap representation of the thresholded image
    NSBitmapImageRep *dataRep = [NSBitmapImageRep imageRepWithData:[thresholdedImage TIFFRepresentation]];
    
    if ([typeName isEqualToString:@"JPEG File"])
        return [dataRep representationUsingType:NSJPEGFileType properties:nil];
    if ([typeName isEqualToString:@"GIF File"])
        return [dataRep representationUsingType:NSGIFFileType properties:nil];
    
    return nil;
}

// Opening files
- (BOOL)readFromData:(NSData *)data ofType:(NSString *)typeName error:(NSError **)outError
{
    originalImage = [[[NSImage alloc] initWithData:data] autorelease];
    imageRep = [NSBitmapImageRep imageRepWithData:[originalImage TIFFRepresentation]];
    ciImage = [[[CIImage alloc] initWithBitmapImageRep:imageRep] autorelease];
    [thresholdFilter setValue:ciImage forKey:@"inputImage"];
    
    // Set the initial threshold
    [self thresholdImage:0.0];
    
    return (thresholdedImage != nil);
}

// Run the threshold filter
- (void)thresholdImage:(float)threshold {
    CIImage *result;
    
    [thresholdFilter setValue:[NSNumber numberWithFloat:threshold]
                       forKey:@"inputThreshold"];
    result = [thresholdFilter valueForKey: @"outputImage"]; 
    
    NSCIImageRep *ir;
    CGRect r = [result extent];
    
    // Convert the CIImage back to an NSImage
    ir = [NSCIImageRep imageRepWithCIImage:result];
    thresholdedImage = [[[NSImage alloc] initWithSize:
        NSMakeSize(r.size.width, r.size.height)]
        autorelease];
    [thresholdedImage addRepresentation:ir];
}

- (NSImage *)image
{
    return thresholdedImage;
}

@end
