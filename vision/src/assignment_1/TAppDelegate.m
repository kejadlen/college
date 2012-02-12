//
//  TAppDelegate.m
//  assignment_1
//
//  Created by Alpha Chen on 2/17/06.
//  Copyright 2006. All rights reserved.
//

/*
 This code is essentially taken from Apple's Core Image Funhouse application.
 */

#import "TAppDelegate.h"


@implementation TAppDelegate

// application delegates implement this to accomplish tasks right after the application launches
// in this case, I copy the example images from the application package to the ~/Library/Application Support/Computer Vision/images folder
// we also automatically open a file on launch if we're not already opening one (by drag or double-click)
- (void)applicationDidFinishLaunching:(NSNotification *)notification
{
    BOOL isDir;
    int i, count;
    NSError *err;
    NSOpenPanel *op;
    NSString *path, *path2, *source, *file, *sourcefile, *destfile;
    NSBundle *bundle;
    NSFileManager *manager;
    NSArray *files;
    
    // decide if images have yet been copied to ~/Library/Application Support/Computer Vision/images
    path = @"~/Library/Application Support/Computer Vision";
    path = [path stringByExpandingTildeInPath];
    path2 = [path stringByAppendingString:@"/images"];
    manager = [NSFileManager defaultManager];
    if (![manager fileExistsAtPath:path isDirectory:&isDir])
    {
        // otherwise we need to create the ~/Library/Application Support/Computer Vision folder
        [manager createDirectoryAtPath:path attributes:nil];
        [manager createDirectoryAtPath:path2 attributes:nil];
        bundle = [NSBundle bundleForClass:[self class]];
        source = [[bundle resourcePath] stringByAppendingString:@"/images"];
        // and copy the files
        files = [manager directoryContentsAtPath:source];
        count = [files count];
        for (i = 0; i < count; i++)
        {
            file = [files objectAtIndex:i];
            sourcefile = [[source stringByAppendingString:@"/"] stringByAppendingString:file];
            destfile = [[path2 stringByAppendingString:@"/"] stringByAppendingString:file];
            [manager copyPath:sourcefile toPath:destfile handler:nil];
        }
    }
    // only automatically open a file if none has already been opened
    if ([[[NSDocumentController sharedDocumentController] documents] count] == 0)
    {
        // open a file at launch from the ~/Library/Application Support/Computer Vision/images folder
        op = [NSOpenPanel openPanel];
        [op setAllowsMultipleSelection:NO];
        if ([op runModalForDirectory:path2 file:nil types:[NSArray arrayWithObjects:@"jpeg", @"jpg", @"gif", nil]] == NSOKButton)
            [[NSDocumentController sharedDocumentController] openDocumentWithContentsOfURL:[[op URLs] objectAtIndex:0] display:YES error:&err];
    }
}

// don't open an untitled file at the start
- (BOOL)applicationShouldOpenUntitledFile:(NSApplication *)sender
{
    return NO;
}

@end
