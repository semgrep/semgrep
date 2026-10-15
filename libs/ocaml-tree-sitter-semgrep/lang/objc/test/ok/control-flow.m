#import <Foundation/Foundation.h>

NSInteger sum(NSArray<NSNumber *> *xs) {
  NSInteger total = 0;
  for (NSNumber *x in xs) {
    if ([x integerValue] > 0) {
      total += [x integerValue];
    }
  }
  @try {
    NSDictionary *d = @{ @"total" : @(total) };
    NSLog(@"%@", d[@"total"]);
  } @catch (NSException *e) {
    total = -1;
  } @finally {
    NSLog(@"done");
  }
  return total;
}
