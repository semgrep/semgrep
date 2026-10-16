#import <Foundation/Foundation.h>

@interface Greeter : NSObject
@property (nonatomic, copy) NSString *name;
- (NSString *)greet:(NSString *)who politely:(BOOL)polite;
@end

@implementation Greeter

- (NSString *)greet:(NSString *)who politely:(BOOL)polite {
  NSString *prefix = polite ? @"Good day" : @"Hi";
  return [NSString stringWithFormat:@"%@, %@", prefix, who];
}

@end
