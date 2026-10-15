@interface Opener : NSObject
- (void)openURL:(NSURL *)u options:(NSDictionary *)o completionHandler:(void (^)(BOOL))h;
- (void)openURL:(NSURL *)u completionHandler:(void (^)(BOOL))h options:(NSDictionary *)o;
@end

void handle(Opener *app, NSURL *u) {
  //ERROR:
  [app openURL:u options:@{} completionHandler:nil];

  [app openURL:u completionHandler:nil options:@{}];
}
