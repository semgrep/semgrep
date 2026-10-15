@implementation Handler

#if DEBUG
- (void)debugPath:(NSURL *)u {
  //ERROR:
  [self hit:u];
}
#else
- (void)releasePath:(NSURL *)u {
  //ERROR:
  [self hit:u];
}
#endif

- (void)hit:(NSURL *)u {
}

@end
