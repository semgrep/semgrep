void SessionAccessibility::SetAttached(bool aAttached,
                                       already_AddRefed<Runnable> aRunnable) {
  if (RefPtr<nsThread> uiThread = GetAndroidUiThread()) {
    uiThread->Dispatch(NS_NewRunnableFunction(
        "SessionAccessibility::Attach",
         [aAttached,
         sa = java::SessionAccessibility::NativeProvider::GlobalRef(
             mSessionAccessibility),
         runnable = RefPtr<Runnable>(aRunnable)] {
          sa->SetAttached(aAttached);
          if (runnable) {
            runnable->Run();
          }
        }));
  }
}
