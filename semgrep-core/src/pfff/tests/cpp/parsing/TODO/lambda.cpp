mozilla::jni::Object::LocalRef SessionAccessibility::GetNodeInfo(int32_t aID) {
  java::GeckoBundle::GlobalRef ret = nullptr;
  RefPtr<SessionAccessibility> self(this);
  nsAppShell::SyncRunEvent([this, self, aID, &ret] {
    if (RootAccessibleWrap* rootAcc = GetRoot()) {
      AccessibleWrap* acc = rootAcc->FindAccessibleById(aID);
      if (acc) {
        ret = acc->ToBundle();
      } else {
        AALOG("oops, nothing for %d", aID);
      }
    }
  });

  return mozilla::jni::Object::Ref::From(ret);
}
