interface Enum {

  public enum AddShortcutResult {
    /**
     * A shortcut with the same intent already exist on one of the home screens and duplicated
     * shortcuts are not allowed
     */
    DUPLICATED,

    NO_ROOM,

    /**
     * a shortcut was added to a home screen
     */
    SUCCESS
  }

  protected static enum TransitionState {
     STARTING,
     PAUSED,
     RUNNING,
     None
   }
}
