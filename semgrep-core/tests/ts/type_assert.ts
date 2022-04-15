// https://github.com/returntocorp/semgrep/issues/4515
import v8 from 'v8';
//ERROR:
const snapshotStream1 = (v8 as any).getHeapSnapshot();
//OK:
const snapshotStream2 = v8.getHeapSnapshot();
