
struct Pcidev {
  int tbdf;     /* type+bus+device+function */
  ushort  vid;      /* vendor ID */
  ushort  did;      /* device ID */
  uchar rid;      /* revision ID */

  struct {
    ulong bar;    /* base address */
    int size;
  } mem[6];

  uchar intl;     /* interrupt line */
  ushort  ccru;


  Pcidev* list;
  Pcidev* bridge;     /* down a bus */
  Pcidev* link;     /* next device on this bno */
};
