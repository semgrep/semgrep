typedef struct MiiPhy {
  Mii*  mii;
  int oui;
  int phyno;

  int anar;
  int fc;
  int mscr;

  int link;
  int speed;
  int fd;
  int rfc;
  int tfc;
} x; // if forget x then it does not make any sense ...
