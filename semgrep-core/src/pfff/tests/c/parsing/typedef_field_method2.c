
struct	Machdata {		/* Machine-dependent debugger support */
	uchar	bpinst[4];			/* break point instr. */
	short	bpsize;				/* size of break point instr. */

 	ushort	(*swab)(ushort x);		/* ushort to local byte order */
	ulong	(*swal)(ulong x);			/* ulong to local byte o */


   void  (*clockenable)(void);
   uvlong  (*fastclock)(uvlong*);

  uchar   (*get)(Pcidev *, uchar);

  Uart* (*pnp)(void);

  SDev* (*pnp)(void);

  Chan* (*open)(Chan*, int);

//  void  (*addmulti)(Ipifc *ifc, uchar *a, uchar *ia);

};
