struct Tree {
	File *root;
	void	(*destroy)(File *file);

/* below is implementation-specific; don't use */
	Lock genlock;
	ulong qidgen;
	ulong dirqidgen;
};
