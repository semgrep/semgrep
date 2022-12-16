struct Tree {
	File *root;
 	void	(*destroy)(File file);
 	void	(*destroy)(File *file);
};
