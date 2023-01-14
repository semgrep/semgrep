export default class {
    // this used to not parse correctly in tree-sitter-typescript
    someMethod() 
    {
	//ERROR: match
        return 0;
    }
}
