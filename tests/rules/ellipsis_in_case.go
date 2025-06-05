// ruleid: ellipsis-in-case
switch n {
	case 0: return
}

// ruleid: ellipsis-in-case
switch n {
	case 0, 1: return
}

// ruleid: ellipsis-in-case
switch n {
	case 0, 1, 2: return
}


// ok: ellipsis-in-case
switch n {
	case 1, 0: return
}


// ok: ellipsis-in-case
switch n {
	case 1: return
}