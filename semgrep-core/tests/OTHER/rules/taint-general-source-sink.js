const merge1 = (dst, src) => {
	for (let key in src) {
	    if (!src.hasOwnProperty(key)) continue;
	    if (isObject(dst[key])) {
		merge1(dst[key], src[key]);
	    } else {
		// tests filter-irrelevant-rule does not filter this out
		// ruleid: prototype-pollution-function
		dst[key] = src[key];
	    }
	}
    }
    
    function merge2(dst, src) {
	for (let key in src) {
	    if (!src.hasOwnProperty(key)) continue;
	    if (isObject(dst[key])) {
		merge2(dst[key], src[key]);
	    } else {
		// tests filter-irrelevant-rule does not filter this out
		// ruleid: prototype-pollution-function
		dst[key] = src[key];
	    }
	}
    }
    
    function okMerge1(dst, src) {
	for (let key in src) {
	    if (!src.hasOwnProperty(key)) continue;
	    if (dst.hasOwnProperty(key) && isObject(dst[key])) {
		okMerge1(dst[key], src[key]);
	    } else {
		// ok: prototype-pollution-function
		dst[key] = src[key];
	    }
	}
    }
    
    function okMerge2(dst, src) {
	for (let key in src) {
	    if (!src.hasOwnProperty(key)) continue;
	    if (key === "__proto__" || key === "constructor") continue;
	    if (isObject(dst[key])) {
		okMerge2(dst[key], src[key]);
	    } else {
		// ok: prototype-pollution-function
		dst[key] = src[key];
	    }
	}
    }
    