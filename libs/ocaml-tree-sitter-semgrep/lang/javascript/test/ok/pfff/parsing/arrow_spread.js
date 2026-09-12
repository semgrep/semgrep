const optionsOrFallback = (...args) => {
	let optionValues = [];
	optionValues.push(...args);
	return optionValues.find(optionValue => optionValue !== undefined);
};
