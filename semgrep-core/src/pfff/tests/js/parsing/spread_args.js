test('Returns the longest object from mixed input', () => {
   expect(longestItem(...['a', 'ab', 'abc'], 'abcd')).toEqual('abcd');
});
