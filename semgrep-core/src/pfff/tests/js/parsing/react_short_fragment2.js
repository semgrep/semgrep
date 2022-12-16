const Val = (x) => {
  return (
      <div>
        {toExpand && (
          <Button>
            {baz ? 'test' : <a href={parentTag}>test2</a>} // added > before test2
          </Button>
        )}
      </div>
  );
};

export default Val;
