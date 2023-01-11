const Val = ({ x = []}) => {
  return (
    <>
      <div>
        {x.map((a, b) => {
          return 3;
        })}
      </div>
      <div>
        {toExpand && (
          <Button
            key="a"
            size="small"
            type="link"
            onClick={() => {
              foo(!bar);
            }}
          >
            {baz ? 'test' : <a href={parentTag}>test2</a>} // real error there before
          </Button>
        )}
      </div>
    </>
  );
};

export default Val;
