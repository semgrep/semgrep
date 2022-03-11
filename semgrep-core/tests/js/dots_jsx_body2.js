export const Invites: React.FC = () => {
  return (
    <>
      //ERROR: match
      <div>Hi why does this match here but not locally.</div>
      //ERROR: match
      <div>
        Hello???
        //ERROR: match
        {Array.from(new Array(10)).map((_, i) => (<div>{i}</div>))}
      </div>
    </>
  );
};
