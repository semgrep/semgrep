// this should not match!
async function should_not_match(
  req: Sails.Request,
  res: Sails.Response,
): Promise<void> {
  return;
}

//ERROR: match
export async function should_match(
  req: Sails.Request,
  res: Sails.Response,
): Promise<void> {
  return;
}
