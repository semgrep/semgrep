const expectedScore = (self, opponent) => 1 / (1 + 10 ** ((opponent - self) / 400));
