val random: ThreadLocalRandom = ThreadLocalRandom.current()
//ERROR: match
random.nextInt(alphabet.size)
//ERROR: match
(random).nextInt(alphabet.size)
