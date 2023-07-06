{ local t = 'a', ['h' + i + '_' + z]: if 'h' + (i - 1) + '_' + z in self then t + 1 else 0 + t for i in [1, 2, 3] for z in [2, 3, 4] if z != i }
