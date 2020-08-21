def foo():
    return 1

def test():
    self.assertEqual(foo(), 1)
    self.assertEqual(foo['bar'], 1)
    
    self.assertEqual(
        response.json()['response'],
        {
            'a': 1,
            'b': 2,
            'c': [1,2,3][2],
        }
    )