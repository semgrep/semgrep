def compose_call_path(self, node):
            if isinstance(node, ast.Attribute):
                yield from self.compose_call_path(node.value)
