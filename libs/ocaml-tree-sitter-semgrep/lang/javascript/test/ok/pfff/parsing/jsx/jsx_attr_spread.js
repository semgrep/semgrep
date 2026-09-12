function SlimWidthRow({ children, ...restProps }) {
  return (
    <Row {...restProps}>
      <Col md={6} mdOffset={3} sm={12}>
        {children}
      </Col>
    </Row>
  );
}
