function MyComponent() {
  //ERROR: match
  return <div dangerouslySetInnerHTML={{__html: 'inner html'}} />;
}