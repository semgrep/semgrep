async fn index(form: web::Form<MyForm>, response: HttpResponseBuilder) -> impl Responder {
    let input = form.input.clone();
    if input.is_empty() {
      // ok: no-direct-response-write
      response.body("default string".to_string())
    } else {
      // Construct an HTTP response with the tainted input
      let msg = format!("Hello, {}!", input);
      // ruleid: no-direct-response-write
      response.body(msg)
    }
}
