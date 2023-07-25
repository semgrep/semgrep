async fn index(form: web::Form<MyForm>,
               response: HttpResponseBuilder,
               safe_response: SafeHttpResponseBuilder) -> impl Responder {
    let input = form.input.clone();
    match input.as_str() {
      "dangerous" => {
        let msg = format!("response, {}!", input);
        // ruleid: no-direct-response-write
        response.body(msg)
      }
      "safe" => {
        let msg = format!("safe_response, {}!", input);
        // ok: no-direct-response-write
        safe_response.body(msg)
      }
      _ =>
        // ok: no-direct-response-write
        response.body("default string".to_string()),
    }
}

async fn index2() -> impl Responder {
    let response: HttpResponseBuilder = get_response();
    // ruleid: no-direct-response-write
    response.body(msg)
}
